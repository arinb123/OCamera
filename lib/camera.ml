open Vec
open Ray
open Ppm
open Object

type camera = {
  center : vector;
  pixel00_loc : vector;
  pixel_delta_u : vector;
  pixel_delta_v : vector;
  image_width : int;
  image_height : int;
  samples_per_pixel : int;
  max_depth : int;
  yaw : float;
  pitch : float;
}

let make ?(samples_per_pixel = 10) ?(max_depth = 10) ?(viewport_height = 2.0)
    ?(yaw = 0.0) ?(pitch = 0.0) ~image_width ~aspect_ratio ~focal_length ~center
    () =
  let image_height =
    max 1 (int_of_float (float_of_int image_width /. aspect_ratio))
  in
  let viewport_width =
    viewport_height *. (float_of_int image_width /. float_of_int image_height)
  in
  (* Clamp pitch to avoid gimbal lock at ±90° *)
  let pitch = max (-1.5) (min 1.5 pitch) in
  let forward =
    Vec.normalize
      (Vec.make (sin yaw *. cos pitch, sin pitch, -.(cos yaw *. cos pitch)))
  in
  let world_up = Vec.make (0., 1., 0.) in
  let right = Vec.normalize (Vec.cross forward world_up) in
  let up = Vec.normalize (Vec.cross right forward) in
  let viewport_u = viewport_width *^ right in
  let viewport_v = viewport_height *^ (Vec.make (0., 0., 0.) -^ up) in
  let pixel_delta_u = 1. /. float_of_int image_width *^ viewport_u in
  let pixel_delta_v = 1. /. float_of_int image_height *^ viewport_v in
  let viewport_upper_left =
    center +^ (focal_length *^ forward) -^ (0.5 *^ viewport_u)
    -^ (0.5 *^ viewport_v)
  in
  let pixel00_loc =
    viewport_upper_left +^ (0.5 *^ (pixel_delta_u +^ pixel_delta_v))
  in
  {
    center;
    pixel00_loc;
    pixel_delta_u;
    pixel_delta_v;
    image_width;
    image_height;
    samples_per_pixel;
    max_depth;
    yaw;
    pitch;
  }

(* Parse a vertex line: "v x y z" *)
let parse_vertex (line : string) =
  match String.split_on_char ' ' line |> List.filter (( <> ) "") with
  | [ "v"; x; y; z ] -> (
      try Vec.make (float_of_string x, float_of_string y, float_of_string z)
      with Failure _ -> failwith ("Invalid vertex format: " ^ line))
  | _ -> failwith ("Invalid vertex line: " ^ line)

(* Parse a face line : "f x y z"*)
let parse_face (line : string) =
  match String.split_on_char ' ' line |> List.filter (( <> ) "") with
  | [ "f"; x; y; z ] -> (
      try (int_of_string x - 1, int_of_string y - 1, int_of_string z - 1)
      with Failure _ -> failwith ("Invalid face format: " ^ line))
  | _ -> failwith ("Invalid face line: " ^ line)

let process_obj (obj_filename : string) mat =
  let vertices = Dynarray.create () in
  let faces = Dynarray.create () in
  let chan = open_in obj_filename in
  let normals = Dynarray.create () in
  let min_vec = ref (Vec.make (infinity, infinity, infinity)) in
  let max_vec = ref (Vec.make (neg_infinity, neg_infinity, neg_infinity)) in
  try
    while true do
      let line = input_line chan in
      if String.length line >= 2 then
        match String.sub line 0 2 with
        | "v " ->
            let vertex = parse_vertex line in
            Dynarray.add_last vertices vertex;
            min_vec := Vec.vec_min !min_vec vertex;
            max_vec := Vec.vec_max !max_vec vertex
        | "f " -> (
            let face = parse_face line in
            Dynarray.add_last faces face;
            match face with
            | a, b, c ->
                let v0 = Dynarray.get vertices a in
                let v1 = Dynarray.get vertices b in
                let v2 = Dynarray.get vertices c in
                let v0v1 = v1 -^ v0 in
                let v0v2 = v2 -^ v0 in
                let n = cross v0v1 v0v2 in
                Dynarray.add_last normals n)
        | _ -> ()
    done
  with End_of_file ->
    close_in chan;
    (* print_endline (string_of_int (List.length vertices)); *)
    print_endline (string_of_float (Vec.vec_fst !min_vec));
    print_endline (string_of_float (Vec.vec_fst !max_vec));
    TriangularMesh
      ( Dynarray.to_array vertices,
        Dynarray.to_list faces,
        Dynarray.to_array normals,
        { min = !min_vec; max = !max_vec },
        mat )

let find_field key fields =
  match List.assoc_opt key fields with
  | Some v -> v
  | None -> failwith ("Missing required field: " ^ key)

let float_of_yaml = function
  | `Float f -> f
  | `String s -> (
      try float_of_string s with _ -> failwith ("Expected float, got: " ^ s))
  | _ -> failwith "Expected a float scalar"

let int_of_yaml v = int_of_float (float_of_yaml v)

let vec_of_yaml = function
  | `A [ x; y; z ] ->
      Vec.make (float_of_yaml x, float_of_yaml y, float_of_yaml z)
  | _ -> failwith "Expected a list of 3 floats for vector"

let parse_material_yaml fields =
  match List.assoc_opt "material" fields with
  | None -> Object.Lambertian (Vec.make (0.8, 0.8, 0.8))
  | Some (`O mat_fields) ->
      let mat_type =
        match find_field "type" mat_fields with
        | `String s -> s
        | _ -> failwith "material type must be a string"
      in
      let albedo = find_field "albedo" mat_fields |> vec_of_yaml in
      if mat_type = "metal" then
        let fuzz = find_field "fuzz" mat_fields |> float_of_yaml in
        Object.Metal (albedo, fuzz)
      else Object.Lambertian albedo
  | _ -> failwith "material must be a mapping"

let parse_yaml_scene (yaml_filename : string) : camera * Object.hittable =
  let content =
    let chan = open_in yaml_filename in
    let n = in_channel_length chan in
    let s = Bytes.create n in
    really_input chan s 0 n;
    close_in chan;
    Bytes.to_string s
  in
  let yaml =
    match Yaml.of_string content with
    | Ok v -> v
    | Error (`Msg msg) ->
        failwith ("Invalid YAML in '" ^ yaml_filename ^ "': " ^ msg)
  in
  match yaml with
  | `O top_fields ->
      let cam =
        match find_field "camera" top_fields with
        | `O cam_fields ->
            let image_width =
              find_field "image_width" cam_fields |> int_of_yaml
            in
            let aspect_ratio =
              find_field "aspect_ratio" cam_fields |> float_of_yaml
            in
            let focal_length =
              find_field "focal_length" cam_fields |> float_of_yaml
            in
            let center = find_field "center" cam_fields |> vec_of_yaml in
            let samples_per_pixel =
              match List.assoc_opt "samples_per_pixel" cam_fields with
              | Some v -> int_of_yaml v
              | None -> 10
            in
            let max_depth =
              match List.assoc_opt "max_depth" cam_fields with
              | Some v -> int_of_yaml v
              | None -> 10
            in
            let viewport_height =
              match List.assoc_opt "viewport_height" cam_fields with
              | Some v -> float_of_yaml v
              | None -> 2.0
            in
            make ~samples_per_pixel ~max_depth ~viewport_height ~image_width
              ~aspect_ratio ~focal_length ~center ()
        | _ -> failwith "camera must be a mapping"
      in
      let world =
        match find_field "objects" top_fields with
        | `A obj_list ->
            Object.HittableList
              (List.map
                 (fun obj ->
                   match obj with
                   | `O obj_fields -> (
                       let mat = parse_material_yaml obj_fields in
                       match find_field "type" obj_fields with
                       | `String "sphere" ->
                           let center =
                             find_field "center" obj_fields |> vec_of_yaml
                           in
                           let radius =
                             find_field "radius" obj_fields |> float_of_yaml
                           in
                           Object.Sphere (center, radius, mat)
                       | `String "triangle" ->
                           let v0 = find_field "v0" obj_fields |> vec_of_yaml in
                           let v1 = find_field "v1" obj_fields |> vec_of_yaml in
                           let v2 = find_field "v2" obj_fields |> vec_of_yaml in
                           Object.Triangle (v0, v1, v2, mat)
                       | `String "triangular_mesh" ->
                           let file_path =
                             match find_field "file_path" obj_fields with
                             | `String s -> s
                             | _ -> failwith "file_path must be a string"
                           in
                           process_obj file_path mat
                       | _ -> failwith "Unsupported object type in YAML scene")
                   | _ -> failwith "Each object entry must be a mapping")
                 obj_list)
        | _ -> failwith "objects must be a list"
      in
      (cam, world)
  | _ -> failwith "YAML scene must be a top-level mapping"

(* render_pixel is a helper function for parallelism *)
let render_pixel cam world rng i j =
  let pixel_center =
    cam.pixel00_loc
    +^ (float_of_int j *^ cam.pixel_delta_u)
    +^ (float_of_int i *^ cam.pixel_delta_v)
  in
  let rec loop n acc =
    if n = 0 then acc
    else
      let ox = Random.State.float rng 1.0 -. 0.5 in
      let oy = Random.State.float rng 1.0 -. 0.5 in
      let sample =
        pixel_center +^ (ox *^ cam.pixel_delta_u) +^ (oy *^ cam.pixel_delta_v)
      in
      let ray = Ray.make cam.center (sample -^ cam.center) in
      loop (n - 1) (acc +^ ray_color ray cam.max_depth world rng)
  in
  loop cam.samples_per_pixel (Vec.make (0., 0., 0.))
  /^ float_of_int cam.samples_per_pixel

let render_world ?(use_threading = true) (cam : camera)
    (world : Object.hittable) (output_filename : string) : unit =
  let start_time = Unix.gettimeofday () in

  (* pre-allocate flat pixel buffer *)
  let n_pixels = cam.image_width * cam.image_height in
  let buf = Array.make n_pixels (Vec.make (0., 0., 0.)) in

  let n_domains =
    if use_threading then Domain.recommended_domain_count () else 1
  in
  print_endline
    ("Using " ^ string_of_int n_domains ^ " domain(s) for rendering.");
  let rows_per_domain = cam.image_height / n_domains in
  let rows_completed = Atomic.make 0 in

  let spawn_domain d =
    Domain.spawn (fun () ->
        (* each domain gets its own Random state seeded distinctly *)
        let rng = Random.State.make [| d * 1337 |] in
        let y0 = d * rows_per_domain in
        let y1 =
          if d = n_domains - 1 then cam.image_height else y0 + rows_per_domain
        in
        (* there's no risk of race condition because each domain works on
           distinct rows *)
        for i = y0 to y1 - 1 do
          for j = 0 to cam.image_width - 1 do
            buf.((i * cam.image_width) + j) <- render_pixel cam world rng i j
          done;
          (* update progress *)
          let completed = Atomic.fetch_and_add rows_completed 1 + 1 in
          if completed mod 10 = 0 then
            Printf.printf "Progress: %d / %d rows completed\n%!" completed
              cam.image_height
        done)
  in
  let domains = Array.init n_domains spawn_domain in
  Array.iter Domain.join domains;

  (* write buffer to file after all domains finish *)
  let b = create_file output_filename cam.image_width cam.image_height in
  Array.iter (write_file b) buf;
  close_file b;

  let end_time = Unix.gettimeofday () in
  let elapsed = end_time -. start_time in
  Printf.printf "Rendering complete in %.2f seconds\n%!" elapsed

let render_from_yaml ?(use_threading = true) (yaml_filename : string)
    (output_filename : string) : unit =
  let cam, world = parse_yaml_scene yaml_filename in
  render_world ~use_threading cam world output_filename

let render_yaml_with_cam ?(use_threading = true) (cam : camera)
    (yaml_filename : string) (output_filename : string) : unit =
  let _cam_from_yaml, world = parse_yaml_scene yaml_filename in
  render_world ~use_threading cam world output_filename
