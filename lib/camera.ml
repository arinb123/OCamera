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
}

let make ?(samples_per_pixel = 10) ?(max_depth = 10) ?(viewport_height = 2.0)
    ~image_width ~aspect_ratio ~focal_length ~center () =
  let image_height =
    max 1 (int_of_float (float_of_int image_width /. aspect_ratio))
  in
  let viewport_width =
    viewport_height *. (float_of_int image_width /. float_of_int image_height)
  in
  let viewport_u = Vec.make (viewport_width, 0., 0.) in
  let viewport_v = Vec.make (0., -.viewport_height, 0.) in
  let pixel_delta_u = 1. /. float_of_int image_width *^ viewport_u in
  let pixel_delta_v = 1. /. float_of_int image_height *^ viewport_v in
  let viewport_upper_left =
    center
    -^ Vec.make (0., 0., focal_length)
    -^ (0.5 *^ viewport_u) -^ (0.5 *^ viewport_v)
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
  }

let parse_shapes (json_filename : string) =
  try
    let json = Yojson.Basic.from_file json_filename in
    match json with
    | `List object_list -> object_list
    | _ -> failwith "Scene JSON must be a list of objects."
  with
  | Yojson.Json_error msg ->
      failwith ("Invalid JSON in scene file '" ^ json_filename ^ "': " ^ msg)
  | Sys_error msg ->
      failwith ("Could not read scene file '" ^ json_filename ^ "': " ^ msg)

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

let process_obj (obj_filename : string) =
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
        match String.sub line 0 1 with
        | "v" ->
            let vertex = parse_vertex line in
            Dynarray.add_last vertices vertex;
            min_vec := Vec.vec_min !min_vec vertex;
            max_vec := Vec.vec_max !max_vec vertex
        | "f" -> (
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
        { min = !min_vec; max = !max_vec } )

let parse_scene json_filename =
  let object_list = parse_shapes json_filename in
  let vec_of_json json =
    match Yojson.Basic.Util.convert_each Yojson.Basic.Util.to_float json with
    | [ x; y; z ] -> Vec.make (x, y, z)
    | _ -> failwith "Expected a list of 3 floats for vector fields"
  in
  HittableList
    (List.map
       (fun obj ->
         match Yojson.Basic.Util.(obj |> member "type" |> to_string) with
         | "sphere" ->
             let center =
               Yojson.Basic.Util.(obj |> member "center" |> vec_of_json)
             in
             let radius =
               Yojson.Basic.Util.(obj |> member "radius" |> to_float)
             in
             Sphere (center, radius)
         | "triangle" ->
             let v0 = Yojson.Basic.Util.(obj |> member "v0" |> vec_of_json) in
             let v1 = Yojson.Basic.Util.(obj |> member "v1" |> vec_of_json) in
             let v2 = Yojson.Basic.Util.(obj |> member "v2" |> vec_of_json) in
             Triangle (v0, v1, v2)
         | "triangular_mesh" ->
             let file_path =
               Yojson.Basic.Util.(obj |> member "file_path" |> to_string)
             in
             process_obj file_path
         | _ -> failwith "Unknown object type in scene JSON")
       object_list)

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

let render ?(use_threading = true) (json_filename : string)
    (output_filename : string) : unit =
  let start_time = Unix.gettimeofday () in
  let cam =
    make ~image_width:400 ~aspect_ratio:(16. /. 9.) ~focal_length:1.
      ~center:(Vec.make (0., 0., 5.))
      ()
  in
  let world = parse_scene json_filename in

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
