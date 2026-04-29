open Vec
open Ray
open Ppm
open Object

(* width / height *)
let aspect_ratio = 16.0 /. 9.0
let image_width = 400
let image_height = int_of_float (float_of_int image_width /. aspect_ratio)
let focal_length = 1.
let camera_center = Vec.make (0., 0., 0.)
let viewport_height = 2.
let max_depth = 10

let viewport_width =
  viewport_height *. (float_of_int image_width /. float_of_int image_height)

let viewport_u, viewport_v =
  (Vec.make (viewport_width, 0., 0.), Vec.make (0., viewport_height *. -1., 0.))

let pixel_delta_u, pixel_delta_v =
  ( 1. /. float_of_int image_width *^ viewport_u,
    1. /. float_of_int image_height *^ viewport_v )

let viewport_upper_left =
  camera_center
  -^ Vec.make (0.0, 0.0, focal_length)
  -^ (0.5 *^ viewport_u) -^ (0.5 *^ viewport_v)

let pixel00_loc =
  viewport_upper_left +^ (0.5 *^ (pixel_delta_u +^ pixel_delta_v))

let samples_per_pixel = 10

let parse_shapes json_filename =
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
let parse_vertex line =
  match String.split_on_char ' ' line |> List.filter (( <> ) "") with
  | [ "v"; x; y; z ] -> (
      try Vec.make (float_of_string x, float_of_string y, float_of_string z)
      with Failure _ -> failwith ("Invalid vertex format: " ^ line))
  | _ -> failwith ("Invalid vertex line: " ^ line)

let parse_face line =
  match String.split_on_char ' ' line |> List.filter (( <> ) "") with
  | [ "f"; x; y; z ] -> (
      try (int_of_string x - 1, int_of_string y - 1, int_of_string z - 1)
      with Failure _ -> failwith ("Invalid face format: " ^ line))
  | _ -> failwith ("Invalid vertex line: " ^ line)

let process_obj (obj_filename : string) =
  let vertices = ref [] in
  let faces = ref [] in
  let chan = open_in obj_filename in
  try
    while true do
      let line = input_line chan in
      if String.length line >= 2 then
        match String.sub line 0 1 with
        | "v" -> vertices := parse_vertex line :: !vertices
        | "f" -> faces := parse_face line :: !faces
        | _ -> ()
    done
  with End_of_file ->
    close_in chan;
    let vertices = List.rev !vertices in
    let faces = List.rev !faces in
    (* print_endline (string_of_int (List.length vertices)); *)
    TriangularMesh (vertices, faces)

let render (json_filename : string) (output_filename : string) : unit =
  let object_list = parse_shapes json_filename in
  let create_vector_yojson json =
    match Yojson.Basic.Util.convert_each Yojson.Basic.Util.to_float json with
    | [ x; y; z ] -> Vec.make (x, y, z)
    | _ -> failwith "Expected a list of 3 floats for vector fields"
    (* parse the object list into a hittable list *)
  in
  let hittable_list =
    HittableList
      (List.map
         (fun obj ->
           (* match based on the type specified in the json *)
           match Yojson.Basic.Util.(obj |> member "type" |> to_string) with
           | "sphere" ->
               let center =
                 Yojson.Basic.Util.(
                   obj |> member "center" |> create_vector_yojson)
               in
               let radius =
                 Yojson.Basic.Util.(obj |> member "radius" |> to_float)
               in
               Sphere (center, radius)
           | "triangle" ->
               let v0 =
                 Yojson.Basic.Util.(obj |> member "v0" |> create_vector_yojson)
               in
               let v1 =
                 Yojson.Basic.Util.(obj |> member "v1" |> create_vector_yojson)
               in
               let v2 =
                 Yojson.Basic.Util.(obj |> member "v2" |> create_vector_yojson)
               in
               Triangle (v0, v1, v2)
           | "triangular_mesh" ->
               let file_path =
                 Yojson.Basic.Util.(obj |> member "file_path" |> to_string)
               in
               let mesh = process_obj file_path in
               mesh
           | _ -> failwith "Unknown object type in scene JSON")
         object_list)
  in
  let b = create_file output_filename image_width image_height in
  for i = 1 to image_height do
    Printf.printf "\rScanlines remaining: %d " (image_height - i);
    for j = 1 to image_width do
      let pixel_center =
        pixel00_loc
        +^ (float_of_int (j - 1) *^ pixel_delta_u)
        +^ (float_of_int (i - 1) *^ pixel_delta_v)
      in
      let rec sum_nearest_pixels n acc =
        if n = 0 then acc
        else
          let offset_x = Random.float 1.0 -. 0.5 in
          let offset_y = Random.float 1.0 -. 0.5 in
          let pixel_sample =
            pixel_center
            +^ (offset_x *^ pixel_delta_u)
            +^ (offset_y *^ pixel_delta_v)
          in
          let new_ray =
            Ray.make camera_center (pixel_sample -^ camera_center)
          in
          let color = ray_color new_ray max_depth hittable_list in
          sum_nearest_pixels (n - 1) (acc +^ color)
      in

      write_file b
        (sum_nearest_pixels samples_per_pixel (Vec.make (0., 0., 0.))
        /^ float_of_int samples_per_pixel)
    done
  done;
  close_file b
