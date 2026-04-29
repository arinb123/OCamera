open Vec
open Ray
open Ppm
open Object

(* width / height *)
let aspect_ratio = 16.0 /. 9.0
let image_width = 400
let image_height = int_of_float (float_of_int image_width /. aspect_ratio)
let focal_length = 1.
let viewport_height = 2.

let viewport_width =
  viewport_height *. (float_of_int image_width /. float_of_int image_height)

let camera_center = Vec.make (0., 0., 0.)
let viewport_u = Vec.make (viewport_width, 0., 0.)
let viewport_v = Vec.make (0., viewport_height *. -1., 0.)
let pixel_delta_u = 1. /. float_of_int image_width *^ viewport_u
let pixel_delta_v = 1. /. float_of_int image_height *^ viewport_v

let viewport_upper_left camera_center =
  camera_center
  -^ Vec.make (0.0, 0.0, focal_length)
  -^ (0.5 *^ viewport_u) -^ (0.5 *^ viewport_v)

let pixel00_loc camera_center =
  viewport_upper_left camera_center +^ (0.5 *^ (pixel_delta_u +^ pixel_delta_v))

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

let render_with_camera (camera_center : vector) (json_filename : string)
    (output_filename : string) : unit =
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
           | _ -> failwith "Unknown object type in scene JSON")
         object_list)
  in
  let pixel00_loc = pixel00_loc camera_center in
  let b = create_file output_filename image_width image_height in
  for i = 0 to image_height - 1 do
    Printf.printf "\rScanlines remaining: %d " (image_height - i);
    for j = 0 to image_width - 1 do
      let pixel_center =
        pixel00_loc
        +^ (float_of_int j *^ pixel_delta_u)
        +^ (float_of_int i *^ pixel_delta_v)
      in
      let ray_direction = pixel_center -^ camera_center in
      let r = Ray.make camera_center ray_direction in
      let pixel_color = Object.ray_color r hittable_list in
      write_file b pixel_color
    done
  done;
  close_file b

let render (json_filename : string) (output_filename : string) : unit =
  render_with_camera camera_center json_filename output_filename
