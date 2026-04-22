open Raytracer3110.Vec
open Raytracer3110.Ray
open Raytracer3110.Camera
open Raytracer3110.Ppm
open Raytracer3110.Object

let () =
  let json = Yojson.Basic.from_file "./data/shapes.json" in
  let object_list = Yojson.Basic.Util.to_list json in
  (* parse the object list into a hittable list *)
  let hittable_list =
    HittableList
      (List.map
         (fun obj ->
           let center =
             match
               Yojson.Basic.Util.(
                 obj |> member "center" |> convert_each to_float)
             with
             | [ x; y; z ] -> Raytracer3110.Vec.make (x, y, z)
             | _ -> failwith "Expected center to be a list of 3 floats"
           in
           let radius =
             Yojson.Basic.Util.(obj |> member "radius" |> to_float)
           in
           Sphere (center, radius))
         object_list)
  in
  let b = create_file "data/test.ppm" image_width image_height in
  for i = 1 to image_height do
    Printf.printf "\rScanlines remaining: %d " (image_height - i);
    for j = 1 to image_width do
      let pixel_center =
        pixel00_loc
        ^+ (float_of_int j ^* pixel_delta_u)
        ^+ float_of_int i ^* pixel_delta_v
      in
      let ray_direction = pixel_center ^- camera_center in
      let r = Raytracer3110.Ray.make camera_center ray_direction in
      let pixel_color = Raytracer3110.Object.ray_color r hittable_list in
      write_file b pixel_color
    done
  done
