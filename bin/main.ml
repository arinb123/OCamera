open Raytracer3110.Vec
open Raytracer3110.Ray
open Raytracer3110.Camera
open Raytracer3110.Ppm

let () =
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
      let r = make camera_center ray_direction in
      let pixel_color = ray_color r in
      write_file b pixel_color
    done
  done
