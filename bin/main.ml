open Raytracer3110.Camera

let () =
  if Array.length Sys.argv <> 3 then
    failwith
      "usage: dune exec bin/main.exe <input_scene.json> <output_image.ppm>"
  else Random.init 42;
  let scene_file = Sys.argv.(1) in
  let output_file = Sys.argv.(2) in
  render ~use_threading:true scene_file output_file
