module Camera = Raytracer3110.Camera
module Ui = Raytracer3110.Ui
module Vec = Raytracer3110.Vec

let usage =
  "usage:\n\
  \  dune exec bin/main.exe <input_scene.json> <output_image.ppm>\n\
  \  dune exec bin/main.exe -- -ui <input_scene.json>"

let candidate_scene_files path =
  let via_data = Filename.concat "data" (Filename.basename path) in
  if via_data = path || not (Sys.file_exists via_data) then [ path ]
  else [ path; via_data ]

let choose_scene path =
  let rec find = function
    | [] -> path
    | p :: ps -> if Sys.file_exists p then p else find ps
  in
  find (candidate_scene_files path)

let () =
  let args = Array.to_list Sys.argv in
  let cam =
    Camera.make ~image_width:400 ~aspect_ratio:(16. /. 9.) ~focal_length:1.
      ~center:(Vec.make (0., 0., 5.))
      ()
  in
  match args with
  | [ _prog; "-ui"; scene ] ->
      Random.init 42;
      let scene_file = choose_scene scene in
      Ui.run scene_file cam
  | [ _prog; scene; output ] ->
      Random.init 42;
      let scene_file = choose_scene scene in
      Camera.render ~use_threading:true cam scene_file output
  | _ ->
      prerr_endline usage;
      exit 1
