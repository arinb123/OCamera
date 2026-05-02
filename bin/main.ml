module Camera = Raytracer3110.Camera
module Ui = Raytracer3110.Ui

let usage =
  "usage:\n\
  \  dune exec bin/main.exe <input_scene.yaml> <output_image.ppm>\n\
  \  dune exec bin/main.exe -- -ui <input_scene.yaml>"

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
  match args with
  | [ _prog; "-ui"; scene ] ->
      Random.init 42;
      let scene_file = choose_scene scene in
      let cam, _ = Camera.parse_yaml_scene scene_file in
      Ui.run scene_file cam
  | [ _prog; scene; output ] ->
      Random.init 42;
      let scene_file = choose_scene scene in
      Camera.render_from_yaml ~use_threading:true scene_file output
  | _ ->
      prerr_endline usage;
      exit 1
