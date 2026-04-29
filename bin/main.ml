open Raytracer3110.Camera

let usage =
  "usage:\n\
  \  dune exec bin/main.exe <input_scene.json> <output_image.ppm>\n\
  \  dune exec bin/main.exe -- -ui <input_scene.json>"

let candidate_scene_files path =
  let via_data = Filename.concat "data" (Filename.basename path) in
  if via_data = path || not (Sys.file_exists via_data) then [path]
  else [path; via_data]

let rec try_scenes candidates f =
  match candidates with
  | [] -> failwith "No valid scene file found"
  | [s] -> f s
  | s :: rest ->
      (try f s
       with Failure msg when
         let prefix = "Invalid JSON in scene file" in
         String.length msg >= String.length prefix &&
         String.sub msg 0 (String.length prefix) = prefix ->
         try_scenes rest f)

let () =
  match Array.to_list Sys.argv with
  | [_; "-ui"; scene] ->
      try_scenes (candidate_scene_files scene) Native_ui.run
  | [_; scene; output] ->
      try_scenes (candidate_scene_files scene) (fun s -> render s output)
  | _ -> failwith usage