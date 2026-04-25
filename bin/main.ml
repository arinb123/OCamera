open Raytracer3110.Camera

let () =
  if Array.length Sys.argv <> 2 then failwith "needs exactly one file name"
  else
    let file_name = Sys.argv.(1) in
    render file_name
