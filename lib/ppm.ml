open Vec

type ppm_buffer = out_channel

let create_file filename width height =
  try
    let oc = open_out filename in
    Printf.fprintf oc "P3\n %d %d\n255\n" width height;
    oc
  with Sys_error msg -> failwith ("Failed to create file: " ^ msg)

let write_file buff color =
  let color = vec_to_tup color in
  match color with
  | r, g, b ->
      Printf.fprintf buff "%d %d %d \n"
        (int_of_float (255.99 *. r))
        (int_of_float (255.99 *. g))
        (int_of_float (255.99 *. b))

let close_file buff = close_out buff
