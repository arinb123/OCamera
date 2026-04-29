open Raytracer3110.Camera
open Raytracer3110.Vec

type image = {
  width : int;
  height : int;
  pixels : (int * int * int) array;
}

type state = {
  mutable cx : float;
  mutable cy : float;
  mutable cz : float;
  mutable step : float;
  mutable image : image option;
  mutable status : string;
}

let color r g b = Raylib.Color.create r g b 255

let is_ws = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let tokenize_ppm text =
  let cleaned_lines = String.split_on_char '\n' text in
  let all = String.concat " " cleaned_lines in
  let len = String.length all in
  let buf = Buffer.create 256 in
  let tokens = ref [] in
  for i = 0 to len - 1 do
    let ch = all.[i] in
    if is_ws ch then
      if Buffer.length buf > 0 then (
        tokens := Buffer.contents buf :: !tokens;
        Buffer.clear buf)
      else ();
    if not (is_ws ch) then Buffer.add_char buf ch
  done;
  if Buffer.length buf > 0 then tokens := Buffer.contents buf :: !tokens;
  List.rev !tokens

let parse_int token = try Some (int_of_string token) with Failure _ -> None
let clamp v = if v < 0 then 0 else if v > 255 then 255 else v

let parse_ppm text =
  match tokenize_ppm text with
  | magic :: w :: h :: maxv :: rest when magic = "P3" -> (
      match (parse_int w, parse_int h, parse_int maxv) with
      | Some width, Some height, Some max_value
        when width > 0 && height > 0 && max_value > 0 ->
          let expected = width * height * 3 in
          let values =
            rest |> List.filter_map parse_int |> List.to_seq |> Array.of_seq
          in
          if Array.length values < expected then
            Error
              (Printf.sprintf
                 "Unexpected end of PPM: expected %d color values, got %d"
                 expected (Array.length values))
          else
            let pixels = Array.make (width * height) (0, 0, 0) in
            for i = 0 to (width * height) - 1 do
              let r = values.(i * 3) in
              let g = values.((i * 3) + 1) in
              let b = values.((i * 3) + 2) in
              let rr = clamp (r * 255 / max_value) in
              let gg = clamp (g * 255 / max_value) in
              let bb = clamp (b * 255 / max_value) in
              pixels.(i) <- (rr, gg, bb)
            done;
            Ok { width; height; pixels }
      | _ -> Error "Invalid PPM header values")
  | _ -> Error "Unsupported PPM format (expected P3)"

let read_file_contents filename =
  let ic = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let render_scene scene_file cx cy cz =
  let output_file = Filename.temp_file "raytracer-ui" ".ppm" in
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists output_file then Sys.remove output_file)
    (fun () ->
      render_with_camera (make (cx, cy, cz)) scene_file output_file;
      let ppm = read_file_contents output_file in
      parse_ppm ppm)

let draw_image image canvas_x canvas_y canvas_w canvas_h =
  let sx = float_of_int canvas_w /. float_of_int image.width in
  let sy = float_of_int canvas_h /. float_of_int image.height in
  let scale = min sx sy in
  let draw_w = int_of_float (float_of_int image.width *. scale) in
  let draw_h = int_of_float (float_of_int image.height *. scale) in
  let ox = canvas_x + ((canvas_w - draw_w) / 2) in
  let oy = canvas_y + ((canvas_h - draw_h) / 2) in
  for yy = 0 to draw_h - 1 do
    for xx = 0 to draw_w - 1 do
      let src_x =
        min (image.width - 1) (int_of_float (float_of_int xx /. scale))
      in
      let src_y =
        min (image.height - 1) (int_of_float (float_of_int yy /. scale))
      in
      let i = (src_y * image.width) + src_x in
      let r, g, b = image.pixels.(i) in
      Raylib.draw_rectangle (ox + xx) (oy + yy) 1 1 (color r g b)
    done
  done

let rerender state scene_file =
  match render_scene scene_file state.cx state.cy state.cz with
  | Ok img ->
      state.image <- Some img;
      state.status <- Printf.sprintf "Rendered %dx%d" img.width img.height
  | Error msg -> state.status <- msg

let run scene_file =
  let screen_w, screen_h, canvas_x, canvas_y, canvas_w, canvas_h =
    (1200, 760, 20, 110, 820, 620)
  in
  Raylib.init_window screen_w screen_h "Raytracer UI";
  Raylib.set_target_fps 15;
  let state =
    {
      cx = 0.0;
      cy = 0.0;
      cz = 0.0;
      step = 0.1;
      image = None;
      status = "Rendering...";
    }
  in
  rerender state scene_file;
  while not (Raylib.window_should_close ()) do
    let moved = ref false in
    if Raylib.is_key_pressed Raylib.Key.Left then (
      state.cx <- state.cx -. state.step;
      moved := true);
    if Raylib.is_key_pressed Raylib.Key.Right then (
      state.cx <- state.cx +. state.step;
      moved := true);
    if Raylib.is_key_pressed Raylib.Key.Up then (
      state.cy <- state.cy +. state.step;
      moved := true);
    if Raylib.is_key_pressed Raylib.Key.Down then (
      state.cy <- state.cy -. state.step;
      moved := true);
    if Raylib.is_key_pressed Raylib.Key.Q then (
      state.cz <- state.cz +. state.step;
      moved := true);
    if Raylib.is_key_pressed Raylib.Key.E then (
      state.cz <- state.cz -. state.step;
      moved := true);
    if Raylib.is_key_pressed Raylib.Key.R then (
      state.cx <- 0.0;
      state.cy <- 0.0;
      state.cz <- 0.0;
      moved := true);
    if !moved then rerender state scene_file;

    Raylib.begin_drawing ();
    Raylib.clear_background (color 7 12 20);
    Raylib.draw_text "Raytracer UI" 20 20 34 (color 235 241 255);
    Raylib.draw_text
      (Printf.sprintf "Camera: (%.2f, %.2f, %.2f)" state.cx state.cy state.cz)
      20 66 20 (color 173 196 224);
    Raylib.draw_text
      (Printf.sprintf "Step: %.2f   Controls: Arrow Keys, Q/E depth, R reset"
         state.step)
      20 88 16 (color 143 167 194);

    Raylib.draw_rectangle_lines canvas_x canvas_y canvas_w canvas_h
      (color 92 173 255);
    (match state.image with
    | Some img -> draw_image img canvas_x canvas_y canvas_w canvas_h
    | None ->
        Raylib.draw_text "No image" (canvas_x + 12) (canvas_y + 12) 18
          (color 255 120 120));

    Raylib.draw_text state.status 860 130 18 (color 212 224 243);
    Raylib.end_drawing ()
  done;
  Raylib.close_window ()
