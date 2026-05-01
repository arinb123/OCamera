open Camera
open Vec

type image = {
  width : int;
  height : int;
  pixels : (int * int * int) array;
}

type input_field = {
  mutable text : string;
  label : string;
  x : int;
  y : int;
  w : int;
  h : int;
}

type state = {
  mutable step : float;
  mutable image : image option;
  mutable cam : Camera.camera;
  mutable status : string;
  fields : input_field array;
  mutable focused_field : int;
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

let render_scene scene_file cam =
  let output_file = Filename.temp_file "raytracer-ui" ".ppm" in
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists output_file then Sys.remove output_file)
    (fun () ->
      Camera.render_yaml_with_cam ~use_threading:true cam scene_file output_file;
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
  match render_scene scene_file state.cam with
  | Ok img ->
      state.image <- Some img;
      state.status <- Printf.sprintf "Rendered %dx%d" img.width img.height
  | Error msg -> state.status <- msg

let panel_x = 860

let sync_fields_from_cam state =
  let cx, cy, cz = Vec.vec_to_tup state.cam.center in
  state.fields.(0).text <- Printf.sprintf "%.3f" cx;
  state.fields.(1).text <- Printf.sprintf "%.3f" cy;
  state.fields.(2).text <- Printf.sprintf "%.3f" cz;
  state.fields.(3).text <- Printf.sprintf "%.3f" state.cam.yaw;
  state.fields.(4).text <- Printf.sprintf "%.3f" state.cam.pitch

let apply_fields state scene_file =
  let parse s = try float_of_string s with Failure _ -> 0.0 in
  let cx = parse state.fields.(0).text in
  let cy = parse state.fields.(1).text in
  let cz = parse state.fields.(2).text in
  let yaw = parse state.fields.(3).text in
  let pitch = parse state.fields.(4).text in
  let w = state.cam.image_width in
  let h = state.cam.image_height in
  let aspect = float_of_int w /. float_of_int h in
  state.cam <-
    Camera.make ~image_width:w ~aspect_ratio:aspect ~focal_length:1.
      ~center:(Vec.make (cx, cy, cz))
      ~yaw ~pitch ();
  rerender state scene_file;
  sync_fields_from_cam state

let run scene_file cam =
  let screen_w, screen_h, canvas_x, canvas_y, canvas_w, canvas_h =
    (1200, 760, 20, 110, 820, 620)
  in
  Raylib.init_window screen_w screen_h "Raytracer UI";
  Raylib.set_target_fps 15;

  let cx0, cy0, cz0 = Vec.vec_to_tup cam.center in
  let fld_w = 120 in
  let fld_h = 26 in
  let inp_x = panel_x + 60 in
  let fields =
    [|
      { text = Printf.sprintf "%.3f" cx0;    label = "X";     x = inp_x; y = 205; w = fld_w; h = fld_h };
      { text = Printf.sprintf "%.3f" cy0;    label = "Y";     x = inp_x; y = 240; w = fld_w; h = fld_h };
      { text = Printf.sprintf "%.3f" cz0;    label = "Z";     x = inp_x; y = 275; w = fld_w; h = fld_h };
      { text = Printf.sprintf "%.3f" cam.yaw;   label = "Yaw";   x = inp_x; y = 345; w = fld_w; h = fld_h };
      { text = Printf.sprintf "%.3f" cam.pitch; label = "Pitch"; x = inp_x; y = 380; w = fld_w; h = fld_h };
    |]
  in
  let state =
    { step = 0.1; image = None; cam; status = "Rendering..."; fields; focused_field = -1 }
  in
  rerender state scene_file;
  sync_fields_from_cam state;

  let rot_step = 0.05 in
  let btn_x = panel_x + 10 in
  let btn_y = 425 in
  let btn_w = 130 in
  let btn_h = 30 in

  while not (Raylib.window_should_close ()) do
    let mx = Raylib.get_mouse_x () in
    let my = Raylib.get_mouse_y () in
    let clicked = Raylib.is_mouse_button_pressed Raylib.MouseButton.Left in

    if clicked then begin
      let hit = ref (-1) in
      Array.iteri
        (fun i f ->
          if mx >= f.x && mx <= f.x + f.w && my >= f.y && my <= f.y + f.h then
            hit := i)
        fields;
      state.focused_field <- !hit;
      if
        mx >= btn_x && mx <= btn_x + btn_w
        && my >= btn_y && my <= btn_y + btn_h
      then begin
        state.focused_field <- -1;
        apply_fields state scene_file
      end
    end;

    if state.focused_field >= 0 then begin
      let f = state.fields.(state.focused_field) in
      if
        Raylib.is_key_pressed Raylib.Key.Backspace
        && String.length f.text > 0
      then f.text <- String.sub f.text 0 (String.length f.text - 1);
      let rec drain () =
        let c = Raylib.get_char_pressed () in
        let ci = Uchar.to_int c in
        if ci > 0 then begin
          if ci < 128 then begin
            let ch = Char.chr ci in
            if (ch >= '0' && ch <= '9') || ch = '-' || ch = '.' then
              f.text <- f.text ^ String.make 1 ch
          end;
          drain ()
        end
      in
      drain ();
      if Raylib.is_key_pressed Raylib.Key.Enter then begin
        state.focused_field <- -1;
        apply_fields state scene_file
      end
    end else begin
      let moved = ref false in
      let cx, cy, cz = Vec.vec_to_tup state.cam.center in
      let ncx = ref cx in
      let ncy = ref cy in
      let ncz = ref cz in
      let nyaw = ref state.cam.yaw in
      let npitch = ref state.cam.pitch in
      if Raylib.is_key_pressed Raylib.Key.A then (
        ncx := !ncx -. state.step;
        moved := true);
      if Raylib.is_key_pressed Raylib.Key.D then (
        ncx := !ncx +. state.step;
        moved := true);
      if Raylib.is_key_pressed Raylib.Key.W then (
        ncy := !ncy +. state.step;
        moved := true);
      if Raylib.is_key_pressed Raylib.Key.S then (
        ncy := !ncy -. state.step;
        moved := true);
      if Raylib.is_key_pressed Raylib.Key.Q then (
        ncz := !ncz +. state.step;
        moved := true);
      if Raylib.is_key_pressed Raylib.Key.E then (
        ncz := !ncz -. state.step;
        moved := true);
      if Raylib.is_key_pressed Raylib.Key.Left then (
        nyaw := !nyaw -. rot_step;
        moved := true);
      if Raylib.is_key_pressed Raylib.Key.Right then (
        nyaw := !nyaw +. rot_step;
        moved := true);
      if Raylib.is_key_pressed Raylib.Key.Up then (
        npitch := !npitch +. rot_step;
        moved := true);
      if Raylib.is_key_pressed Raylib.Key.Down then (
        npitch := !npitch -. rot_step;
        moved := true);
      if Raylib.is_key_pressed Raylib.Key.R then (
        ncx := 0.0;
        ncy := 0.0;
        ncz := 0.0;
        nyaw := 0.0;
        npitch := 0.0;
        moved := true);
      if !moved then begin
        let w = state.cam.image_width in
        let h = state.cam.image_height in
        let aspect = float_of_int w /. float_of_int h in
        state.cam <-
          Camera.make ~image_width:w ~aspect_ratio:aspect ~focal_length:1.
            ~center:(Vec.make (!ncx, !ncy, !ncz))
            ~yaw:!nyaw ~pitch:!npitch ();
        rerender state scene_file;
        sync_fields_from_cam state
      end
    end;

    Raylib.begin_drawing ();
    Raylib.clear_background (color 7 12 20);
    Raylib.draw_text "Raytracer UI" 20 20 34 (color 235 241 255);
    let c = state.cam.center in
    Raylib.draw_text
      (Printf.sprintf "Camera: (%.2f, %.2f, %.2f)  Yaw: %.2f  Pitch: %.2f"
         (Vec.vec_fst c) (Vec.vec_snd c) (Vec.vec_thd c)
         state.cam.yaw state.cam.pitch)
      20 66 20 (color 173 196 224);
    Raylib.draw_text
      (Printf.sprintf
         "Step: %.2f   WASD: translate  Arrow keys: rotate  Q/E: depth  R: reset"
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

    (* Input panel *)
    Raylib.draw_text "Position" panel_x 182 18 (color 173 196 224);
    Raylib.draw_text "Rotation" panel_x 322 18 (color 173 196 224);

    Array.iteri
      (fun i f ->
        let focused = state.focused_field = i in
        let border =
          if focused then color 80 150 255 else color 60 90 130
        in
        Raylib.draw_text f.label panel_x (f.y + 5) 16 (color 143 167 194);
        Raylib.draw_rectangle f.x f.y f.w f.h (color 20 32 50);
        Raylib.draw_rectangle_lines f.x f.y f.w f.h border;
        let display = if focused then f.text ^ "|" else f.text in
        Raylib.draw_text display (f.x + 4) (f.y + 5) 15 (color 230 240 255))
      fields;

    let btn_hover =
      mx >= btn_x && mx <= btn_x + btn_w
      && my >= btn_y && my <= btn_y + btn_h
    in
    let btn_col =
      if btn_hover then color 80 140 240 else color 40 80 180
    in
    Raylib.draw_rectangle btn_x btn_y btn_w btn_h btn_col;
    Raylib.draw_text "Apply" (btn_x + 38) (btn_y + 7) 16 (color 230 240 255);

    Raylib.end_drawing ()
  done;
  Raylib.close_window ()
