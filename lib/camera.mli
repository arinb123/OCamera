open Vec

type camera = {
  center : vector;
  pixel00_loc : vector;
  pixel_delta_u : vector;
  pixel_delta_v : vector;
  image_width : int;
  image_height : int;
  samples_per_pixel : int;
  max_depth : int;
}

(** [make image_width aspect_ratio focal_length center ?samples_per_pixel
     ?max_depth ?viewport_height ()] constructs a camera. Derived fields (pixel
    deltas, pixel00_loc, etc.) are computed once here and stored in the record.
*)
val make :
  ?samples_per_pixel:int ->
  ?max_depth:int ->
  ?viewport_height:float ->
  image_width:int ->
  aspect_ratio:float ->
  focal_length:float ->
  center:vector ->
  unit ->
  camera

(** [render cam json_filename output_filename ?use_threading ()] renders the
    scene to a PPM file. If [use_threading] is true (default), uses all
    available processor cores. *)
val render : ?use_threading:bool -> camera -> string -> string -> unit
