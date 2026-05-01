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
  yaw : float;
  pitch : float;
}

(** [make image_width aspect_ratio focal_length center ?samples_per_pixel
     ?max_depth ?viewport_height ?yaw ?pitch ()] constructs a camera. Derived
    fields (pixel deltas, pixel00_loc, etc.) are computed once here. [yaw] and
    [pitch] are in radians; pitch is clamped to ±1.5 to avoid gimbal lock. *)
val make :
  ?samples_per_pixel:int ->
  ?max_depth:int ->
  ?viewport_height:float ->
  ?yaw:float ->
  ?pitch:float ->
  image_width:int ->
  aspect_ratio:float ->
  focal_length:float ->
  center:vector ->
  unit ->
  camera

(** [parse_yaml_scene yaml_filename] parses a YAML scene file that contains
    both camera and object definitions, returning a [(camera, hittable)] pair. *)
val parse_yaml_scene : string -> camera * Object.hittable

(** [render_from_yaml yaml_filename output_filename ?use_threading] renders the
    scene described in a YAML file (camera + objects) to a PPM file. *)
val render_from_yaml : ?use_threading:bool -> string -> string -> unit

(** [render_yaml_with_cam cam yaml_filename output_filename ?use_threading]
    parses the world from a YAML scene file but uses the provided [cam] instead
    of the camera defined in the file. Useful for interactive UIs that mutate
    the camera while keeping the scene fixed. *)
val render_yaml_with_cam : ?use_threading:bool -> camera -> string -> string -> unit
