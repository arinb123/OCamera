open Vec

(** [aspect_ratio] is the aspect ratio of the image. *)
val aspect_ratio : float

(** [image_width] is the width of the image in pixels. *)
val image_width : int

(** [image_height] is the height of the image in pixels. *)
val image_height : int

(** [focal_length] is the focal length of the camera. *)
val focal_length : float

(** [viewport_height] is the height of the viewport. *)
val viewport_height : float

(** [viewport_width] is the width of the viewport. *)
val viewport_width : float

(** [camera_center] is the position of the camera. *)
val camera_center : vector

(** [pixel_delta_u] is the vector from one pixel to the next in the u direction.
*)
val pixel_delta_u : vector

(** [pixel_delta_v] is the vector from one pixel to the next in the v direction.
*)
val pixel_delta_v : vector

(** [pixel00_loc] is the location of the top-left pixel. *)
val pixel00_loc : vector

(** [render json_filename output_filename] renders shapes from [json_filename]
    to a PPM file at [output_filename]. *)
val render : string -> string -> unit
