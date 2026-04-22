open Vec

(** Abstract type representing a PPM file buffer. *)
type ppm_buffer

(** [create_file filename width height] creates a new PPM file buffer with given
    dimensions. *)
val create_file : string -> int -> int -> ppm_buffer

(** [write_file buffer color] writes a color vector to the buffer. *)
val write_file : ppm_buffer -> vector -> unit

(** [close_file buffer] closes the PPM file buffer. *)
val close_file : ppm_buffer -> unit
