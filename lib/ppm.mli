open Vec

type ppm_buffer

val create_file : string -> int -> int -> ppm_buffer
val write_file : ppm_buffer -> vector -> unit
val close_file : ppm_buffer -> unit
