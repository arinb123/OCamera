open Vec

(* width / height *)
let aspect_ratio = 16.0 /. 9.0
let image_width = 400
let image_height = int_of_float (float_of_int image_width /. aspect_ratio)
let focal_length = 1.
let viewport_height = 2.

let viewport_width =
  viewport_height *. (float_of_int image_width /. float_of_int image_height)

let camera_center = make (0., 0., 0.)
let viewport_u = make (viewport_width, 0., 0.)
let viewport_v = make (0., viewport_height *. -1., 0.)
let pixel_delta_u = (1. /. float_of_int image_width) ^* viewport_u
let pixel_delta_v = (1. /. float_of_int image_height) ^* viewport_v

let viewport_upper_left =
  ((camera_center ^- make (0.0, 0.0, focal_length)) ^- 0.5 ^* viewport_u)
  ^- 0.5 ^* viewport_v

let pixel00_loc = viewport_upper_left ^+ 0.5 ^* pixel_delta_u ^+ pixel_delta_v
