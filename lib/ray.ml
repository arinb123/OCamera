open Vec

type ray = vector * vector

let make orig dir = (orig, dir)
let origin ray = fst ray
let direction ray = snd ray
let at ray t = add (origin ray) (mult t (direction ray))

let ray_color ray =
  let unit_direction = normalize (direction ray) in
  let _, y, _ = vec_to_tup unit_direction in
  let a = 0.5 *. (y +. 1.0) in
  add ((1.0 -. a) ^* Vec.make (1.0, 1.0, 1.0)) (a ^* Vec.make (0.5, 0.7, 1.0))
