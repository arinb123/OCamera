open Vec

type ray = vector * vector

let make orig dir = (orig, dir)
let origin ray = fst ray
let direction ray = snd ray
let at ray t = add (origin ray) (mult t (direction ray))

let hit_sphere (center : vector) (radius : float) (r : ray) : float =
  let (oc : vector) = center ^- origin r in
  let a = direction r ^. direction r in
  let b = -2.0 *. (direction r ^. oc) in
  let c = (oc ^. oc) -. (radius *. radius) in
  let discriminant = (b *. b) -. (4. *. a *. c) in
  if discriminant < 0. then -1. else (-.b -. sqrt discriminant) /. (2. *. a)

let ray_color ray =
  let sphere_center = Vec.make (0., 0., -1.) in
  let t = hit_sphere sphere_center 0.5 ray in
  if t > 0. then
    let normal = normalize (at ray t ^- sphere_center) in
    0.5 ^* Vec.make (1., 1., 1.) ^+ normal
  else
    let unit_direction = normalize (direction ray) in
    let _, y, _ = vec_to_tup unit_direction in
    let a = 0.5 *. (y +. 1.0) in
    add ((1.0 -. a) ^* Vec.make (1.0, 1.0, 1.0)) (a ^* Vec.make (0.5, 0.7, 1.0))
