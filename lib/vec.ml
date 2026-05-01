type vector = float * float * float

let add (u1, u2, u3) (v1, v2, v3) = (u1 +. v1, u2 +. v2, u3 +. v3)
let sub (u1, u2, u3) (v1, v2, v3) = add (u1, u2, u3) (-.v1, -.v2, -.v3)
let mult s (u1, u2, u3) = (s *. u1, s *. u2, s *. u3)
let dot (u1, u2, u3) (v1, v2, v3) = (u1 *. v1) +. (u2 *. v2) +. (u3 *. v3)

let cross (u1, u2, u3) (v1, v2, v3) =
  ( (u2 *. v3) -. (u3 *. v2),
    -.((u1 *. v3) -. (u3 *. v1)),
    (u1 *. v2) -. (u2 *. v1) )

let length_squared (u1, u2, u3) = (u1 *. u1) +. (u2 *. u2) +. (u3 *. u3)

let normalize (u1, u2, u3) =
  let magnitude = sqrt (length_squared (u1, u2, u3)) in
  if magnitude = 0.0 then (0.0, 0.0, 0.0)
  else (u1 /. magnitude, u2 /. magnitude, u3 /. magnitude)

let vec_fst = function
  | x, _, _ -> x

let vec_snd = function
  | _, y, _ -> y

let vec_thd = function
  | _, _, z -> z

let vec_to_string (u1, u2, u3) = Printf.sprintf "(%.2f, %.2f, %.2f)" u1 u2 u3
let vec_to_tup (u1, u2, u3) = (u1, u2, u3)
let ( +^ ) = add
let ( -^ ) = sub
let ( *^ ) = mult
let ( /^ ) v s = mult (1. /. s) v
let make (u1, u2, u3) = (u1, u2, u3)

let vec_eq v1 v2 =
  let u1, u2, u3 = v1 in
  let v1, v2, v3 = v2 in
  u1 = v1 && u2 = v2 && u3 = v3

let vec_min v0 v1 =
  let x1, y1, z1 = vec_to_tup v0 in
  let x2, y2, z2 = vec_to_tup v1 in
  make (min x1 x2, min y1 y2, min z1 z2)

let vec_max v0 v1 =
  let x1, y1, z1 = vec_to_tup v0 in
  let x2, y2, z2 = vec_to_tup v1 in
  make (max x1 x2, max y1 y2, max z1 z2)

let random_vec rng min max =
  let rand_float () = (Random.State.float rng 1.0 *. (max -. min)) +. min in
  (rand_float (), rand_float (), rand_float ())

let rec random_unit_vector rng =
  let rand_vec = random_vec rng (-1.0) 1. in
  let lensq = length_squared rand_vec in
  if 1e-160 < lensq && lensq <= 1. then rand_vec /^ sqrt lensq
  else random_unit_vector rng

let random_on_hemisphere rng normal =
  let on_unit_sphere = random_unit_vector rng in
  if dot on_unit_sphere normal > 0.0 then on_unit_sphere
  else -1. *^ on_unit_sphere

let vec_mul (u1, u2, u3) (v1, v2, v3) = (u1 *. v1, u2 *. v2, u3 *. v3)
let ( *^* ) = vec_mul
let reflect v n = v -^ (2.0 *. dot v n *^ n)
