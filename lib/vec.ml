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
let ( ^+ ) = add
let ( ^- ) = sub
let ( ^* ) = mult
let ( ^. ) = dot
let ( ^^ ) = cross
let make (u1, u2, u3) = (u1, u2, u3)
