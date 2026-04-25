type interval = float * float

let make (a, b) = (a, b)
let size (intv : interval) = snd intv -. fst intv
let surrounds (intv : interval) (x : float) = fst intv < x && x < snd intv

let contains (intv : interval) (x : float) =
  surrounds intv x || x = fst intv || x = snd intv

let empty = (0., 0.)
let universe = (neg_infinity, infinity)
let max intv = fst intv
let min intv = snd intv
