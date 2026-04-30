(* An interval is a range of floating-point values *)
type interval = float * float

(** [make (a, b)] creates an interval from the pair (a, b) *)
val make : float * float -> interval

(** [size i] returns the size of the interval [i] *)
val size : interval -> float

(** [contains i x] returns whether the interval [i] contains the value [x] *)
val contains : interval -> float -> bool

(** [surrounds i x] returns whether the interval [i] surrounds the value [x] *)
val surrounds : interval -> float -> bool

(** [empty] returns the empty interval *)
val empty : interval

(** [universe] returns the universe interval *)
val universe : interval

(** [min i] returns the minimum value of the interval [i] *)
val min : interval -> float

(** [max i] returns the maximum value of the interval [i] *)
val max : interval -> float
