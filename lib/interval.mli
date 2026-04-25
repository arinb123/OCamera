type interval = float * float

val make : float * float -> interval
val size : interval -> float
val contains : interval -> float -> bool
val surrounds : interval -> float -> bool
val empty : interval
val universe : interval
val max : interval -> float
val min : interval -> float
