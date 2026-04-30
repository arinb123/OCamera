open Vec

(** A ray in 3D space with an origin and a direction *)
type ray

(** [make orig dir] makes a ray with origin [orig] and direction [dir] *)
val make : Vec.vector -> vector -> ray

(** [origin r] returns the origin of the ray [r] *)
val origin : ray -> Vec.vector

(** [direction r] returns the direction of the ray [r] *)
val direction : ray -> Vec.vector

(** [at r t] returns the point on the ray [r] at parameter [t] *)
val at : ray -> float -> Vec.vector
