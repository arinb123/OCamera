(** A vector in 3D space with three float components *)
type vector

(** [add u v] returns the vector sum of [u] and [v] *)
val add : vector -> vector -> vector

(** [sub u v] returns the vector difference of [u] and [v] *)
val sub : vector -> vector -> vector

(** [mult s v] returns the scalar multiple of [s] and [v] *)
val mult : float -> vector -> vector

(** [dot u v] returns the dot product of [u] and [v] *)
val dot : vector -> vector -> float

(** [cross u v] returns the cross product of [u] and [v] *)
val cross : vector -> vector -> vector

(** [normalize v] returns the normalized version of [v] *)
val normalize : vector -> vector

(** [vec_to_string v] returns the string representation of [v] *)
val vec_to_string : vector -> string

(** [vec_to_tup v] returns the tuple representation of [v] *)
val vec_to_tup : vector -> float * float * float

(** [make x y z] returns a vector with components [x], [y], and [z] *)
val make : float * float * float -> vector

(** [length_squared v] returns the square of the length of [v] *)
val length_squared : vector -> float

(** [vec_fst v] returns the first element of the vector *)
val vec_fst : vector -> float

(** [vec_snd v] returns the second element of the vector *)
val vec_snd : vector -> float

(** [vec_thd v] returns the third element of the vector *)
val vec_thd : vector -> float

(** [vec_eq v1 v2] returns whether v1 and v2 are the same vector (same
    components) *)
val vec_eq : vector -> vector -> bool

(** [random_vec rng min max] returns a random vector in which each component is
    a random float between [min] and [max], using the provided random state
    [rng] *)
val random_unit_vector : Random.State.t -> vector

val random_vec : Random.State.t -> float -> float -> vector

(** [random_on_hemisphere rng normal] generates a random valid unit vector on
    the same hemisphere as the given normal, using the provided random state
    [rng]. *)
val random_on_hemisphere : Random.State.t -> vector -> vector

(** [random_unit_vector rng] generates a random unit vector using the provided
    random state [rng]. *)
val random_unit_vector : Random.State.t -> vector

(** [vec_mul u v] returns the element-wise product of [u] and [v] *)
val vec_mul : vector -> vector -> vector

(** [reflect v n] returns the reflection of vector [v] around normal [n] *)
val reflect : vector -> vector -> vector

(** [vec_min v0 v1] returns the vector with components being the minimum of each
    component of [v0] and [v1]. *)
val vec_min : vector -> vector -> vector

(** [vec_max v0 v1] returns the vector with components being the maximum of each
    component of [v0] and [v1]. *)
val vec_max : vector -> vector -> vector

(** [u +^ v] is an alias for [add u v]. *)
val ( +^ ) : vector -> vector -> vector

(** [u -^ v] is an alias for [sub u v]. *)
val ( -^ ) : vector -> vector -> vector

(** [s *^ v] is an alias for [mult s v]. *)
val ( *^ ) : float -> vector -> vector

(** [v /^ s] is an alias for [mult (1. /. s) v]. *)
val ( /^ ) : vector -> float -> vector

(** [u *^* v] is an alias for [vec_mul u v]. *)
val ( *^* ) : vector -> vector -> vector
