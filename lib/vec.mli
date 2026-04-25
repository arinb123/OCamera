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

(** [u ^+ v] is an alias for [add u v]. *)
val ( ^+ ) : vector -> vector -> vector

(** [u ^- v] is an alias for [sub u v]. *)
val ( ^- ) : vector -> vector -> vector

(** [s ^* v] is an alias for [mult s v]. *)
val ( ^* ) : float -> vector -> vector

(** [u ^. v] is an alias for [dot u v]. *)
val ( ^. ) : vector -> vector -> float

(** [u ^^ v] is an alias for [cross u v]. *)
val ( ^^ ) : vector -> vector -> vector
