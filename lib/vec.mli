type vector

val add : vector -> vector -> vector
val sub : vector -> vector -> vector
val mult : float -> vector -> vector
val dot : vector -> vector -> float
val cross : vector -> vector -> vector
val normalize : vector -> vector
val vec_to_string : vector -> string
val vec_to_tup : vector -> float * float * float
val make : float * float * float -> vector
val ( ^+ ) : vector -> vector -> vector
val ( ^- ) : vector -> vector -> vector
val ( ^* ) : float -> vector -> vector
val ( ^. ) : vector -> vector -> float
val ( ^^ ) : vector -> vector -> vector
