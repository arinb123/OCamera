open Vec

type ray

val origin : ray -> vector
val direction : ray -> vector
val at : ray -> float -> vector
