open Vec

type ray

val make : vector -> vector -> ray
val origin : ray -> vector
val direction : ray -> vector
val at : ray -> float -> vector
val ray_color : ray -> vector
