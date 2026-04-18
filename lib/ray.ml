open Vec

type ray = vector * vector

let origin ray = fst ray
let direction ray = snd ray
let at ray t = add (origin ray) (mult t (direction ray))
