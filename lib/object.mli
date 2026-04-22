open Vec
open Ray

(** [hittable] represents an object that can be hit by a ray *)
type hittable =
  | Sphere of vector * float
  | HittableList of hittable list

(** [hit_record] defines a hit event, tracking the point of intersection,
    normal, and time *)
type hit_record = {
  p : vector;
  normal : vector;
  t : float;
  hit_obj : hittable;
}

(** [make_hit_record p normal t hit_obj] creates a new hit record with the given
    point, normal, time, and hit object *)
val make_hit_record : vector -> vector -> float -> hittable -> hit_record

(** [hit h r t1 t2] determines if a ray intersects with a hittable object within
    a time range *)
val hit : hittable -> ray -> float -> float -> hit_record option

(** [ray_color r obj] returns the color of the ray [r] when it hits the hittable
    [obj] *)
val ray_color : ray -> hittable -> vector
