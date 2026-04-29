open Vec
open Ray
open Interval


type bounding_box = {
  min : vector;
  max : vector;
}
(** [hittable] represents an object that can be hit by a ray *)
type hittable =
  | Sphere of vector * float
  | HittableList of hittable list
  | Triangle of vector * vector * vector
  | TriangularMesh of vector list * (int * int * int) list * vector list * bounding_box
(* A triangular mesh is defined by a list of vertices, a tuple list of the
   vertices of each face, and the normal of each face*)

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

(** [hit h r t1 t2] returns a hit_record option representing metadata regarding
    the point of intersection, if any. *)
val hit : hittable -> ray -> interval -> hit_record option

(** [ray_color r obj] returns the color of the ray [r] when it hits the hittable
    [obj] *)
val ray_color : ray -> hittable -> vector
