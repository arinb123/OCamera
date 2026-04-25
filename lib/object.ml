open Vec
open Ray
open Interval

type hittable =
  | Sphere of vector * float
  | HittableList of hittable list

type hit_record = {
  p : vector;
  normal : vector;
  t : float;
  hit_obj : hittable;
}

let make_hit_record p normal t hit_obj = { p; normal; t; hit_obj }

let rec hit (obj : hittable) (curr_ray : ray) (interval : interval) =
  (* helper method *)
  let make_record_with_normal (t : float) (radius : float)
      (p_intersect : vector) (center : vector) =
    let outward_normal = (1. /. radius) ^* p_intersect ^- center in
    let front_face = dot (direction curr_ray) outward_normal < 0. in
    let normal = if front_face then outward_normal else -1. ^* outward_normal in
    Some (make_hit_record p_intersect normal t obj)
  in
  match obj with
  | Sphere (center, radius) ->
      (* solve for intersection point (if exists), then compute surface
         normals *)
      let (oc : vector) = center ^- origin curr_ray in
      let a = length_squared (direction curr_ray) in
      let h = dot (direction curr_ray) oc in
      let c = length_squared oc -. (radius *. radius) in
      let discriminant = (h *. h) -. (a *. c) in
      if discriminant < 0. then None
      else
        let sqrt_disc = sqrt discriminant in
        let t1 = (h -. sqrt_disc) /. a in
        let t2 = (h +. sqrt_disc) /. a in
        if contains interval t1 then
          let p_intersect = at curr_ray t1 in
          make_record_with_normal t1 radius p_intersect center
        else if contains interval t2 then
          let p_intersect = at curr_ray t2 in
          make_record_with_normal t2 radius p_intersect center
        else None
  | HittableList (h :: t) -> (
      (* recursively traverse the list of objects, collecting the record
         associated w/ the closest hit *)
      let hit_left = hit h curr_ray interval in
      match hit_left with
      | None -> hit (HittableList t) curr_ray interval
      | Some rec_left -> (
          let hit_right =
            hit (HittableList t) curr_ray
              (Interval.make (min interval, rec_left.t))
          in
          match hit_right with
          | None -> Some rec_left
          | Some rec_right -> Some rec_right))
  | HittableList [] -> None

let ray_color ray obj =
  let hit_record = hit obj ray (Interval.make (0.001, infinity)) in
  match hit_record with
  | None ->
      (* render a blue gradient for the background, linear interpolation between
         blue and white depending on y-value. *)
      let unit_direction = normalize (direction ray) in
      let _, y, _ = vec_to_tup unit_direction in
      let a = 0.5 *. (y +. 1.0) in
      add
        ((1.0 -. a) ^* Vec.make (1.0, 1.0, 1.0))
        (a ^* Vec.make (0.5, 0.7, 1.0))
  | Some hit -> 0.5 ^* Vec.make (1., 1., 1.) ^+ hit.normal
