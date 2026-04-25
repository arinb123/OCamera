open Vec
open Ray
open Interval

type hittable =
  | Sphere of vector * float
  | HittableList of hittable list
  | Triangle of vector * vector * vector

type hit_record = {
  p : vector;
  normal : vector;
  t : float;
  hit_obj : hittable;
}

let make_hit_record p normal t hit_obj = { p; normal; t; hit_obj }

let get_hit_triangle triangle ray interval : hit_record option =
  match triangle with
  | Triangle (v0, v1, v2) ->
      (* Check if ray intersects w/ the plane formed by the triangle*)
      let v0v1 = v1 -^ v0 in
      let v0v2 = v2 -^ v0 in
      let n = cross v0v1 v0v2 in
      let n_dot_dir = dot n (direction ray) in
      if abs_float n_dot_dir < 1e-8 then None
      else
        let d = -1.0 *. dot n v0 in
        let t = -1.0 *. (dot n (origin ray) +. d) /. n_dot_dir in
        if contains interval t then
          let p = at ray t in
          (* Now check if p is inside the actual triangle using inside-out
             test *)
          let v0p = p -^ v0 in
          let ne = cross v0v1 v0p in
          (* for each side of the triangle we check if the point is on the right
             side*)
          if dot n ne < 0. then None
          else
            let v1p = p -^ v1 in
            let ne = cross (v2 -^ v1) v1p in
            if dot n ne < 0. then None
            else
              let v2p = p -^ v2 in
              let ne = cross (v0 -^ v2) v2p in
              if dot n ne < 0. then None
              else Some { p; normal = n; t; hit_obj = triangle }
        else None
  | _ ->
      raise (Invalid_argument "get_hit_triangle called on non-triangle object")

let get_hit_sphere (sphere : hittable) (curr_ray : ray) (interval : interval) :
    hit_record option =
  match sphere with
  | Sphere (center, radius) ->
      let make_record_with_normal (t : float) (radius : float)
          (p_intersect : vector) (center : vector) =
        let outward_normal = (p_intersect -^ center) /^ radius in
        let front_face = dot (direction curr_ray) outward_normal < 0. in
        let normal =
          if front_face then outward_normal else -1. *^ outward_normal
        in
        Some (make_hit_record p_intersect normal t sphere)
      in
      let (oc : vector) = center -^ origin curr_ray in
      let a = length_squared (direction curr_ray) in
      let h = dot (direction curr_ray) oc in
      let c = length_squared oc -. (radius *. radius) in
      let discriminant = (h *. h) -. (a *. c) in
      if discriminant < 0. then None
      else
        let sqrt_disc = sqrt discriminant in
        let t1 = (h -. sqrt_disc) /. a in
        let t2 = (h +. sqrt_disc) /. a in
        (* nearest acceptable root lying within the range *)
        if contains interval t1 then
          let p_intersect = at curr_ray t1 in
          make_record_with_normal t1 radius p_intersect center
        else if contains interval t2 then
          let p_intersect = at curr_ray t2 in
          make_record_with_normal t2 radius p_intersect center
        else None
  | _ -> raise (Invalid_argument "get_hit_sphere called on non-sphere object")

let rec hit (obj : hittable) (curr_ray : ray) (interval : interval) =
  (* helper method *)
  match obj with
  | Sphere _ -> get_hit_sphere obj curr_ray interval
  | Triangle _ -> get_hit_triangle obj curr_ray interval
  | HittableList (h :: t) -> (
      (* recursively traverse the list of objects, collecting the record
         associated w/ the closest hit *)
      let hit_left = hit h curr_ray interval in
      match hit_left with
      | None -> hit (HittableList t) curr_ray interval
      | Some rec_left -> (
          let hit_right =
            hit (HittableList t) curr_ray
              (Interval.make (Interval.min interval, rec_left.t))
          in
          (* if hit_right gives a result, then we prefer it over rec_left *)
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
      ((1.0 -. a) *^ Vec.make (1.0, 1.0, 1.0)) +^ (a *^ Vec.make (0.5, 0.7, 1.0))
  | Some hit -> 0.5 *^ (hit.normal +^ Vec.make (1., 1., 1.))
