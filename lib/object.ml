open Vec
open Ray

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

let rec hit obj r t_min t_max =
  match obj with
  | Sphere (center, radius) ->
      let (oc : vector) = center ^- origin r in

      let a = length_squared (direction r) in
      let h = direction r ^. oc in
      let c = length_squared oc -. (radius *. radius) in
      let discriminant = (h *. h) -. (a *. c) in
      if discriminant < 0. then None
      else
        let t = (h -. sqrt discriminant) /. a in
        if t <= t_min || t >= t_max then
          let t = (h +. sqrt discriminant) /. a in
          if t <= t_min || t >= t_max then None
          else
            let normal = normalize (at r t ^- center) in
            Some (make_hit_record (at r t) normal t obj)
        else
          let normal = normalize (at r t ^- center) in
          Some (make_hit_record (at r t) normal t obj)
  | HittableList (h :: t) -> (
      let hit_left = hit h r t_min t_max in
      match hit_left with
      | None -> hit (HittableList t) r t_min t_max
      | Some rec_left -> (
          let hit_right = hit (HittableList t) r t_min rec_left.t in
          match hit_right with
          | None -> Some rec_left
          | Some rec_right -> Some rec_right))
  | HittableList [] -> None

let ray_color ray obj =
  let hit_record = hit obj ray 0.001 infinity in
  match hit_record with
  | None ->
      let unit_direction = normalize (direction ray) in
      let _, y, _ = vec_to_tup unit_direction in
      let a = 0.5 *. (y +. 1.0) in
      add
        ((1.0 -. a) ^* Vec.make (1.0, 1.0, 1.0))
        (a ^* Vec.make (0.5, 0.7, 1.0))
  | Some hit ->
      if hit.t > 0. then
        match hit.hit_obj with
        | Sphere (sphere_center, _) ->
            let normal = normalize (at ray hit.t ^- sphere_center) in
            0.5 ^* Vec.make (1., 1., 1.) ^+ normal
        | HittableList _ -> failwith "this should never happen"
      else
        let unit_direction = normalize (direction ray) in
        let _, y, _ = vec_to_tup unit_direction in
        let a = 0.5 *. (y +. 1.0) in
        add
          ((1.0 -. a) ^* Vec.make (1.0, 1.0, 1.0))
          (a ^* Vec.make (0.5, 0.7, 1.0))
