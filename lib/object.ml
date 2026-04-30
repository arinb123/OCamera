open Vec
open Ray
open Interval

type bounding_box = {
  min : vector;
  max : vector;
}

type hittable =
  | Sphere of vector * float
  | HittableList of hittable list
  | Triangle of vector * vector * vector
  | TriangularMesh of
      vector array * (int * int * int) list * vector array * bounding_box

type hit_record = {
  p : vector;
  normal : vector;
  t : float;
  hit_obj : hittable;
}

let make_hit_record p normal t hit_obj = { p; normal; t; hit_obj }

let get_hit_triangle ?normal triangle ray interval : hit_record option =
  match triangle with
  | Triangle (v0, v1, v2) ->
      (* Use provided normal if available, otherwise compute it *)
      let v0v1 = v1 -^ v0 in
      let v0v2 = v2 -^ v0 in
      let n =
        match normal with
        | Some n -> n
        | None -> cross v0v1 v0v2
      in

      let n_dot_dir = dot n (direction ray) in
      if abs_float n_dot_dir < 1e-8 then None
      else
        let d = -1.0 *. dot n v0 in
        let t = -1.0 *. (dot n (origin ray) +. d) /. n_dot_dir in
        if contains interval t then
          let p = at ray t in

          let v0p = p -^ v0 in
          let ne = cross v0v1 v0p in
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

let check_bound_box ray box =
  let org = Ray.origin ray in
  let dir = Ray.direction ray in
  let min = box.min in
  let max = box.max in
  let tmin = (Vec.vec_fst min -. Vec.vec_fst org) /. Vec.vec_fst dir in
  let tmax = (Vec.vec_fst max -. Vec.vec_fst org) /. Vec.vec_fst dir in
  let tmin, tmax = if tmin > tmax then (tmax, tmin) else (tmin, tmax) in
  let y_tmin = (Vec.vec_snd min -. Vec.vec_snd org) /. Vec.vec_snd dir in
  let y_tmax = (Vec.vec_snd max -. Vec.vec_snd org) /. Vec.vec_snd dir in
  let y_tmin, y_tmax =
    if y_tmin > y_tmax then (y_tmax, y_tmin) else (y_tmin, y_tmax)
  in
  if tmin > y_tmax || y_tmin > tmax then false
  else
    let tmin = if y_tmin > tmin then y_tmin else tmin in
    let tmax = if y_tmax < tmax then y_tmax else tmax in
    let z_tmin = (Vec.vec_thd min -. Vec.vec_thd org) /. Vec.vec_thd dir in
    let z_tmax = (Vec.vec_thd max -. Vec.vec_thd org) /. Vec.vec_thd dir in
    let z_tmin, z_tmax =
      if z_tmin > z_tmax then (z_tmax, z_tmin) else (z_tmin, z_tmax)
    in
    if tmin > z_tmax || z_tmin > tmax then false else true

let get_hit_mesh mesh ray interval =
  match mesh with
  | TriangularMesh (vertices, faces, normals, bounding_box) ->
      if check_bound_box ray bounding_box then
        let verts = vertices in
        let temp = Vec.make (infinity, infinity, infinity) in
        let normals = normals in
        let latest_hit, i =
          List.fold_left
            (fun (acc, i) (a, b, c) ->
              let triangle = Triangle (verts.(a), verts.(b), verts.(c)) in
              let triangle_hit =
                get_hit_triangle ~normal:normals.(i) triangle ray interval
              in
              match triangle_hit with
              | None -> (acc, i + 1)
              | Some hit -> if hit.t < acc.t then (hit, i + 1) else (acc, i + 1))
            ( {
                p = temp;
                normal = temp;
                t = infinity;
                hit_obj = Triangle (temp, temp, temp);
              },
              0 )
            faces
        in
        if latest_hit.t = infinity then None else Some latest_hit
      else None
  | _ -> raise (Invalid_argument "get_hit_mesh called on non-mesh object")

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
  | TriangularMesh _ -> get_hit_mesh obj curr_ray interval
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

let rec ray_color ray depth obj rng =
  if depth <= 0 then Vec.make (0., 0., 0.)
  else
    let hit_record = hit obj ray (Interval.make (0.001, infinity)) in
    match hit_record with
    | None ->
        (* render a blue gradient for the background, linear interpolation
           between blue and white depending on y-value. *)
        let unit_direction = normalize (direction ray) in
        let _, y, _ = vec_to_tup unit_direction in
        let a = 0.5 *. (y +. 1.0) in
        ((1.0 -. a) *^ Vec.make (1.0, 1.0, 1.0))
        +^ (a *^ Vec.make (0.5, 0.7, 1.0))
    | Some hit ->
        (* implement true lambertian reflection *)
        let direction = hit.normal +^ Vec.random_on_hemisphere rng hit.normal in
        0.3 *^ ray_color (Ray.make hit.p direction) (depth - 1) obj rng
