open OUnit2
open QCheck
module V = Raytracer3110.Vec
module R = Raytracer3110.Ray
module O = Raytracer3110.Object
module C = Raytracer3110.Camera
open Raytracer3110.Vec
open Raytracer3110.Ray
open Raytracer3110.Ppm
open Raytracer3110.Object
open Raytracer3110.Interval

let eps = 1e-6
let float_eq a b = abs_float (a -. b) < eps

let v (a, b, c) = V.make (a, b, c)

let read_file filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  read_lines []

let cam_test =
  C.make ~image_width:400 ~aspect_ratio:(16. /. 9.) ~focal_length:1.
    ~center:(V.make (0., 0., 5.))
    ()

let rng = Random.State.make [| 42 |]

let vector_tests =
  "vector tests"
  >::: [
         ( "vector add" >:: fun _ ->
           assert_equal
             (v (1.0, 2.0, 3.0))
             (add (v (0.0, 1.0, 2.0)) (v (1.0, 1.0, 1.0))) );
         ( "vector sub" >:: fun _ ->
           assert_equal
             (v (2.0, 1.0, 0.0))
             (sub (v (3.0, 2.0, 1.0)) (v (1.0, 1.0, 1.0))) );
         ( "scalar multiplication" >:: fun _ ->
           assert_equal (v (2.0, 4.0, 6.0)) (mult 2.0 (v (1.0, 2.0, 3.0))) );
         ( "dot product" >:: fun _ ->
           assert_equal 14.0 (dot (v (1.0, 2.0, 3.0)) (v (1.0, 2.0, 3.0))) );
         ( "cross product" >:: fun _ ->
           assert_equal
             (v (0.0, 0.0, 1.0))
             (cross (v (1.0, 0.0, 0.0)) (v (0.0, 1.0, 0.0))) );
         ( "length squared" >:: fun _ ->
           assert_equal 14.0 (length_squared (v (1.0, 2.0, 3.0))) );
         ( "normalize nonzero" >:: fun _ ->
           assert_equal (v (1.0, 0.0, 0.0)) (normalize (v (1.0, 0.0, 0.0))) );
         ( "normalize zero" >:: fun _ ->
           assert_equal (v (0.0, 0.0, 0.0)) (normalize (v (0.0, 0.0, 0.0))) );
         ( "vector to string" >:: fun _ ->
           assert_equal "(1.00, 2.00, 3.00)" (vec_to_string (v (1.0, 2.0, 3.0)))
         );
         ( "infix add" >:: fun _ ->
           assert_equal
             (v (1.0, 2.0, 3.0))
             (v (0.0, 1.0, 2.0) +^ v (1.0, 1.0, 1.0)) );
         ( "infix sub" >:: fun _ ->
           assert_equal
             (v (2.0, 1.0, 0.0))
             (v (3.0, 2.0, 1.0) -^ v (1.0, 1.0, 1.0)) );
         ( "infix mult" >:: fun _ ->
           assert_equal (v (2.0, 4.0, 6.0)) (2.0 *^ v (1.0, 2.0, 3.0)) );
         ( "vec fst/snd/thd" >:: fun _ ->
           assert_equal (V.vec_fst (v (0.0, 1.0, 2.0))) 0.0;
           assert_equal (V.vec_snd (v (0.0, 1.0, 2.0))) 1.0;
           assert_equal (V.vec_thd (v (0.0, 1.0, 2.0))) 2.0 );
         ( "vec_min picks minimum per component" >:: fun _ ->
           let r = vec_min (V.make (1., 8., 3.)) (V.make (5., 2., 7.)) in
           assert_equal (vec_to_tup r) (1., 2., 3.) );
         ( "vec_min with equal vectors" >:: fun _ ->
           let u = V.make (3., 3., 3.) in
           assert_bool "equal" (vec_eq (vec_min u u) u) );
         ( "vec_min with negative components" >:: fun _ ->
           let r = vec_min (V.make (-1., -2., -3.)) (V.make (-4., -1., -2.)) in
           assert_equal (vec_to_tup r) (-4., -2., -3.) );
         ( "vec_max picks maximum per component" >:: fun _ ->
           let r = vec_max (V.make (1., 8., 3.)) (V.make (5., 2., 7.)) in
           assert_equal (vec_to_tup r) (5., 8., 7.) );
         ( "vec_max with equal vectors" >:: fun _ ->
           let u = V.make (3., 3., 3.) in
           assert_bool "equal" (vec_eq (vec_max u u) u) );
         ( "vec_min and vec_max are complementary" >:: fun _ ->
           let v0 = V.make (1., 8., 3.) in
           let v1 = V.make (5., 2., 7.) in
           let lo = vec_min v0 v1 in
           let hi = vec_max v0 v1 in
           let lx, ly, lz = vec_to_tup lo in
           let hx, hy, hz = vec_to_tup hi in
           assert_bool "x" (lx <= hx);
           assert_bool "y" (ly <= hy);
           assert_bool "z" (lz <= hz) );
         ( "random_on_hemisphere is a unit vector in correct hemisphere" >:: fun _ ->
           let normal = V.make (0., 1., 0.) in
           for _ = 1 to 100 do
             let u = random_on_hemisphere rng normal in
             let len = sqrt (length_squared u) in
             assert_bool "len ≈ 1" (abs_float (len -. 1.0) < 1e-10);
             assert_bool "dot > 0" (dot u normal > 0.0)
           done );
       ]

let order_of_operations_tests =
  "operator precedence tests"
  >::: [
         ( "vector divide matches scalar reciprocal multiply" >:: fun _ ->
           let p = v (5.0, 7.0, 9.0) in
           let c = v (1.0, 2.0, 3.0) in
           let radius = 2.0 in
           let expr = (p -^ c) /^ radius in
           let expected = 1.0 /. radius *^ (p -^ c) in
           assert_bool "expected (p -^ c) /^ r = (1/r) *^ (p -^ c)"
             (vec_eq expr expected) );
         ( "subtraction chain needs explicit grouping" >:: fun _ ->
           let a = v (10.0, 10.0, 10.0) in
           let b = v (1.0, 2.0, 3.0) in
           let c = v (4.0, 5.0, 6.0) in
           let d = v (7.0, 8.0, 9.0) in
           let grouped_left = a -^ b -^ c -^ d in
           let grouped_right = a -^ (b -^ (c -^ d)) in
           assert_bool
             "left and right groupings differ; parenthesize subtraction chains"
             (not (vec_eq grouped_left grouped_right)) );
       ]

let ray_tests =
  "ray tests"
  >::: [
         ( "make ray" >:: fun _ ->
           let r = R.make (v (0.0, 0.0, 0.0)) (v (1.0, 0.0, 0.0)) in
           assert_equal (v (0.0, 0.0, 0.0)) (origin r);
           assert_equal (v (1.0, 0.0, 0.0)) (direction r) );
         ( "at t = 0" >:: fun _ ->
           let r = R.make (v (1.0, 2.0, 3.0)) (v (4.0, 5.0, 6.0)) in
           assert_equal (v (1.0, 2.0, 3.0)) (at r 0.0) );
         ( "at t = 1" >:: fun _ ->
           let r = R.make (v (1.0, 2.0, 3.0)) (v (1.0, 0.0, 0.0)) in
           assert_equal (v (2.0, 2.0, 3.0)) (at r 1.0) );
         ( "at t = 2" >:: fun _ ->
           let r = R.make (v (0.0, 0.0, 0.0)) (v (1.0, 1.0, 1.0)) in
           assert_equal (v (2.0, 2.0, 2.0)) (at r 2.0) );
       ]

let ppm_tests =
  "ppm tests"
  >::: [
         ( "create file writes header" >:: fun _ ->
           let filename = "test1.ppm" in
           let buf = create_file filename 2 2 in
           close_file buf;
           let lines = read_file filename in
           assert_equal "P3" (List.nth lines 0);
           assert_equal "2 2" (String.trim (List.nth lines 1));
           assert_equal "255" (String.trim (List.nth lines 2)) );
         ( "write single white pixel" >:: fun _ ->
           let filename = "test2.ppm" in
           let buf = create_file filename 1 1 in
           write_file buf (v (1.0, 1.0, 1.0));
           close_file buf;
           let lines = read_file filename in
           assert_equal "255 255 255" (String.trim (List.nth lines 3)) );
         ( "write black pixel" >:: fun _ ->
           let filename = "test3.ppm" in
           let buf = create_file filename 1 1 in
           write_file buf (v (0.0, 0.0, 0.0));
           close_file buf;
           let lines = read_file filename in
           assert_equal "0 0 0" (String.trim (List.nth lines 3)) );
         ( "write red pixel scaling check" >:: fun _ ->
           let filename = "test4.ppm" in
           let buf = create_file filename 1 1 in
           write_file buf (v (1.0, 0.0, 0.0));
           close_file buf;
           let lines = read_file filename in
           assert_equal "255 0 0" (String.trim (List.nth lines 3)) );
         ( "multiple writes append correctly" >:: fun _ ->
           let filename = "test5.ppm" in
           let buf = create_file filename 2 1 in
           write_file buf (v (1.0, 0.0, 0.0));
           write_file buf (v (0.0, 1.0, 0.0));
           close_file buf;
           let lines = read_file filename in
           assert_equal "255 0 0" (String.trim (List.nth lines 3));
           assert_equal "0 255 0" (String.trim (List.nth lines 4)) );
       ]

let interval_tests =
  "interval_tests"
  >::: [
         ( "make returns correct pair" >:: fun _ ->
           assert_equal (1.0, 5.0) (make (1.0, 5.0)) );
         ( "size of normal interval" >:: fun _ ->
           assert_equal 4.0 (size (make (1.0, 5.0))) );
         ( "size of empty interval is zero" >:: fun _ ->
           assert_equal 0.0 (size empty) );
         ( "surrounds true for interior, false for endpoints/outside" >:: fun _ ->
           assert_bool "interior" (surrounds (make (0.0, 10.0)) 5.0);
           assert_bool "left endpoint" (not (surrounds (make (0.0, 10.0)) 0.0));
           assert_bool "right endpoint" (not (surrounds (make (0.0, 10.0)) 10.0));
           assert_bool "outside" (not (surrounds (make (0.0, 10.0)) 11.0)) );
         ( "contains true for interior and endpoints, false outside" >:: fun _ ->
           assert_bool "interior" (contains (make (0.0, 10.0)) 5.0);
           assert_bool "left" (contains (make (0.0, 10.0)) 0.0);
           assert_bool "right" (contains (make (0.0, 10.0)) 10.0);
           assert_bool "outside" (not (contains (make (0.0, 10.0)) 11.0)) );
         ( "universe contains large values and surrounds zero" >:: fun _ ->
           assert_bool "large pos" (contains universe 1e308);
           assert_bool "large neg" (contains universe (-1e308));
           assert_bool "surrounds 0" (surrounds universe 0.0) );
         ( "min and max return bounds" >:: fun _ ->
           assert_equal 2.0 (min (make (2.0, 8.0)));
           assert_equal 8.0 (max (make (2.0, 8.0))) );
       ]

let camera_tests =
  "camera tests"
  >::: [
         ( "image dimensions and defaults" >:: fun _ ->
           let cam =
             C.make ~image_width:400 ~aspect_ratio:(16. /. 9.)
               ~focal_length:1.0 ~center:(V.make (0., 0., 0.)) ()
           in
           assert_equal 225 cam.image_height;
           assert_equal 400 cam.image_width;
           assert_equal 10 cam.samples_per_pixel;
           assert_equal 10 cam.max_depth;
           assert_bool "yaw = 0.0" (float_eq cam.yaw 0.0);
           assert_bool "pitch = 0.0" (float_eq cam.pitch 0.0) );
         ( "image height is at least 1 for tiny image" >:: fun _ ->
           let cam =
             C.make ~image_width:1 ~aspect_ratio:1000.0
               ~focal_length:1.0 ~center:(V.make (0., 0., 0.)) ()
           in
           assert_equal 1 cam.image_height );
         ( "custom fields stored correctly" >:: fun _ ->
           let center = V.make (1., 2., 3.) in
           let cam =
             C.make ~samples_per_pixel:50 ~max_depth:20 ~yaw:1.0
               ~image_width:400 ~aspect_ratio:(16. /. 9.)
               ~focal_length:1.0 ~center ()
           in
           assert_equal 50 cam.samples_per_pixel;
           assert_equal 20 cam.max_depth;
           assert_equal center cam.center;
           assert_bool "yaw = 1.0" (float_eq cam.yaw 1.0) );
         ( "pitch clamping" >:: fun _ ->
           let mk p =
             C.make ~pitch:p ~image_width:100 ~aspect_ratio:1.0
               ~focal_length:1.0 ~center:(V.make (0., 0., 0.)) ()
           in
           assert_bool "hi clamped to 1.5" (float_eq (mk 2.0).pitch 1.5);
           assert_bool "lo clamped to -1.5" (float_eq (mk (-2.0)).pitch (-1.5));
           assert_bool "valid not clamped" (float_eq (mk 1.0).pitch 1.0) );
         ( "pixel_delta_u/v orthogonal and correctly oriented" >:: fun _ ->
           let cam =
             C.make ~image_width:400 ~aspect_ratio:(16. /. 9.)
               ~focal_length:1.0 ~yaw:0.0 ~pitch:0.0
               ~center:(V.make (0., 0., 0.)) ()
           in
           assert_bool "orthogonal"
             (abs_float (V.dot cam.pixel_delta_u cam.pixel_delta_v) < 1e-10);
           let ux, uy, uz = vec_to_tup cam.pixel_delta_u in
           assert_bool "delta_u +x" (ux > 0.0);
           assert_bool "delta_u y=0" (abs_float uy < 1e-10);
           assert_bool "delta_u z=0" (abs_float uz < 1e-10);
           let vx, vy, vz = vec_to_tup cam.pixel_delta_v in
           assert_bool "delta_v x=0" (abs_float vx < 1e-10);
           assert_bool "delta_v -y" (vy < 0.0);
           assert_bool "delta_v z=0" (abs_float vz < 1e-10) );
         ( "pixel00_loc placement" >:: fun _ ->
           let cam1 =
             C.make ~image_width:400 ~aspect_ratio:(16. /. 9.)
               ~focal_length:1.0 ~yaw:0.0 ~pitch:0.0
               ~center:(V.make (0., 0., 0.)) ()
           in
           let cam2 =
             C.make ~image_width:400 ~aspect_ratio:(16. /. 9.)
               ~focal_length:1.0 ~center:(V.make (1., 0., 0.)) ()
           in
           let _, _, z = vec_to_tup cam1.pixel00_loc in
           let x1, _, _ = vec_to_tup cam1.pixel00_loc in
           let x2, _, _ = vec_to_tup cam2.pixel00_loc in
           assert_bool "z = -1.0" (abs_float (z -. (-1.0)) < 1e-10);
           assert_bool "shifts by 1 in x" (abs_float (x2 -. x1 -. 1.0) < 1e-10) );
       ]

let vec_eq v1 v2 =
  let x1, y1, z1 = vec_to_tup v1 in
  let x2, y2, z2 = vec_to_tup v2 in
  float_eq x1 x2 && float_eq y1 y2 && float_eq z1 z2

let gen_vec =
  let open Gen in
  triple (float_range (-100.) 100.) (float_range (-100.) 100.)
    (float_range (-100.) 100.)
  |> map V.make

let arb_vec = QCheck.make gen_vec

let add_comm =
  Test.make ~name:"vector addition is commutative" arb_vec (fun v1 ->
      let v2 =
        V.make
          ( Random.float 200. -. 100.,
            Random.float 200. -. 100.,
            Random.float 200. -. 100. )
      in
      vec_eq (add v1 v2) (add v2 v1))

let add_assoc =
  Test.make ~name:"vector addition is associative" arb_vec (fun v1 ->
      let v2 =
        V.make
          ( Random.float 200. -. 100.,
            Random.float 200. -. 100.,
            Random.float 200. -. 100. )
      in
      let v3 =
        V.make
          ( Random.float 200. -. 100.,
            Random.float 200. -. 100.,
            Random.float 200. -. 100. )
      in
      vec_eq (add (add v1 v2) v3) (add v1 (add v2 v3)))

let scalar_mult_dist =
  Test.make ~name:"scalar multiplication distributes\n   over vector addition"
    arb_vec (fun v1 ->
      let v2 =
        V.make
          ( Random.float 200. -. 100.,
            Random.float 200. -. 100.,
            Random.float 200. -. 100. )
      in
      let s = Random.float 10. in
      vec_eq (mult s (add v1 v2)) (add (mult s v1) (mult s v2)))

let dot_linear =
  Test.make ~name:"dot product is linear in first argument" arb_vec (fun v1 ->
      let v2 =
        V.make
          ( Random.float 200. -. 100.,
            Random.float 200. -. 100.,
            Random.float 200. -. 100. )
      in
      let v3 =
        V.make
          ( Random.float 200. -. 100.,
            Random.float 200. -. 100.,
            Random.float 200. -. 100. )
      in
      let s = Random.float 10. in
      float_eq (dot (add v1 v2) v3) (dot v1 v3 +. dot v2 v3)
      && float_eq (dot (mult s v1) v3) (s *. dot v1 v3))

let cross_orthogonal =
  Test.make ~name:"cross product of orthogonal vectors\n   is nonzero" arb_vec
    (fun v1 ->
      let x1, y1, _ = vec_to_tup v1 in
      let v2 = V.make (-.y1, x1, 0.) in
      let cross_prod = cross v1 v2 in
      let x, y, z = vec_to_tup cross_prod in
      not (float_eq x 0. && float_eq y 0. && float_eq z 0.))

let cross_anticomm =
  Test.make ~name:"cross product is anti-commutative" arb_vec (fun v1 ->
      let v2 =
        V.make
          ( Random.float 200. -. 100.,
            Random.float 200. -. 100.,
            Random.float 200. -. 100. )
      in
      vec_eq (cross v1 v2) (mult (-1.) (cross v2 v1)))

let length_dot =
  Test.make ~name:"length squared equals dot product with\n   itself" arb_vec
    (fun v1 -> float_eq (length_squared v1) (dot v1 v1))

let gen_ray = Gen.pair gen_vec gen_vec
let arb_ray = QCheck.make gen_ray

let test_ray_at =
  Test.make ~name:"ray at function" arb_ray (fun (origin, direction) ->
      let r = R.make origin direction in
      let t = Random.float 100. in
      vec_eq (at r t) (add origin (mult t direction)))

let gen_sphere = Gen.pair (Gen.float_range 0.1 100.) gen_vec
let arb_sphere = QCheck.make gen_sphere

let test_sphere_hit =
  Test.make ~name:"sphere hit satisfies sphere equation"
    (QCheck.pair arb_ray arb_sphere) (fun ((ray_o, ray_d), (radius, origin)) ->
      let r = R.make ray_o ray_d in
      let s =
        O.Sphere (origin, radius, O.Lambertian (V.make (0.8, 0.8, 0.8)))
      in
      match O.hit s r (make (0.001, infinity)) with
      | Some hit_rec ->
          let hit_point = hit_rec.p in
          let hit_vec = sub hit_point origin in
          float_eq (length_squared hit_vec) (radius *. radius)
      | None -> true)

let qcheck_tests =
  List.map QCheck_ounit.to_ounit2_test
    [
      add_comm;
      add_assoc;
      scalar_mult_dist;
      dot_linear;
      cross_orthogonal;
      cross_anticomm;
      length_dot;
      test_ray_at;
      test_sphere_hit;
    ]

let orig = V.make (0., 0., 0.)
let fwd = V.make (0., 0., 1.)
let ray_fwd = R.make orig fwd
let wide = Raytracer3110.Interval.make (0.001, 1000.)
let sphere_z5 = Sphere (V.make (0., 0., 5.), 1., Lambertian (V.make (0.8, 0.8, 0.8)))
let sphere_neg = Sphere (V.make (0., 0., -5.), 1., Lambertian (V.make (0.8, 0.8, 0.8)))
let tri_v0 = V.make (0., 0., 1.)
let tri_v1 = V.make (1., 0., 1.)
let tri_v2 = V.make (0., 1., 1.)
let triangle = Triangle (tri_v0, tri_v1, tri_v2, Lambertian (V.make (0.8, 0.8, 0.8)))
let tri_v0 = V.make (-1., -1., 1.)
let tri_v1 = V.make (1., -1., 1.)
let tri_v2 = V.make (0., 1., 1.)
let wide_triangle = Triangle (tri_v0, tri_v1, tri_v2, Lambertian (V.make (0.8, 0.8, 0.8)))

let hittable_tests =
  "hittable_tests"
  >::: [
         ( "make_hit_record stores all fields" >:: fun _ ->
           let r = make_hit_record orig fwd 3.14 sphere_z5 in
           assert_equal orig r.p;
           assert_equal fwd r.normal;
           assert_equal 3.14 r.t;
           assert_equal sphere_z5 r.hit_obj );
         ( "sphere hit returns Some" >:: fun _ ->
           assert_bool "expected hit"
             (Option.is_some (hit sphere_z5 ray_fwd wide)) );
         ( "sphere hit t is approximately 4.0 (front surface)" >:: fun _ ->
           match hit sphere_z5 ray_fwd wide with
           | None -> assert_failure "expected a hit"
           | Some r -> assert_bool "t ≈ 4.0" (abs_float (r.t -. 4.0) < 1e-6) );
         ( "sphere hit point lies on sphere surface" >:: fun _ ->
           match hit sphere_z5 ray_fwd wide with
           | None -> assert_failure "expected a hit"
           | Some r ->
               let center = V.make (0., 0., 5.) in
               let dist = sqrt (V.length_squared (r.p -^ center)) in
               assert_bool "dist ≈ 1.0" (abs_float (dist -. 1.0) < 1e-6) );
         ( "sphere normal opposes ray direction (front face)" >:: fun _ ->
           match hit sphere_z5 ray_fwd wide with
           | None -> assert_failure "expected a hit"
           | Some r -> assert_bool "dot < 0" (V.dot r.normal fwd < 0.) );
         ( "sphere behind ray returns None" >:: fun _ ->
           assert_bool "expected no hit"
             (Option.is_none (hit sphere_neg ray_fwd wide)) );
         ( "sphere outside interval returns None" >:: fun _ ->
           let tiny = Raytracer3110.Interval.make (0.001, 1.) in
           assert_bool "expected no hit"
             (Option.is_none (hit sphere_z5 ray_fwd tiny)) );
         ( "sphere lateral miss returns None" >:: fun _ ->
           let side_ray = R.make (V.make (5., 0., 0.)) fwd in
           assert_bool "expected no hit"
             (Option.is_none (hit sphere_z5 side_ray wide)) );
         ( "triangle ray along +z hits z=1 plane" >:: fun _ ->
           assert_bool "expected hit"
             (Option.is_some (hit triangle ray_fwd wide)) );
         ( "triangle hit t is approximately 1.0" >:: fun _ ->
           match hit triangle ray_fwd wide with
           | None -> assert_failure "expected a hit"
           | Some r -> assert_bool "t ≈ 1.0" (abs_float (r.t -. 1.0) < 1e-6) );
         ( "triangle hit point is in z=1 plane" >:: fun _ ->
           match hit triangle ray_fwd wide with
           | None -> assert_failure "expected a hit"
           | Some r ->
               assert_bool "z ≈ 1.0" (abs_float (V.vec_thd r.p -. 1.0) < 1e-6)
         );
         ( "triangle ray misses when outside triangle bounds" >:: fun _ ->
           let outside = R.make orig (V.make (0.9, 0.9, 1.)) in
           assert_bool "expected no hit"
             (Option.is_none (hit triangle outside wide)) );
         ( "triangle parallel ray returns None" >:: fun _ ->
           let parallel = R.make orig (V.make (1., 0., 0.)) in
           assert_bool "expected no hit"
             (Option.is_none (hit triangle parallel wide)) );
         ( "triangle outside interval returns None" >:: fun _ ->
           let tiny = Raytracer3110.Interval.make (0.001, 0.5) in
           assert_bool "expected no hit"
             (Option.is_none (hit triangle ray_fwd tiny)) );
         ( "empty list returns None" >:: fun _ ->
           assert_bool "expected no hit"
             (Option.is_none (hit (HittableList []) ray_fwd wide)) );
         ( "single sphere list returns hit" >:: fun _ ->
           assert_bool "expected hit"
             (Option.is_some (hit (HittableList [ sphere_z5 ]) ray_fwd wide)) );
         ( "list returns closest of two spheres" >:: fun _ ->
           let near = Sphere (V.make (0., 0., 3.), 0.5, Lambertian (V.make (0.8, 0.8, 0.8))) in
           let far = Sphere (V.make (0., 0., 8.), 0.5, Lambertian (V.make (0.8, 0.8, 0.8))) in
           match hit (HittableList [ far; near ]) ray_fwd wide with
           | None -> assert_failure "expected a hit"
           | Some r ->
               assert_bool "t closer to near sphere" (r.t < 4.0) );
         ( "list of all misses returns None" >:: fun _ ->
           let side = R.make orig (V.make (1., 0., 0.)) in
           assert_bool "expected no hit"
             (Option.is_none
                (hit (HittableList [ sphere_z5; triangle ]) side wide)) );
         ( "list hit obj matches the struck hittable" >:: fun _ ->
           match hit (HittableList [ sphere_z5 ]) ray_fwd wide with
           | None -> assert_failure "expected a hit"
           | Some r -> assert_equal sphere_z5 r.hit_obj );
         ( "mesh hit returns Some when ray passes through bounding box and triangle"
         >:: fun _ ->
           let verts =
             [|
               V.make (-1., -1., 5.); V.make (1., -1., 5.); V.make (0., 1., 5.);
             |]
           in
           let faces = [ (0, 1, 2) ] in
           let normals = [| V.make (0., 0., 1.) |] in
           let bbox =
             { min = V.make (-2., -2., 4.); max = V.make (2., 2., 6.) }
           in
           let mesh = TriangularMesh (verts, faces, normals, bbox, Lambertian (V.make (0.8, 0.8, 0.8))) in
           assert_bool "expected hit" (Option.is_some (hit mesh ray_fwd wide))
         );
         ( "mesh returns closest face when two faces present" >:: fun _ ->
           let verts =
             [|
               V.make (-1., -1., 3.);
               V.make (1., -1., 3.);
               V.make (0., 1., 3.);
               V.make (-1., -1., 7.);
               V.make (1., -1., 7.);
               V.make (0., 1., 7.);
             |]
           in
           let faces = [ (0, 1, 2); (3, 4, 5) ] in
           let normals = [| V.make (0., 0., 1.); V.make (0., 0., 1.) |] in
           let bbox =
             { min = V.make (-2., -2., 2.); max = V.make (2., 2., 8.) }
           in
           let mesh = TriangularMesh (verts, faces, normals, bbox, Lambertian (V.make (0.8, 0.8, 0.8))) in
           match hit mesh ray_fwd wide with
           | None -> assert_failure "expected a hit"
           | Some r ->
               assert_bool "t ≈ 3.0 (near face)" (abs_float (r.t -. 3.0) < 1e-6)
         );
       ]

let write_temp_yaml content =
  let f = Filename.temp_file "raytest" ".yaml" in
  let oc = open_out f in
  output_string oc content;
  close_out oc;
  f

let write_temp_obj content =
  let f = Filename.temp_file "raytest" ".obj" in
  let oc = open_out f in
  output_string oc content;
  close_out oc;
  f

let vec_extra_tests =
  "vec_extra_tests"
  >::: [
    ( "vec_mul component-wise product" >:: fun _ ->
      let a = V.make (2., 3., 4.) and b = V.make (5., 6., 7.) in
      assert_bool "vec_mul" (vec_eq (V.vec_mul a b) (V.make (10., 18., 28.))) );
    ( "vec_mul by zero vector" >:: fun _ ->
      let a = V.make (2., 3., 4.) and z = V.make (0., 0., 0.) in
      assert_bool "vec_mul zero" (vec_eq (V.vec_mul a z) z) );
    ( "vec_mul infix *^*" >:: fun _ ->
      let a = V.make (1., 2., 3.) and b = V.make (3., 2., 1.) in
      assert_bool "*^*" (vec_eq (a *^* b) (V.make (3., 4., 3.))) );
    ( "reflect straight down off horizontal surface" >:: fun _ ->
      assert_bool "reflect"
        (vec_eq
           (V.reflect (V.make (0., -1., 0.)) (V.make (0., 1., 0.)))
           (V.make (0., 1., 0.))) );
    ( "reflect 45 degrees off horizontal surface" >:: fun _ ->
      assert_bool "reflect 45"
        (vec_eq
           (V.reflect (V.make (1., -1., 0.)) (V.make (0., 1., 0.)))
           (V.make (1., 1., 0.))) );
    ( "reflect preserves tangential component" >:: fun _ ->
      let r = V.reflect (V.make (3., -4., 0.)) (V.make (0., 1., 0.)) in
      let rx, ry, _ = vec_to_tup r in
      assert_bool "tangential x=3" (float_eq rx 3.0);
      assert_bool "normal flipped y=4" (float_eq ry 4.0) );
  ]

let object_extra_tests =
  let make_rng () = Random.State.make [| 42 |] in
  "object_extra_tests"
  >::: [
    ( "ray_color depth=0 returns black" >:: fun _ ->
      let color =
        O.ray_color
          (R.make (V.make (0., 0., 0.)) (V.make (0., 0., 1.)))
          0 (HittableList []) (make_rng ())
      in
      assert_bool "black" (vec_eq color (V.make (0., 0., 0.))) );

    ( "ray_color no-hit sky gradient" >:: fun _ ->
      let up =
        O.ray_color
          (R.make (V.make (0., 0., 0.)) (V.make (0., 1., 0.)))
          5 (HittableList []) (make_rng ())
      in
      let down =
        O.ray_color
          (R.make (V.make (0., 0., 0.)) (V.make (0., 0., -1.)))
          5 (HittableList []) (make_rng ())
      in
      let _, _, bz = vec_to_tup up in
      assert_bool "sky is bluish (z > 0.5)" (bz > 0.5);
      let x, y, z = vec_to_tup down in
      assert_bool "non-negative" (x >= 0. && y >= 0. && z >= 0.) );

    ( "ray_color Lambertian sphere hit" >:: fun _ ->
      let sphere =
        Sphere (V.make (0., 0., 5.), 1., Lambertian (V.make (0.8, 0.8, 0.8)))
      in
      let color =
        O.ray_color
          (R.make (V.make (0., 0., 0.)) (V.make (0., 0., 1.)))
          3 (HittableList [ sphere ]) (make_rng ())
      in
      let x, y, z = vec_to_tup color in
      assert_bool "lambertian non-negative" (x >= 0. && y >= 0. && z >= 0.) );

    ( "ray_color Metal sphere" >:: fun _ ->
      let mk_metal fuzz =
        Sphere (V.make (0., 0., 5.), 1., Metal (V.make (0.8, 0.6, 0.2), fuzz))
      in
      let check fuzz label =
        let color =
          O.ray_color
            (R.make (V.make (0., 0., 0.)) (V.make (0., 0., 1.)))
            3 (HittableList [ mk_metal fuzz ]) (make_rng ())
        in
        let x, y, z = vec_to_tup color in
        assert_bool label (x >= 0. && y >= 0. && z >= 0.)
      in
      check 0.0 "fuzz=0 non-negative";
      check 100.0 "high-fuzz non-negative" );

    ( "ray_color Triangle hit" >:: fun _ ->
      let tri =
        Triangle
          ( V.make (-2., -2., 3.),
            V.make (2., -2., 3.),
            V.make (0., 2., 3.),
            Lambertian (V.make (0.7, 0.7, 0.7)) )
      in
      let color =
        O.ray_color
          (R.make (V.make (0., 0., 0.)) (V.make (0., 0., 1.)))
          2 (HittableList [ tri ]) (make_rng ())
      in
      let x, y, z = vec_to_tup color in
      assert_bool "triangle non-negative" (x >= 0. && y >= 0. && z >= 0.) );

    ( "ray_color TriangularMesh hit" >:: fun _ ->
      let verts =
        [| V.make (-1., -1., 5.); V.make (1., -1., 5.); V.make (0., 1., 5.) |]
      in
      let normals = [| V.make (0., 0., -1.) |] in
      let bbox = { min = V.make (-2., -2., 4.); max = V.make (2., 2., 6.) } in
      let mesh =
        TriangularMesh
          (verts, [ (0, 1, 2) ], normals, bbox, Lambertian (V.make (0.8, 0.8, 0.8)))
      in
      let color =
        O.ray_color
          (R.make (V.make (0., 0., 0.)) (V.make (0., 0., 1.)))
          2 (HittableList [ mesh ]) (make_rng ())
      in
      let x, y, z = vec_to_tup color in
      assert_bool "mesh non-negative" (x >= 0. && y >= 0. && z >= 0.) );

    ( "sphere t2 hit: ray origin inside sphere uses t2 not t1" >:: fun _ ->
      let sphere =
        Sphere (V.make (0., 0., 0.), 2., Lambertian (V.make (0.8, 0.8, 0.8)))
      in
      let ray = R.make (V.make (0., 0., 0.)) (V.make (0., 0., 1.)) in
      match hit sphere ray (Raytracer3110.Interval.make (0.001, 100.)) with
      | None -> assert_failure "expected hit from inside sphere"
      | Some r -> assert_bool "t ≈ 2.0" (abs_float (r.t -. 2.0) < 1e-6) );

    ( "sphere back-face normal is flipped inward" >:: fun _ ->
      let sphere =
        Sphere (V.make (0., 0., 0.), 2., Lambertian (V.make (0.8, 0.8, 0.8)))
      in
      let ray = R.make (V.make (0., 0., 0.)) (V.make (0., 0., 1.)) in
      match hit sphere ray (Raytracer3110.Interval.make (0.001, 100.)) with
      | None -> assert_failure "expected hit"
      | Some r ->
          let _, _, nz = vec_to_tup r.normal in
          assert_bool "back-face normal is -z" (nz < 0.) );

    ( "bbox early-exit: x-slab past y-slab" >:: fun _ ->
      let bbox = { min = V.make (5., 0., 4.); max = V.make (10., 3., 6.) } in
      let mesh =
        TriangularMesh ([||], [], [||], bbox, Lambertian (V.make (0.8, 0.8, 0.8)))
      in
      assert_bool "bbox y-miss → None"
        (Option.is_none
           (hit mesh
              (R.make (V.make (0., 0., 0.)) (V.make (1., 1., 1.)))
              wide)) );

    ( "bbox early-exit: z-slab past xy-slab" >:: fun _ ->
      let bbox = { min = V.make (2., 2., 10.); max = V.make (4., 4., 20.) } in
      let mesh =
        TriangularMesh ([||], [], [||], bbox, Lambertian (V.make (0.8, 0.8, 0.8)))
      in
      assert_bool "bbox z-miss → None"
        (Option.is_none
           (hit mesh
              (R.make (V.make (0., 0., 0.)) (V.make (1., 1., 1.)))
              wide)) );

    ( "bbox hit with negative direction + triangle miss" >:: fun _ ->
      let verts =
        [| V.make (5., 0., 5.); V.make (6., 0., 5.); V.make (5., 1., 5.) |]
      in
      let normals = [| V.make (0., 0., 1.) |] in
      let bbox = { min = V.make (5., 0., 4.); max = V.make (10., 3., 6.) } in
      let mesh =
        TriangularMesh
          (verts, [ (0, 1, 2) ], normals, bbox, Lambertian (V.make (0.8, 0.8, 0.8)))
      in
      assert_bool "swap branches + fold None → None"
        (Option.is_none
           (hit mesh
              (R.make (V.make (12., 5., 8.)) (V.make (-1., -1., -1.)))
              wide)) );
  ]

let camera_extra_tests =
  let mk_yaml s =
    let f = Filename.temp_file "raytest" ".yaml" in
    let oc = open_out f in
    output_string oc s;
    close_out oc;
    f
  in
  let mk_out () = Filename.temp_file "raytest" ".ppm" in

  let tiny_sphere_yaml =
    {|camera:
  image_width: 2
  aspect_ratio: 1.0
  focal_length: 1.0
  center: [0.0, 0.0, 0.0]
  samples_per_pixel: 1
  max_depth: 2
  viewport_height: 2.0
objects:
  - type: sphere
    center: [0.0, 0.0, -3.0]
    radius: 1.0
    material:
      type: lambertian
      albedo: [0.8, 0.8, 0.8]
|}
  in

  let minimal_no_opts_yaml =
    {|camera:
  image_width: 2
  aspect_ratio: 1.0
  focal_length: 1.0
  center: [0.0, 0.0, 0.0]
objects:
  - type: sphere
    center: [0.0, 0.0, -3.0]
    radius: 1.0
|}
  in

  let metal_yaml =
    {|camera:
  image_width: 2
  aspect_ratio: 1.0
  focal_length: 1.0
  center: [0.0, 0.0, 0.0]
  samples_per_pixel: 1
  max_depth: 2
objects:
  - type: sphere
    center: [0.0, 0.0, -3.0]
    radius: 1.0
    material:
      type: metal
      albedo: [0.8, 0.6, 0.2]
      fuzz: 0.1
|}
  in

  let triangle_yaml =
    {|camera:
  image_width: 2
  aspect_ratio: 1.0
  focal_length: 1.0
  center: [0.0, 0.0, 0.0]
  samples_per_pixel: 1
  max_depth: 2
objects:
  - type: triangle
    v0: [0.0, 0.0, -1.0]
    v1: [1.0, 0.0, -1.0]
    v2: [0.0, 1.0, -1.0]
    material:
      type: lambertian
      albedo: [0.7, 0.7, 0.7]
|}
  in

  let string_aspect_yaml =
    {|camera:
  image_width: 2
  aspect_ratio: "1.0"
  focal_length: 1.0
  center: [0.0, 0.0, 0.0]
  samples_per_pixel: 1
  max_depth: 2
objects:
  - type: sphere
    center: [0.0, 0.0, -3.0]
    radius: 1.0
    material:
      type: lambertian
      albedo: [0.8, 0.8, 0.8]
|}
  in

  "camera_extra_tests"
  >::: [
    ( "parse_yaml_scene: camera fields read correctly" >:: fun _ ->
      let cam, _ = C.parse_yaml_scene (mk_yaml tiny_sphere_yaml) in
      assert_equal 2 cam.image_width;
      assert_equal 1 cam.samples_per_pixel;
      assert_equal 2 cam.max_depth );

    ( "parse_yaml_scene: missing optional fields use defaults" >:: fun _ ->
      let cam, world = C.parse_yaml_scene (mk_yaml minimal_no_opts_yaml) in
      assert_equal 10 cam.samples_per_pixel;
      assert_equal 10 cam.max_depth;
      match world with
      | HittableList [ Sphere (_, _, Lambertian _) ] -> assert_bool "ok" true
      | _ -> assert_failure "expected default Lambertian sphere" );

    ( "parse_yaml_scene: metal material parsed" >:: fun _ ->
      let _, world = C.parse_yaml_scene (mk_yaml metal_yaml) in
      match world with
      | HittableList [ Sphere (_, _, Metal (_, fuzz)) ] ->
          assert_bool "fuzz ≈ 0.1" (abs_float (fuzz -. 0.1) < 1e-6)
      | _ -> assert_failure "expected Metal sphere" );

    ( "parse_yaml_scene: triangle parsed and string aspect_ratio accepted" >:: fun _ ->
      let _, world = C.parse_yaml_scene (mk_yaml triangle_yaml) in
      (match world with
       | HittableList [ Triangle _ ] -> assert_bool "ok" true
       | _ -> assert_failure "expected Triangle");
      let cam, _ = C.parse_yaml_scene (mk_yaml string_aspect_yaml) in
      assert_equal 2 cam.image_width );

    ( "parse_yaml_scene: triangular_mesh from OBJ" >:: fun _ ->
      let obj_content =
        "v 0.0 0.0 0.0\nv 1.0 0.0 0.0\nv 0.0 1.0 0.0\nf 1 2 3\n"
      in
      let obj_f = write_temp_obj obj_content in
      let yaml_content =
        Printf.sprintf
          {|camera:
  image_width: 2
  aspect_ratio: 1.0
  focal_length: 1.0
  center: [0.0, 5.0, 5.0]
  samples_per_pixel: 1
  max_depth: 1
objects:
  - type: triangular_mesh
    file_path: %s
    material:
      type: lambertian
      albedo: [0.8, 0.8, 0.8]
|}
          obj_f
      in
      let _, world = C.parse_yaml_scene (mk_yaml yaml_content) in
      match world with
      | HittableList [ TriangularMesh _ ] -> assert_bool "ok" true
      | _ -> assert_failure "expected TriangularMesh" );

    ( "render_from_yaml: output PPM correct" >:: fun _ ->
      let yf = mk_yaml tiny_sphere_yaml and of_ = mk_out () in
      C.render_from_yaml ~use_threading:false yf of_;
      let lines = read_file of_ in
      assert_equal "P3" (List.nth lines 0);
      assert_equal "2 2" (String.trim (List.nth lines 1));
      assert_bool "has pixel data" (List.length lines > 3) );

    ( "render_yaml_with_cam: uses provided camera dimensions" >:: fun _ ->
      let yf = mk_yaml tiny_sphere_yaml and of_ = mk_out () in
      let cam =
        C.make ~image_width:3 ~aspect_ratio:1.0 ~focal_length:1.0
          ~samples_per_pixel:1 ~max_depth:1
          ~center:(V.make (0., 0., 0.))
          ()
      in
      C.render_yaml_with_cam ~use_threading:false cam yf of_;
      let lines = read_file of_ in
      assert_equal "3 3" (String.trim (List.nth lines 1)) );
  ]

let tests =
  "test suite"
  >::: [
         vector_tests;
         order_of_operations_tests;
         ray_tests;
         ppm_tests;
         camera_tests;
         interval_tests;
         hittable_tests;
         vec_extra_tests;
         object_extra_tests;
         camera_extra_tests;
       ]
       @ qcheck_tests

let _ = run_test_tt_main tests

module _ = Raytracer3110.Ui
