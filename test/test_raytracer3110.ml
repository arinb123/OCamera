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
(*helper functions*)

let eps = 1e-6
let float_eq a b = abs_float (a -. b) < eps

(* make vector *)
let v (a, b, c) = V.make (a, b, c)

(* equality *)
let float_eq a b = abs_float (a -. b) < 1e-6

(* read file*)
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

(*vector test*)
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
         ( "infix dot" >:: fun _ ->
           assert_equal 14.0 (dot (v (1.0, 2.0, 3.0)) (v (1.0, 2.0, 3.0))) );
         ( "infix cross" >:: fun _ ->
           assert_equal
             (v (0.0, 0.0, 1.0))
             (cross (v (1.0, 0.0, 0.0)) (v (0.0, 1.0, 0.0))) );
         ( "vec fst" >:: fun _ ->
           assert_equal (V.vec_fst (v (0.0, 1.0, 2.0))) 0.0 );
         ( "vec snd" >:: fun _ ->
           assert_equal (V.vec_snd (v (0.0, 1.0, 2.0))) 1.0 );
         ( "vec thd" >:: fun _ ->
           assert_equal (V.vec_thd (v (0.0, 1.0, 2.0))) 2.0 );
         ( "vec_min takes smaller x" >:: fun _ ->
           let r = vec_min (V.make (1., 0., 0.)) (V.make (3., 0., 0.)) in
           assert_equal (vec_to_tup r) (1., 0., 0.) );
         ( "vec_min takes smaller y" >:: fun _ ->
           let r = vec_min (V.make (0., 5., 0.)) (V.make (0., 2., 0.)) in
           assert_equal (vec_to_tup r) (0., 2., 0.) );
         ( "vec_min takes smaller z" >:: fun _ ->
           let r = vec_min (V.make (0., 0., 9.)) (V.make (0., 0., 4.)) in
           assert_equal (vec_to_tup r) (0., 0., 4.) );
         ( "vec_min picks minimum per component independently" >:: fun _ ->
           let r = vec_min (V.make (1., 8., 3.)) (V.make (5., 2., 7.)) in
           assert_equal (vec_to_tup r) (1., 2., 3.) );
         ( "vec_min with equal vectors returns same vector" >:: fun _ ->
           let v = V.make (3., 3., 3.) in
           assert_bool "equal" (vec_eq (vec_min v v) v) );
         ( "vec_min with negative components" >:: fun _ ->
           let r = vec_min (V.make (-1., -2., -3.)) (V.make (-4., -1., -2.)) in
           assert_equal (vec_to_tup r) (-4., -2., -3.) );
         ( "vec_max takes larger x" >:: fun _ ->
           let r = vec_max (V.make (1., 0., 0.)) (V.make (3., 0., 0.)) in
           assert_equal (vec_to_tup r) (3., 0., 0.) );
         ( "vec_max takes larger y" >:: fun _ ->
           let r = vec_max (V.make (0., 5., 0.)) (V.make (0., 2., 0.)) in
           assert_equal (vec_to_tup r) (0., 5., 0.) );
         ( "vec_max takes larger z" >:: fun _ ->
           let r = vec_max (V.make (0., 0., 9.)) (V.make (0., 0., 4.)) in
           assert_equal (vec_to_tup r) (0., 0., 9.) );
         ( "vec_max picks maximum per component independently" >:: fun _ ->
           let r = vec_max (V.make (1., 8., 3.)) (V.make (5., 2., 7.)) in
           assert_equal (vec_to_tup r) (5., 8., 7.) );
         ( "vec_max with equal vectors returns same vector" >:: fun _ ->
           let v = V.make (3., 3., 3.) in
           assert_bool "equal" (vec_eq (vec_max v v) v) );
         ( "vec_max with negative components" >:: fun _ ->
           let r = vec_max (V.make (-1., -2., -3.)) (V.make (-4., -1., -2.)) in
           assert_equal (vec_to_tup r) (-1., -1., -2.) );
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
         ( "random_on_hemisphere is a unit vector" >:: fun _ ->
           let normal = V.make (0., 1., 0.) in
           for _ = 1 to 100 do
             let v = random_on_hemisphere rng normal in
             let len = sqrt (length_squared v) in
             assert_bool "len ≈ 1" (abs_float (len -. 1.0) < 1e-10)
           done );
         ( "random_on_hemisphere is in same hemisphere as normal" >:: fun _ ->
           let normal = V.make (0., 1., 0.) in
           for _ = 1 to 100 do
             let v = random_on_hemisphere rng normal in
             assert_bool "dot > 0" (dot v normal > 0.0)
           done );
         ( "random_on_hemisphere works with negative normal" >:: fun _ ->
           let normal = V.make (0., -1., 0.) in
           for _ = 1 to 100 do
             let v = random_on_hemisphere rng normal in
             assert_bool "dot > 0" (dot v normal > 0.0)
           done );
         ( "random_on_hemisphere works with diagonal normal" >:: fun _ ->
           let n = V.make (1., 1., 0.) /^ sqrt 2. in
           for _ = 1 to 100 do
             let v = random_on_hemisphere rng n in
             assert_bool "dot > 0" (dot v n > 0.0)
           done );
       ]

(* order-of-operations tests for custom vector operators *)
let order_of_operations_tests =
  "operator precedence tests"
  >::: [
         ( "scalar multiply binds tighter than vector add" >:: fun _ ->
           let a = v (1.0, 2.0, 3.0) in
           let b = v (4.0, 5.0, 6.0) in
           let expr = a +^ (2.0 *^ b) in
           let expected = a +^ (2.0 *^ b) in
           assert_bool "expected a +^ (2.0 *^ b)" (vec_eq expr expected) );
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
         ( "camera-style viewport formula stays stable when grouped" >:: fun _ ->
           let cam = v (0.0, 0.0, 0.0) in
           let focal = v (0.0, 0.0, 1.0) in
           let u = v (2.0, 0.0, 0.0) in
           let w = v (0.0, -2.0, 0.0) in
           let grouped = cam -^ focal -^ (0.5 *^ u) -^ (0.5 *^ w) in
           let ungrouped = cam -^ focal -^ (0.5 *^ u) -^ (0.5 *^ w) in
           assert_bool
             "camera expression should remain grouped to avoid precedence \
              surprises"
             (vec_eq grouped ungrouped) );
       ]

(*ray tests*)
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

(*ppm tests*)
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
         ( "size of single point is zero" >:: fun _ ->
           assert_equal 0.0 (size (make (3.0, 3.0))) );
         ( "surrounds true for interior point" >:: fun _ ->
           assert_bool "should surround" (surrounds (make (0.0, 10.0)) 5.0) );
         ( "surrounds false for left endpoint" >:: fun _ ->
           assert_bool "should not surround"
             (not (surrounds (make (0.0, 10.0)) 0.0)) );
         ( "surrounds false for right endpoint" >:: fun _ ->
           assert_bool "should not surround"
             (not (surrounds (make (0.0, 10.0)) 10.0)) );
         ( "surrounds false for point outside" >:: fun _ ->
           assert_bool "should not surround"
             (not (surrounds (make (0.0, 10.0)) 11.0)) );
         ( "contains true for interior point" >:: fun _ ->
           assert_bool "should contain" (contains (make (0.0, 10.0)) 5.0) );
         ( "contains true for left endpoint" >:: fun _ ->
           assert_bool "should contain" (contains (make (0.0, 10.0)) 0.0) );
         ( "contains true for right endpoint" >:: fun _ ->
           assert_bool "should contain" (contains (make (0.0, 10.0)) 10.0) );
         ( "contains false for point outside" >:: fun _ ->
           assert_bool "should not contain"
             (not (contains (make (0.0, 10.0)) 11.0)) );
         ( "empty interval has size zero" >:: fun _ ->
           assert_equal 0.0 (size empty) );
         ( "empty min and max are both zero" >:: fun _ ->
           assert_equal 0.0 (min empty);
           assert_equal 0.0 (max empty) );
         ( "universe contains large positive" >:: fun _ ->
           assert_bool "universe should contain" (contains universe 1e308) );
         ( "universe contains large negative" >:: fun _ ->
           assert_bool "universe should contain" (contains universe (-1e308)) );
         ( "universe surrounds zero" >:: fun _ ->
           assert_bool "universe should surround" (surrounds universe 0.0) );
         ( "min returns lower bound" >:: fun _ ->
           assert_equal 2.0 (min (make (2.0, 8.0))) );
         ( "max returns upper bound" >:: fun _ ->
           assert_equal 8.0 (max (make (2.0, 8.0))) );
       ]

(* (* camera tests *) *)
(* let camera_tests = *)
(*   "camera tests" *)
(*   >::: [ *)
(*          ( "image height matches aspect ratio" >:: fun _ -> *)
(*            let expected = *)
(*              int_of_float (float_of_int image_width /. aspect_ratio) *)
(*            in *)
(*            assert_equal expected image_height ); *)
(*          ( "viewport width correct" >:: fun _ -> *)
(*            let expected = *)
(*              viewport_height *)
(*              *. (float_of_int image_width /. float_of_int image_height) *)
(*            in *)
(*            assert_bool "viewport width mismatch" *)
(*              (float_eq expected viewport_width) ); *)
(*        ] *)

(* Randomized QCheck vector tests *)
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

(* Ray random tests*)
let gen_ray = Gen.pair gen_vec gen_vec
let arb_ray = QCheck.make gen_ray

let test_ray_at =
  Test.make ~name:"ray at function" arb_ray (fun (origin, direction) ->
      let r = R.make origin direction in
      let t = Random.float 100. in
      vec_eq (at r t) (add origin (mult t direction)))

let gen_sphere = Gen.pair (Gen.float_range 0.1 100.) gen_vec
let arb_sphere = QCheck.make gen_sphere
let arb_sphere_list = QCheck.list arb_sphere

(* let gen_hittable = QCheck.oneof [ QCheck.map (fun (radius, origin) ->
   O.Sphere (origin, radius)) arb_sphere; ] *)
let test_sphere_hit =
  Test.make ~name:"sphere hit satisfies sphere equation"
    (QCheck.pair arb_ray arb_sphere) (fun ((ray_o, ray_d), (radius, origin)) ->
      let r = R.make ray_o ray_d in
      let s = O.Sphere (origin, radius) in
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

(* Object tests *)

let orig = V.make (0., 0., 0.)
let fwd = V.make (0., 0., 1.)
let ray_fwd = R.make orig fwd
let wide = Raytracer3110.Interval.make (0.001, 1000.)
let sphere_z5 = Sphere (V.make (0., 0., 5.), 1.)
let sphere_neg = Sphere (V.make (0., 0., -5.), 1.)
let tri_v0 = V.make (0., 0., 1.)
let tri_v1 = V.make (1., 0., 1.)
let tri_v2 = V.make (0., 1., 1.)
let triangle = Triangle (tri_v0, tri_v1, tri_v2)
let tri_v0 = V.make (-1., -1., 1.)
let tri_v1 = V.make (1., -1., 1.)
let tri_v2 = V.make (0., 1., 1.)
let wide_triangle = Triangle (tri_v0, tri_v1, tri_v2)

let hittable_tests =
  "hittable_tests"
  >::: [
         ( "make_hit_record stores p" >:: fun _ ->
           let r = make_hit_record orig fwd 1.0 sphere_z5 in
           assert_equal orig r.p );
         ( "make_hit_record stores normal" >:: fun _ ->
           let r = make_hit_record orig fwd 1.0 sphere_z5 in
           assert_equal fwd r.normal );
         ( "make_hit_record stores t" >:: fun _ ->
           let r = make_hit_record orig fwd 3.14 sphere_z5 in
           assert_equal 3.14 r.t );
         ( "make_hit_record stores hit_obj" >:: fun _ ->
           let r = make_hit_record orig fwd 1.0 sphere_z5 in
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
           (* (0.9, 0.9) is outside the right triangle *)
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
           let near = Sphere (V.make (0., 0., 3.), 0.5) in
           let far = Sphere (V.make (0., 0., 8.), 0.5) in
           match hit (HittableList [ far; near ]) ray_fwd wide with
           | None -> assert_failure "expected a hit"
           | Some r ->
               (* near sphere front surface is at t=2.5, far at t=7.5 *)
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
         ( "mesh hit returns Some when ray passes through bounding box and \
            triangle"
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
           let mesh = TriangularMesh (verts, faces, normals, bbox) in
           assert_bool "expected hit" (Option.is_some (hit mesh ray_fwd wide))
         );
         ( "mesh returns closest face when two faces present" >:: fun _ ->
           (* near face at z=3, far face at z=7; ray hits interior of both *)
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
           let mesh = TriangularMesh (verts, faces, normals, bbox) in
           match hit mesh ray_fwd wide with
           | None -> assert_failure "expected a hit"
           | Some r ->
               assert_bool "t ≈ 3.0 (near face)" (abs_float (r.t -. 3.0) < 1e-6)
         );
       ]

let tests =
  "test suite"
  >::: [
         vector_tests;
         order_of_operations_tests;
         ray_tests;
         ppm_tests;
         (* camera_tests; *)
         interval_tests;
         hittable_tests;
       ]
       @ qcheck_tests

let _ = run_test_tt_main tests
module _ = Raytracer3110.Ui
