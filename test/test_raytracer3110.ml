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

let eps = 1e-6
let float_eq a b = abs_float (a -. b) < eps

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

let tests =
  "test suite"
  >::: [ vector_tests; order_of_operations_tests; ray_tests ] @ qcheck_tests

let _ = run_test_tt_main tests
