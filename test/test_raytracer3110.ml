open OUnit2
module V = Raytracer3110.Vec
module R = Raytracer3110.Ray
open Raytracer3110.Vec
open Raytracer3110.Ray
open Raytracer3110.Ppm

(*helper functions*)
let v (a, b, c) = V.make (a, b, c)

(* let read_file filename = let ic = open_in filename in let rec read_lines acc
   = try let line = input_line *)

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
             (v (0.0, 1.0, 2.0) ^+ v (1.0, 1.0, 1.0)) );
         ( "infix sub" >:: fun _ ->
           assert_equal
             (v (2.0, 1.0, 0.0))
             (v (3.0, 2.0, 1.0) ^- v (1.0, 1.0, 1.0)) );
         ( "infix mult" >:: fun _ ->
           assert_equal (v (2.0, 4.0, 6.0)) (2.0 ^* v (1.0, 2.0, 3.0)) );
         ( "infix dot" >:: fun _ ->
           assert_equal 14.0 (v (1.0, 2.0, 3.0) ^. v (1.0, 2.0, 3.0)) );
         ( "infix cross" >:: fun _ ->
           assert_equal
             (v (0.0, 0.0, 1.0))
             (v (1.0, 0.0, 0.0) ^^ v (0.0, 1.0, 0.0)) );
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

let tests = "test suite" >::: [ vector_tests; ray_tests ]
let _ = run_test_tt_main tests
