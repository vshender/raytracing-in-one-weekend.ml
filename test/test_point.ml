open Raytracing_in_one_weekend
open Point

open Testables

(* {{{ Test cases for [Point] operations.
   ---------------------------------------------------------------------------
*)

let test_make () =
  let p = make 1. 2. 3. in
  Alcotest.check point "make creates point with correct coordinates"
    { x = 1.; y = 2.; z = 3. } p

let test_move () =
  let p = make 1. 2. 3. in
  let v = Vec3.make 4. 5. 6. in
  Alcotest.check point "move shifts point by vector"
    { x = 5.; y = 7.; z = 9. } (move p ~by:v)

let test_sub () =
  let p1 = make 5. 7. 9. in
  let p2 = make 1. 2. 3. in
  Alcotest.check vec3 "sub computes vector from p2 to p1"
    (Vec3.make 4. 5. 6.) (sub p1 p2)

(** Test for output function requires capturing stdout, which is more complex.
    We'll skip that test for now, as it's a simple formatting function. *)

(* }}} *)

(* {{{ Test cases for infix operators.
   ---------------------------------------------------------------------------
*)

let test_infix_add () =
  let open Infix in
  let p = make 1. 2. 3. in
  let v = Vec3.make 4. 5. 6. in
  Alcotest.check point "+ adds vector to point"
    { x = 5.; y = 7.; z = 9. } (p + v)

let test_infix_sub () =
  let open Infix in
  let p1 = make 5. 7. 9. in
  let p2 = make 1. 2. 3. in
  Alcotest.check vec3 "- subtracts points to get vector"
    { x = 4.; y = 5.; z = 6. } (p1 - p2)

(* }}} *)

let () =
  let open Alcotest in
  run "Point" [
    "basic", [
      test_case "make" `Quick test_make;
      test_case "move" `Quick test_move;
      test_case "sub" `Quick test_sub;
    ];
    "infix", [
      test_case "+" `Quick test_infix_add;
      test_case "-" `Quick test_infix_sub;
    ];
  ]
