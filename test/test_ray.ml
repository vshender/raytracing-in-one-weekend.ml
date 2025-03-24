open Raytracing_in_one_weekend
open Ray

open Testables

(* {{{ Test cases for [Ray] operations.
   ---------------------------------------------------------------------------
*)

let test_make () =
  let origin = Point.make 1. 2. 3. in
  let direction = Vec3.make 4. 5. 6. in
  let r = make ~origin ~direction in
  Alcotest.check ray "make creates ray with correct origin and direction"
    { origin = origin; direction = direction } r

let test_at () =
  let origin = Point.make 1. 2. 3. in
  let direction = Vec3.make 4. 5. 6. in
  let r = make ~origin ~direction in

  Alcotest.check point "at t=0 returns the origin point"
    origin (at r 0.);

  Alcotest.check point "at t=1 returns origin + direction"
    (Point.make 5. 7. 9.) (at r 1.);

  Alcotest.check point "at t=0.5 returns origin + 0.5*direction"
    (Point.make 3. 4.5 6.) (at r 0.5);

  Alcotest.check point "at t=-1 returns origin - direction"
    (Point.make (-3.) (-3.) (-3.)) (at r (-1.))

(* }}} *)

let () =
  let open Alcotest in
  run "Ray" [
    "basic", [
      test_case "make" `Quick test_make;
      test_case "at" `Quick test_at;
    ];
  ]
