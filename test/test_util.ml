open Raytracing_in_one_weekend.Util

open Testables

(* {{{ Test constants.
   --------------------------------------------------------------------------
*)

let test_pi () =
  Alcotest.(check float_eps) "pi should approximately equal to 3.14159265359"
    3.14159265359 pi

(* }}} *)

(* {{{ Test [float_equal] function.
   --------------------------------------------------------------------------
*)

(** Test with equal values. *)
let test_float_equal () =
    Alcotest.(check bool) "float_equal returns true for equal values"
    true (float_equal 1. 1.)

(** Test with nearly equal values. *)
let test_float_equal_nearly_equal () =
  Alcotest.(check bool) "float_equal returns true for nearly equal values"
    true (float_equal 1. (1. +. eps /. 2.))

(** Test with different values. *)
let test_float_equal_different () =
  Alcotest.(check bool) "float_equal returns false for different values"
    false (float_equal 1. 1.1)

(** Test with custom epsilon. *)
let test_float_equal_custom_epsilon () =
  Alcotest.(check bool) "float_equal with custom epsilon works"
    true (float_equal ~epsilon:0.2 1. 1.15)

(* }}} *)

(* {{{ Test [degrees_to_radians] function.
   --------------------------------------------------------------------------
*)

(** Test for 0 degrees. *)
let test_degrees_to_radians_0 () =
  Alcotest.(check float_eps) "0 degrees is 0 radians"
    0. (degrees_to_radians 0.)

(** Test for 90 degrees. *)
let test_degrees_to_radians_90 () =
  Alcotest.(check float_eps) "90 degrees is pi/2 radians"
    (pi /. 2.) (degrees_to_radians 90.)

(** Test for 180 degrees. *)
let test_degrees_to_radians_180 () =
  Alcotest.(check float_eps) "180 degrees is pi radians"
    pi (degrees_to_radians 180.)

(** Test for 270 degrees. *)
let test_degrees_to_radians_270 () =
  Alcotest.(check float_eps) "270 degrees is -pi/2 radians"
    (3. *. pi /. 2.) (degrees_to_radians 270.)

(** Test for 360 degrees. *)
let test_degrees_to_radians_360 () =
  Alcotest.(check float_eps) "360 degrees is 2pi radians"
    (2. *. pi) (degrees_to_radians 360.)

(** Test for negative angle. *)
let test_degrees_to_radians_negative () =
  Alcotest.(check float_eps) "-90 degrees is -pi/2 radians"
    (-. pi /. 2.) (degrees_to_radians (-90.))

(* }}} *)

let () =
  let open Alcotest in
  run "Util" [
    "constants", [
      test_case "pi" `Quick test_pi;
    ];
    "float_equal", [
      test_case "equal values" `Quick test_float_equal;
      test_case "nearly equal values" `Quick test_float_equal_nearly_equal;
      test_case "different values" `Quick test_float_equal_different;
      test_case "custom epsilon" `Quick test_float_equal_custom_epsilon;
    ];
    "degrees_to_radians", [
      test_case "0 degrees" `Quick test_degrees_to_radians_0;
      test_case "90 degrees" `Quick test_degrees_to_radians_90;
      test_case "180 degrees" `Quick test_degrees_to_radians_180;
      test_case "270 degrees" `Quick test_degrees_to_radians_270;
      test_case "360 degrees" `Quick test_degrees_to_radians_360;
      test_case "negative angle" `Quick test_degrees_to_radians_negative;
    ];
  ]
