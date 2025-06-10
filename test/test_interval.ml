open Raytracing_in_one_weekend

open Testables

let test_make () =
  let interval = Interval.make 1. 2. in
  Alcotest.(check float_eps) "make: min" 1. interval.min;
  Alcotest.(check float_eps) "make: max" 2. interval.max

let test_size () =
  let interval = Interval.make 1. 5. in
  Alcotest.(check float_eps) "size" 4. (Interval.size interval)

let test_contains () =
  let interval = Interval.make 1. 2. in
  Alcotest.(check bool) "contains: lower bound" true (Interval.contains interval 1.);
  Alcotest.(check bool) "contains: upper bound" true (Interval.contains interval 2.);
  Alcotest.(check bool) "contains: within" true (Interval.contains interval 1.5);
  Alcotest.(check bool) "contains: below" false (Interval.contains interval 0.5);
  Alcotest.(check bool) "contains: above" false (Interval.contains interval 2.5)

let test_surrounds () =
  let interval = Interval.make 1. 2. in
  Alcotest.(check bool) "surrounds: lower bound" false (Interval.surrounds interval 1.);
  Alcotest.(check bool) "surrounds: upper bound" false (Interval.surrounds interval 2.);
  Alcotest.(check bool) "surrounds: within" true (Interval.surrounds interval 1.5);
  Alcotest.(check bool) "surrounds: below" false (Interval.surrounds interval 0.5);
  Alcotest.(check bool) "surrounds: above" false (Interval.surrounds interval 2.5)

let test_empty () =
  let interval = Interval.empty in
  Alcotest.(check float_eps) "empty: size" 0. (Interval.size interval)

let test_universe () =
  let interval = Interval.universe in
  Alcotest.(check bool) "universe: size" (Interval.size interval = Float.infinity) true

let () =
  let open Alcotest in
  run "Interval" [
    "interval", [
      test_case "make" `Quick test_make;
      test_case "size" `Quick test_size;
      test_case "contains" `Quick test_contains;
      test_case "surrounds" `Quick test_surrounds;
      test_case "empty" `Quick test_empty;
      test_case "universe" `Quick test_universe;
    ];
  ]