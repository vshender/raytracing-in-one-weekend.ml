open Raytracing_in_one_weekend.Vec3

open Testables

(* {{{ Test cases for [Vec3] operations.
   ---------------------------------------------------------------------------
*)

let test_make () =
  let v = make 1. 2. 3. in
  Alcotest.check vec3 "make creates vector with correct components"
    { x = 1.; y = 2.; z = 3. } v

let test_length_squared () =
  let v = make 2. 3. 4. in
  Alcotest.check float_eps "length_squared computes correct value"
    29. (length_squared v)

let test_length_squared_zero () =
  let v = make 0. 0. 0. in
  Alcotest.check float_eps "length_squared of zero vector is zero"
    0. (length_squared v)

let test_length () =
  let v = make 3. 4. 0. in
  Alcotest.check float_eps "length computes correct value"
    5. (length v)

let test_length_zero () =
  let v = make 0. 0. 0. in
  Alcotest.check float_eps "length of zero vector is zero"
    0. (length v)

let test_neg () =
  let v = make 1. 2. 3. in
  Alcotest.check vec3 "neg negates vector components"
    { x = -1.; y = -2.; z = -3. } (neg v)

let test_neg_zero () =
  let v = make 0. 0. 0. in
  Alcotest.check vec3 "neg of zero vector is zero vector"
    { x = 0.; y = 0.; z = 0. } (neg v)

let test_add () =
  let v1 = make 1. 2. 3.
  and v2 = make 4. 5. 6. in
  Alcotest.check vec3 "add computes vector addition"
    { x = 5.; y = 7.; z = 9. } (add v1 v2)

let test_sub () =
  let v1 = make 5. 7. 9.
  and v2 = make 1. 2. 3. in
  Alcotest.check vec3 "sub computes vector subtraction"
    { x = 4.; y = 5.; z = 6. } (sub v1 v2)

let test_scale () =
  let v = make 1. 2. 3. in
  Alcotest.check vec3 "scale multiplies vector by scalar"
    { x = 2.; y = 4.; z = 6. } (scale v ~by:2.)

let test_mul () =
  let v1 = make 1. 2. 3. in
  let v2 = make 4. 5. 6. in
  Alcotest.check vec3 "mul computes component-wise multiplication"
    { x = 4.; y = 10.; z = 18. } (mul v1 v2)

let test_div () =
  let v = make 2. 4. 6. in
  Alcotest.check vec3 "div divides vector by scalar"
    { x = 1.; y = 2.; z = 3. } (div v ~by:2.)

let test_dot () =
  let v1 = make 1. 2. 3. in
  let v2 = make 4. 5. 6. in
  Alcotest.check float_eps "dot computes dot product"
    32. (dot v1 v2)

let test_cross () =
  let v1 = make 1. 2. 3. in
  let v2 = make 4. 5. 6. in
  Alcotest.check vec3 "cross computes cross product"
    { x = -3.; y = 6.; z = -3. } (cross v1 v2)

let test_normalize () =
  let v = make 1. 2. 3. in
  Alcotest.check vec3 "normalize creates unit vector"
    { x = 0.2672612419124244; y = 0.5345224838248488; z = 0.8017837257372732 } (normalize v)

let test_normalize_zero () =
  let v = make 0. 0. 0. in
  Alcotest.check vec3 "normalize on zero vector returns zero vector"
    { x = 0.; y = 0.; z = 0. } (normalize v)

(* }}} *)

(* {{{ Test cases for infix operators.
   ---------------------------------------------------------------------------
*)

let test_infix_neg () =
  let open Infix in
  let v = make 1. 2. 3. in
  Alcotest.check vec3 "~- negates vector components"
    { x = -1.; y = -2.; z = -3. } (~- v)

let test_infix_add () =
  let open Infix in
  let v1 = make 1. 2. 3. in
  let v2 = make 4. 5. 6. in
  Alcotest.check vec3 "+ computes vector addition"
    { x = 5.; y = 7.; z = 9. } (v1 + v2)

let test_infix_sub () =
  let open Infix in
  let v1 = make 5. 7. 9. in
  let v2 = make 1. 2. 3. in
  Alcotest.check vec3 "- computes vector subtraction"
    { x = 4.; y = 5.; z = 6. } (v1 - v2)

let test_infix_mul () =
  let open Infix in
  let v1 = make 1. 2. 3. in
  let v2 = make 4. 5. 6. in
  Alcotest.check vec3 "* computes component-wise multiplication"
    { x = 4.; y = 10.; z = 18. } (v1 * v2)

let test_infix_div () =
  let open Infix in
  let v = make 2. 4. 6. in
  Alcotest.check vec3 "/ divides vector by scalar"
    { x = 1.; y = 2.; z = 3. } (v / 2.)

let test_infix_dot () =
  let v1 = make 1. 2. 3. in
  let v2 = make 4. 5. 6. in
  let expected = 32. in
  Alcotest.check float_eps "*. computes dot product" expected (Infix.(v1 *. v2))

let test_infix_cross () =
  let open Infix in
  let v1 = make 1. 2. 3. in
  let v2 = make 4. 5. 6. in
  Alcotest.check vec3 "** computes cross product"
    { x = -3.; y = 6.; z = -3. } (v1 ** v2)

(* }}} *)

let () =
  let open Alcotest in
  run "Vec3" [
    "basic", [
      test_case "make" `Quick test_make;
      test_case "length_squared" `Quick test_length_squared;
      test_case "length_squared_zero" `Quick test_length_squared_zero;
      test_case "length" `Quick test_length;
      test_case "length_zero" `Quick test_length_zero;
      test_case "neg" `Quick test_neg;
      test_case "neg_zero" `Quick test_neg_zero;
      test_case "add" `Quick test_add;
      test_case "sub" `Quick test_sub;
      test_case "scale" `Quick test_scale;
      test_case "mul" `Quick test_mul;
      test_case "div" `Quick test_div;
      test_case "dot" `Quick test_dot;
      test_case "cross" `Quick test_cross;
      test_case "normalize" `Quick test_normalize;
      test_case "normalize zero vector" `Quick test_normalize_zero;
    ];
    "infix", [
      test_case "~-" `Quick test_infix_neg;
      test_case "+" `Quick test_infix_add;
      test_case "-" `Quick test_infix_sub;
      test_case "*" `Quick test_infix_mul;
      test_case "/" `Quick test_infix_div;
      test_case "*." `Quick test_infix_dot;
      test_case "**" `Quick test_infix_cross;
    ];
  ]
