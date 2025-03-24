open Raytracing_in_one_weekend.Color

open Testables

(* {{{ Test cases for [Color] operations.
   ---------------------------------------------------------------------------
*)

let test_make () =
  let c = make 0.1 0.2 0.3 in
  Alcotest.check color "make creates color with correct components"
    { r = 0.1; g = 0.2; b = 0.3 } c

let test_output () =
  let c = make 0.2 0.4 0.6 in
  let temp_file = Filename.temp_file "color_test" ".txt" in
  let output_string =
    Fun.protect
      ~finally:(fun () -> Sys.remove temp_file)
      begin fun () ->
        Out_channel.with_open_text temp_file
          (fun oc -> output oc c);
        In_channel.with_open_text temp_file
          input_line
      end
  in
  Alcotest.(check string) "output formats color correctly"
    "51 102 153" output_string

let test_add () =
  let c1 = make 0.1 0.2 0.3 in
  let c2 = make 0.4 0.5 0.6 in
  Alcotest.check color "add computes color addition"
    { r = 0.5; g = 0.7; b = 0.9 } (add c1 c2)

let test_scale () =
  let c = make 0.1 0.2 0.3 in
  Alcotest.check color "scale multiplies color by scalar"
    { r = 0.2; g = 0.4; b = 0.6 } (scale c ~by:2.)

(* }}} *)

(* {{{ Test cases for infix operators.
   ---------------------------------------------------------------------------
*)

let test_infix_add () =
  let open Infix in
  let c1 = make 0.1 0.2 0.3 in
  let c2 = make 0.4 0.5 0.6 in
  Alcotest.check color "+ computes color addition"
    { r = 0.5; g = 0.7; b = 0.9 } (c1 + c2)

let test_infix_mul () =
  let open Infix in
  let c = make 0.1 0.2 0.3 in
  Alcotest.check color "* multiplies color by scalar"
    { r = 0.2; g = 0.4; b = 0.6 } (c * 2.)

let test_infix_div () =
  let open Infix in
  let c = make 0.2 0.4 0.6 in
  Alcotest.check color "/ divides color by scalar"
    { r = 0.1; g = 0.2; b = 0.3 } (c / 2.)

(* }}} *)

let () =
  let open Alcotest in
  run "Color" [
    "basic", [
      test_case "make" `Quick test_make;
      test_case "output" `Quick test_output;
      test_case "add" `Quick test_add;
      test_case "scale" `Quick test_scale;
    ];
    "infix", [
      test_case "+" `Quick test_infix_add;
      test_case "*" `Quick test_infix_mul;
      test_case "/" `Quick test_infix_div;
    ];
  ]
