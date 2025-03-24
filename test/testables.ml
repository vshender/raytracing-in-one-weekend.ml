open Raytracing_in_one_weekend

(** [eps] is the epsilon value for floating point comparisons. *)
let eps = 1e-8

(** [float_equal ?epsilon f1 f2] checks if two floats are equal with a given
    precision. *)
let float_equal ?(epsilon = eps) f1 f2 =
  abs_float (f1 -. f2) < epsilon

(** [float_eps] is a testable for float with a tolerance. *)
let float_eps =
  Alcotest.testable
    (fun ppf f -> Format.fprintf ppf "%f" f)
    float_equal

(** [color] is a testable for [Color.t]. *)
let color =
  Alcotest.testable
    (fun ppf (c : Color.t) ->
      Format.fprintf ppf "(%f, %f, %f)" c.r c.g c.b)
    (fun (c1 : Color.t) (c2 : Color.t) ->
      float_equal c1.r c2.r &&
      float_equal c1.g c2.g &&
      float_equal c1.b c2.b)

(** [point] is a testable for [Point.t]. *)
let point =
  Alcotest.testable
    (fun ppf (p : Point.t) ->
      Format.fprintf ppf "(%f, %f, %f)" p.x p.y p.z)
    (fun (p1 : Point.t) (p2 : Point.t) ->
      float_equal p1.x p2.x &&
      float_equal p1.y p2.y &&
      float_equal p1.z p2.z)

(** [vec3] is a testable for [Vec3.t]. *)
let vec3 =
  Alcotest.testable
    (fun ppf (v : Vec3.t) ->
      Format.fprintf ppf "(%f, %f, %f)" v.x v.y v.z)
    (fun (v1 : Vec3.t) (v2 : Vec3.t) ->
      float_equal v1.x v2.x &&
      float_equal v1.y v2.y &&
      float_equal v1.z v2.z)
