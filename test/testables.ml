open Raytracing_in_one_weekend
open Raytracing_in_one_weekend.Util

(** [point_equal] compares two points with a small epsilon tolerance. *)
let point_equal ?(epsilon = 1e-6) (p1 : Point.t) (p2 : Point.t) =
  float_equal ~epsilon p1.x p2.x &&
  float_equal ~epsilon p1.y p2.y &&
  float_equal ~epsilon p1.z p2.z

(** [vec3_equal] compares two vectors with a small epsilon tolerance. *)
let vec3_equal ?(epsilon = 1e-6) (v1 : Vec3.t) (v2 : Vec3.t) =
  float_equal ~epsilon v1.x v2.x &&
  float_equal ~epsilon v1.y v2.y &&
  float_equal ~epsilon v1.z v2.z

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
    point_equal

(** [vec3] is a testable for [Vec3.t]. *)
let vec3 =
  Alcotest.testable
    (fun ppf (v : Vec3.t) ->
      Format.fprintf ppf "(%f, %f, %f)" v.x v.y v.z)
    vec3_equal

(** [ray] is a testable for [Ray.t]. *)
let ray =
  Alcotest.testable
    (fun ppf (r : Ray.t) ->
      Format.fprintf ppf "origin:(%f, %f, %f), direction:(%f, %f, %f)"
        r.origin.x r.origin.y r.origin.z
        r.direction.x r.direction.y r.direction.z)
    (fun (r1 : Ray.t) (r2 : Ray.t) ->
      point_equal (r1.origin) (r2.origin) &&
      vec3_equal (r1.direction) (r2.direction))
