let eps = 1e-8

let pi = 4. *. atan 1.

let float_equal ?(epsilon = eps) f1 f2 =
  abs_float (f1 -. f2) < epsilon

let degrees_to_radians degrees =
  degrees *. pi /. 180.
