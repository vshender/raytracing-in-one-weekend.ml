type t = {
  r : float;
  g : float;
  b : float;
}

let make r g b =
  { r; g; b }

let output oc { r; g; b } =
  let scale = 255.999 in
  Printf.fprintf oc "%d %d %d"
    (int_of_float @@ scale *. r)
    (int_of_float @@ scale *. g)
    (int_of_float @@ scale *. b)

let add { r = r1; g = g1; b = b1 } { r = r2; g = g2; b = b2 } =
  { r = r1 +. r2; g = g1 +. g2; b = b1 +. b2 }

let scale { r; g; b } ~by:s =
  { r = r *. s; g = g *. s; b = b *. s }

module Infix = struct
  let ( + ) = add
  let ( * ) c s = scale c ~by:s
  let ( / ) c s = scale c ~by:(1. /. s)
end
