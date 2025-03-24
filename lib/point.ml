type t = {
  x : float;
  y : float;
  z : float;
}

let make x y z =
  { x; y; z }

let move { x; y; z } ~by:{ Vec3.x = dx; y = dy; z = dz } =
  { x = x +. dx; y = y +. dy; z = z +. dz }

let sub { x = x1; y = y1; z = z1 } { x = x2; y = y2; z = z2 } =
  Vec3.make (x1 -. x2) (y1 -. y2) (z1 -. z2)

module Infix = struct
  let ( + ) p v = move p ~by:v
  let ( - ) = sub
end
