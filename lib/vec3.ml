type t = {
  x : float;
  y : float;
  z : float;
}

let make x y z =
  { x; y; z }

let length_squared { x; y; z } =
  x *. x +. y *. y +. z *. z

let length v =
  sqrt (length_squared v)

let neg { x; y; z } =
  { x = -.x; y = -.y; z = -.z }

let add { x = x1; y = y1; z = z1 } { x = x2; y = y2; z = z2 } =
  { x = x1 +. x2; y = y1 +. y2; z = z1 +. z2 }

let sub { x = x1; y = y1; z = z1 } { x = x2; y = y2; z = z2 } =
  { x = x1 -. x2; y = y1 -. y2; z = z1 -. z2 }

let scale { x; y; z } ~by:s =
  { x = x *. s; y = y *. s; z = z *. s }

let mul { x = x1; y = y1; z = z1 } { x = x2; y = y2; z = z2 } =
  { x = x1 *. x2; y = y1 *. y2; z = z1 *. z2 }

let div v ~by:s =
  scale v ~by:(1. /. s)

let dot { x = x1; y = y1; z = z1 } { x = x2; y = y2; z = z2 } =
  x1 *. x2 +. y1 *. y2 +. z1 *. z2

let cross { x = x1; y = y1; z = z1 } { x = x2; y = y2; z = z2 } =
  { x = y1 *. z2 -. z1 *. y2;
    y = z1 *. x2 -. x1 *. z2;
    z = x1 *. y2 -. y1 *. x2 }

let normalize v =
  let len = length v in
  if len > 0.0 then
    div v ~by:len
  else
    v

module Infix = struct
  let ( ~- ) = neg
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) v s = div v ~by:s
  let ( *. ) = dot
  let ( ** ) = cross
end
