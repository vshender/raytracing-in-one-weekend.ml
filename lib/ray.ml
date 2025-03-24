type t = {
  origin : Point.t;
  direction : Vec3.t;
}

let make ~origin ~direction =
  { origin; direction }

let at { origin; direction } t =
  Point.move origin ~by:(Vec3.scale direction ~by:t)
