type t = {
  min : float;
  max : float;
}

let make min max =
  { min; max }

let size { min; max } =
  let size = max -. min in
  if Float.is_nan size || size < 0. then
    0.
  else
    size

let contains { min; max } x =
  min <= x && x <= max

let surrounds { min; max } x =
  min < x && x < max

let empty = make Float.infinity Float.neg_infinity

let universe = make Float.neg_infinity Float.infinity
