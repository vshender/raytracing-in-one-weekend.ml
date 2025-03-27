type t = {
  point : Point.t;
  normal : Vec3.t;
  t : float;
  front_face : bool;
}

let make ~ray ~point ~outward_normal ~t =
  (* Calculate the hit record normal vector. *)
  let front_face = Vec3.dot ray.Ray.direction outward_normal < 0. in
  let normal = if front_face then outward_normal else Vec3.neg outward_normal in
  { point; normal; t; front_face }
