(** The type representing information about a ray hitting an object. *)
type t = {
  point : Point.t;    (** The point of intersection. *)
  normal : Vec3.t;    (** The normal vector at the point of intersection. *)
  t : float;          (** The distance to the intersection point. *)
  front_face : bool;  (** Whether the ray is entering or exiting the object. *)
}

(** [make ~ray ~point ~outward_normal ~t] creates a new hit record with the given
    ray, point, outward normal, and distance.

    The parameter [outward_normal] is assumed to have unit length. *)
val make : ray:Ray.t -> point:Point.t -> outward_normal:Vec3.t -> t:float -> t
