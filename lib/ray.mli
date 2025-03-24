(** The type representing a ray with an origin point and a direction vector. *)
type t = {
  origin : Point.t;
  direction : Vec3.t;
}

(** [make origin direction] creates a ray starting at [origin] with direction
    [direction]. *)
val make : origin:Point.t -> direction:Vec3.t -> t

(** [at ray t] computes the position along [ray] at the distance [t] from the
    origin.  That is: origin + t * direction. *)
val at : t -> float -> Point.t
