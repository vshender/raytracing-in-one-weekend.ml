(** The type of spheres. *)
class t : center:Point.t -> radius:float -> object
  val center : Point.t
  val radius : float

  (** [hit ray interval] returns the hit record for the ray if it intersects
      the sphere, otherwise [None]. *)
  method hit : Ray.t -> Interval.t -> HitRecord.t option
end
