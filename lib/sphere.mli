(** The type of spheres. *)
class t : center:Point.t -> radius:float -> object
  val center : Point.t
  val radius : float

  (** [hit ray ~t_min ~t_max] returns the hit record for the ray if it
      intersects the sphere, otherwise [None]. *)
  method hit : Ray.t -> t_min:float -> t_max:float -> HitRecord.t option
end
