(** The type of a hittable objects. *)
class type t = object
  (** [hit ray ~t_min ~t_max] returns the hit record for the given ray,
      if there is one.

      [t_min] and [t_max] are the minimum and maximum distances to consider
      for the intersection point. *)
  method hit : Ray.t -> t_min:float -> t_max:float -> HitRecord.t option
end
