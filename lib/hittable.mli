(** The type of a hittable objects. *)
class type t = object
  (** [hit ray interval] returns the hit record for the given ray, if there is
      one.

      [interval] is the range of distances to consider for the intersection
      point. *)
  method hit : Ray.t -> Interval.t -> HitRecord.t option
end
