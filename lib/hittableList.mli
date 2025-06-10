class ['a] t : object ('b)
  constraint 'a = #Hittable.t

  (** [add hittable] adds a hittable to the list. *)
  method add : 'a -> 'b

  (** [hit ray interval] returns the closest hit record for the ray. *)
  method hit : Ray.t -> Interval.t -> HitRecord.t option
end
