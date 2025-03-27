class ['a] t : object ('b)
  constraint 'a = #Hittable.t

  (** [add hittable] adds a hittable to the list. *)
  method add : 'a -> 'b

  (** [hit ray ~t_min ~t_max] returns the closest hit record for the ray. *)
  method hit : Ray.t -> t_min:float -> t_max:float -> HitRecord.t option
end
