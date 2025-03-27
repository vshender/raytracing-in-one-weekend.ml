class type t = object
  method hit : Ray.t -> t_min:float -> t_max:float -> HitRecord.t option
end
