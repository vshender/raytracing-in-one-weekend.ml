class type t = object
  method hit : Ray.t -> Interval.t -> HitRecord.t option
end
