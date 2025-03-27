class ['a] t = object (_ : 'b)
  constraint 'a = #Hittable.t

  val objects = ([] : 'a list)

  method add : 'a -> 'b =
    fun o -> {< objects = o :: objects >}

  method hit ray ~t_min ~t_max =
    objects
    |> List.fold_left
      (fun (closest, closest_dist) o ->
        match o#hit ray ~t_min ~t_max:closest_dist with
        | None -> (closest, closest_dist)
        | Some hr -> (Some hr, hr.HitRecord.t))
      (None, t_max)
    |> fst
end
