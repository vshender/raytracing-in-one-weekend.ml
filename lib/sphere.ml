class t ~center ~radius = object
  val center = center
  val radius = radius

  method hit ray ~t_min ~t_max =
    let open Ray in

    let oc = Point.Infix.(center - ray.origin) in
    let a = Vec3.length_squared ray.direction
    and h = Vec3.dot ray.direction oc
    and c = Vec3.length_squared oc -. radius *. radius in

    let discriminant = h *. h -. a *. c in
    if discriminant < 0. then
      None
    else
      let sqrtd = sqrt discriminant in

      (* Find the nearest root that lies in the acceptable range. *)
      let root =
        let root = (h -. sqrtd) /. a in
        if t_min <= root && root <= t_max then
          Some root
        else
          let root = (h +. sqrtd) /. a in
          if t_min <= root && root <= t_max then
            Some root
          else
            None
      in

      root |> Option.map @@ fun root ->
        let point = Ray.at ray root in
        let outward_normal = Vec3.Infix.(Point.Infix.(point - center) / radius) in
        HitRecord.make ~ray ~point ~outward_normal ~t:root
end
