open Raytracing_in_one_weekend

(** [ray_color ray world] computes the scene pixel color for the given ray. *)
let ray_color ray world =
  match world#hit ray ~t_min:0. ~t_max:Float.infinity with
  | Some hr ->
    let { HitRecord.normal; _ } = hr in
    Color.Infix.((Color.of_vec3 normal + Color.make 1. 1. 1.) * 0.5)
  | None ->
    let start_value = Color.make 1. 1. 1.
    and end_value = Color.make 0.5 0.7 1. in
    let unit_direction = Vec3.normalize ray.Ray.direction in
    let a = 0.5 *. (unit_direction.y +. 1.) in
    Color.Infix.(start_value * (1. -. a) + end_value * a)

let main () =
  (* Image *)

  let aspect_ratio = 16. /. 9. in
  let image_width = 400 in

  (* Calculate the image height, and ensure that it's at least 1. *)
  let image_height = int_of_float (float image_width /. aspect_ratio) in
  let image_height = if image_height < 1 then 1 else image_height in

  (* World *)
  let world = List.fold_left
    (fun hl -> hl#add)
    (new HittableList.t)
    [
      new Sphere.t ~center:(Point.make 0. 0. (-1.)) ~radius:0.5;
      new Sphere.t ~center:(Point.make 0. (-100.5) (-1.)) ~radius:100.
    ]
  in

  (* Camera *)

  let focal_length = 1. in
  let viewport_height = 2. in
  let viewport_width = viewport_height *. (float image_width /. float image_height) in
  let camera_center = Point.make 0. 0. 0. in

  (* Calculate the vectors across the horizontal and down the vertical
     viewport edges. *)
  let viewport_u = Vec3.make viewport_width 0. 0.
  and viewport_v = Vec3.make 0. (-.viewport_height) 0. in

  (* Calculate the horizontal and vertical delta vectors from pixel to pixel. *)
  let pixel_delta_u = Vec3.Infix.(viewport_u / float image_width)
  and pixel_delta_v = Vec3.Infix.(viewport_v / float image_height) in

  (* Calculate the location of the upper left pixel. *)
  let viewport_upper_left =
    Point.Infix.(
      camera_center
      + Vec3.make 0. 0. (-.focal_length)
      + Vec3.Infix.(-viewport_u / 2.)
      + Vec3.Infix.(-viewport_v / 2.)
    )
  in
  let pixel00_loc =
    Point.Infix.(
      viewport_upper_left
      + Vec3.Infix.((pixel_delta_u + pixel_delta_v) / 2.)
    )
  in

  (* Render *)

  Printf.printf "P3\n%d %d\n255\n" image_width image_height;

  for j = 0 to image_height - 1 do
    Printf.fprintf stderr "\rScanlines remaining: %d %!" (image_height - j);
    for i = 0 to image_width - 1 do
      let pixel_center =
        Point.Infix.(
          pixel00_loc
          + Vec3.scale pixel_delta_u ~by:(float i)
          + Vec3.scale pixel_delta_v ~by:(float j)
        )
      in
      let ray_direction = Point.Infix.(pixel_center - camera_center) in
      let ray = Ray.make ~origin:camera_center ~direction:ray_direction in

      let pixel_color = ray_color ray world in
      Printf.printf "%a\n" Color.output pixel_color
    done
  done;
  Printf.fprintf stderr "\rDone.                 \n"

let () = main ()
