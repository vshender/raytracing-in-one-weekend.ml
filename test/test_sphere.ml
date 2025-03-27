open Raytracing_in_one_weekend

open Testables

(** Test for a ray missing a sphere. *)
let test_hit_ray_misses () =
  let sphere = new Sphere.t ~center:(Point.make 0. 0. 0.) ~radius:1. in

  (* Ray passing by the sphere. *)
  let ray1 = Ray.make ~origin:(Point.make 0. 2. 5.) ~direction:(Vec3.make 0. 0. (-1.)) in
  let result = sphere#hit ray1 ~t_min:0. ~t_max:Float.infinity in
  Alcotest.(check bool) "Ray should miss sphere" true (Option.is_none result);

  (* Ray pointing away from the sphere. *)
  let ray2 = Ray.make ~origin:(Point.make 0. 0. 5.) ~direction:(Vec3.make 0. 0. 1.) in
  let result2 = sphere#hit ray2 ~t_min:0. ~t_max:Float.infinity in
  Alcotest.(check bool) "Ray pointing away should miss sphere" true (Option.is_none result2)

(** Test for a ray hitting a sphere. *)
let test_hit_ray_intersects () =
  let sphere = new Sphere.t ~center:(Point.make 0. 0. 0.) ~radius:1. in

  (* Ray passing through the sphere from the left. *)
  let ray1 = Ray.make ~origin:(Point.make (-5.) 0. 0.) ~direction:(Vec3.make 1. 0. 0.) in
  match sphere#hit ray1 ~t_min:0. ~t_max:Float.infinity with
  | None -> Alcotest.fail "Ray should hit sphere"
  | Some hr ->
      Alcotest.check float_eps "Intersection at correct distance" 4. hr.t;
      Alcotest.check point "Intersection at correct point"
        (Point.make (-1.) 0. 0.) hr.point;
      Alcotest.check vec3 "Normal points outward"
        (Vec3.make (-1.) 0. 0.) hr.normal;
      Alcotest.(check bool) "Front face is true" true hr.front_face;

  (* Ray passing through the sphere from the front. *)
  let ray2 = Ray.make ~origin:(Point.make 0. 0. 5.) ~direction:(Vec3.make 0. 0. (-1.)) in
  match sphere#hit ray2 ~t_min:0. ~t_max:Float.infinity with
  | None -> Alcotest.fail "Ray should hit sphere"
  | Some hr ->
      Alcotest.check float_eps "Intersection at correct distance" 4. hr.t;
      Alcotest.check point "Intersection at correct point"
        (Point.make 0. 0. 1.) hr.point;
      Alcotest.check vec3 "Normal points outward"
        (Vec3.make 0. 0. 1.) hr.normal;
      Alcotest.(check bool) "Front face is true" true hr.front_face

(** Test for a ray hitting a sphere from inside. *)
let test_hit_from_inside () =
  let sphere = new Sphere.t ~center:(Point.make 0. 0. 0.) ~radius:1. in

  (* Ray from inside the sphere. *)
  let ray = Ray.make ~origin:(Point.make 0. 0. 0.) ~direction:(Vec3.make 0. 0. 1.) in
  match sphere#hit ray ~t_min:0. ~t_max:Float.infinity with
  | None -> Alcotest.fail "Ray should hit sphere from inside"
  | Some hr ->
      Alcotest.check float_eps "Intersection at correct distance" 1. hr.t;
      Alcotest.check point "Intersection at correct point"
        (Point.make 0. 0. 1.) hr.point;
      Alcotest.check vec3 "Normal points inward"
        (Vec3.make 0. 0. (-1.)) hr.normal;
      Alcotest.(check bool) "Front face is false" false hr.front_face

(** Test for a ray hitting a sphere with a t range. *)
let test_hit_t_range () =
  let sphere = new Sphere.t ~center:(Point.make 0. 0. 0.) ~radius:1. in

  (* Ray that intersects but outside t range. *)
  let ray = Ray.make ~origin:(Point.make 0. 0. 5.) ~direction:(Vec3.make 0. 0. (-1.)) in
  let result1 = sphere#hit ray ~t_min:10. ~t_max:Float.infinity in
  Alcotest.(check bool) "Hit not in t range (t_min too large)" true (Option.is_none result1);

  (* Ray that intersects but outside t range. *)
  let result2 = sphere#hit ray ~t_min:0. ~t_max:3. in
  Alcotest.(check bool) "Hit not in t range (t_max too small)" true (Option.is_none result2);

  (* Should hit when in range. *)
  let result3 = sphere#hit ray ~t_min:3.9 ~t_max:4.1 in
  Alcotest.(check bool) "Hit in t range should not be None" true (Option.is_some result3)

(** Test for a ray tangent to a sphere. *)
let test_tangent_ray () =
  let sphere = new Sphere.t ~center:(Point.make 0. 0. 0.) ~radius:1. in

  (* Ray that is tangent to the sphere -- from 5 units away in z direction. *)
  let ray = Ray.make ~origin:(Point.make 0. 1. 5.) ~direction:(Vec3.make 0. 0. (-1.)) in
  match sphere#hit ray ~t_min:0. ~t_max:Float.infinity with
  | None -> Alcotest.fail "Tangent ray should hit sphere"
  | Some hr ->
      Alcotest.check float_eps "Intersection at correct distance" 5. hr.t;
      Alcotest.check point "Intersection at correct point"
        (Point.make 0. 1. 0.) hr.point;

      (* The normal could be either direction depending on numerical precision. *)
      let normal_length_squared = Vec3.length_squared hr.normal in
      Alcotest.(check bool) "Normal is unit length"
        true (float_equal normal_length_squared 1.);

      (* Check that the normal has a significant y-component (vertical). *)
      let abs_y = abs_float hr.normal.y in
      Alcotest.(check bool) "Normal has significant y component"
        true (abs_y > 0.9);

      (* Check front_face matches the dot product sign. *)
      let dot_product = Vec3.dot ray.direction hr.normal in
      Alcotest.(check bool) "Front face is correct"
        true ((dot_product < 0. && hr.front_face) || (dot_product >= 0. && not hr.front_face))

(** Test for specific discriminant edge cases. *)
let test_discriminant_edge_cases () =
  (* Test with a discriminant of exactly 0 -- tangent case. *)
  let sphere = new Sphere.t ~center:(Point.make 0. 0. 0.) ~radius:1. in
  let ray = Ray.make ~origin:(Point.make 0. 1. (-5.)) ~direction:(Vec3.normalize (Vec3.make 0. 0. 1.)) in
  let result = sphere#hit ray ~t_min:0. ~t_max:Float.infinity in
  Alcotest.(check bool) "Ray with discriminant=0 should hit sphere" true (Option.is_some result);

  (* Test with a very small discriminant -- almost tangent. *)
  let ray2 = Ray.make ~origin:(Point.make 0. 0.999 (-5.)) ~direction:(Vec3.normalize (Vec3.make 0. 0. 1.)) in
  let result2 = sphere#hit ray2 ~t_min:0. ~t_max:Float.infinity in
  Alcotest.(check bool) "Ray with small discriminant should hit sphere" true (Option.is_some result2);

  (* Test with a large negative discriminant -- clearly missing. *)
  let ray3 = Ray.make ~origin:(Point.make 0. 10. (-5.)) ~direction:(Vec3.normalize (Vec3.make 0. 0. 1.)) in
  let result3 = sphere#hit ray3 ~t_min:0. ~t_max:Float.infinity in
  Alcotest.(check bool) "Ray with large negative discriminant should miss" true (Option.is_none result3)

(** Test for spheres of different sizes. *)
let test_different_sphere_sizes () =
  (* Small sphere. *)
  let small_sphere = new Sphere.t ~center:(Point.make 0. 0. 0.) ~radius:0.5 in
  let ray = Ray.make ~origin:(Point.make 0. 0. (-5.)) ~direction:(Vec3.make 0. 0. 1.) in
  match small_sphere#hit ray ~t_min:0. ~t_max:Float.infinity with
  | None -> Alcotest.fail "Ray should hit small sphere"
  | Some hr ->
      Alcotest.check float_eps "Small sphere: intersection at correct distance" 4.5 hr.t;
      Alcotest.check point "Small sphere: intersection at correct point"
        (Point.make 0. 0. (-0.5)) hr.point;

  (* Large sphere. *)
  let large_sphere = new Sphere.t ~center:(Point.make 0. 0. 0.) ~radius:2. in
  match large_sphere#hit ray ~t_min:0. ~t_max:Float.infinity with
  | None -> Alcotest.fail "Ray should hit large sphere"
  | Some hr ->
      Alcotest.check float_eps "Large sphere: intersection at correct distance" 3. hr.t;
      Alcotest.check point "Large sphere: intersection at correct point"
        (Point.make 0. 0. (-2.)) hr.point

let () =
  let open Alcotest in
  run "Sphere" [
    "hit", [
      test_case "ray intersects sphere" `Quick test_hit_ray_intersects;
      test_case "ray misses sphere" `Quick test_hit_ray_misses;
      test_case "ray from inside sphere" `Quick test_hit_from_inside;
      test_case "hit respects t range" `Quick test_hit_t_range;
      test_case "tangent ray" `Quick test_tangent_ray;
      test_case "discriminant edge cases" `Quick test_discriminant_edge_cases;
      test_case "different sphere sizes" `Quick test_different_sphere_sizes;
    ];
  ]
