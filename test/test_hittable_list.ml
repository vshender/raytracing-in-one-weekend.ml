open Raytracing_in_one_weekend

open Testables

(** Test for an empty hittable list. *)
let test_empty_list () =
  let list = new HittableList.t in

  (* Ray should miss an empty list. *)
  let ray = Ray.make ~origin:(Point.make 0. 0. 0.) ~direction:(Vec3.make 0. 0. 1.) in
  let result = list#hit ray ~t_min:0. ~t_max:Float.infinity in
  Alcotest.(check bool) "Empty list: Ray should miss" true (Option.is_none result)

(** Test for adding a single object to the list. *)
let test_single_object () =
  let list = new HittableList.t in
  let sphere = new Sphere.t ~center:(Point.make 0. 0. 0.) ~radius:1. in
  let list_with_sphere = list#add sphere in

  (* Ray should hit a list with a sphere. *)
  let ray = Ray.make ~origin:(Point.make 0. 0. 5.) ~direction:(Vec3.make 0. 0. (-1.)) in
  let result = list_with_sphere#hit ray ~t_min:0. ~t_max:Float.infinity in
  Alcotest.(check bool) "List with sphere: Ray should hit" true (Option.is_some result)

(** Test for hitting the closest object. *)
let test_hit_closest () =
  let list = new HittableList.t in

  (* Add two spheres at different positions. *)
  let sphere1 = new Sphere.t ~center:(Point.make 0. 0. 2.) ~radius:1. in
  let sphere2 = new Sphere.t ~center:(Point.make 0. 0. (-1.)) ~radius:1. in
  let list_with_spheres = List.fold_left (fun l -> l#add) list [sphere1; sphere2] in

  (* Ray should hit the first sphere (which is closer) *)
  let ray = Ray.make ~origin:(Point.make 0. 0. 5.) ~direction:(Vec3.make 0. 0. (-1.)) in
  match list_with_spheres#hit ray ~t_min:0. ~t_max:Float.infinity with
  | None -> Alcotest.fail "Ray should hit list with spheres"
  | Some hr ->
      Alcotest.check float_eps "Hit closest object: correct distance" 2. hr.t;
      Alcotest.check point "Hit closest object: correct point"
        (Point.make 0. 0. 3.) hr.point

(** Test for hitting an object with a [t_min] constraint. *)
let test_hit_with_t_min () =
  let list = new HittableList.t in

  (* Add two spheres at different positions. *)
  let sphere1 = new Sphere.t ~center:(Point.make 0. 0. 2.) ~radius:1. in
  let sphere2 = new Sphere.t ~center:(Point.make 0. 0. (-1.)) ~radius:1. in
  let list_with_spheres = List.fold_left (fun l -> l#add) list [sphere1; sphere2] in

  (* Ray should hit the second sphere when [t_min] excludes the first. *)
  let ray = Ray.make ~origin:(Point.make 0. 0. 5.) ~direction:(Vec3.make 0. 0. (-1.)) in
  match list_with_spheres#hit ray ~t_min:4.5 ~t_max:Float.infinity with
  | None -> Alcotest.fail "Ray should hit the second sphere in the list"
  | Some hr ->
      Alcotest.check float_eps "Hit with t_min: correct distance" 5. hr.t;
      Alcotest.check point "Hit with t_min: correct point"
        (Point.make 0. 0. 0.) hr.point

(** Test for hitting an object with a [t_max] constraint. *)
let test_hit_with_t_max () =
  let list = new HittableList.t in

  (* Add two spheres at different positions. *)
  let sphere1 = new Sphere.t ~center:(Point.make 0. 0. (-1.)) ~radius:1. in
  let sphere2 = new Sphere.t ~center:(Point.make 0. 0. 2.) ~radius:1. in
  let list_with_spheres = List.fold_left (fun l -> l#add) list [sphere1; sphere2] in

  (* Ray should miss all spheres when [t_max] is too small. *)
  let ray = Ray.make ~origin:(Point.make 0. 0. 5.) ~direction:(Vec3.make 0. 0. (-1.)) in
  let result = list_with_spheres#hit ray ~t_min:0. ~t_max:1. in
  Alcotest.(check bool) "Hit with t_max too small: ray should miss" true (Option.is_none result)

(** Test for a complex scene with multiple objects. *)
let test_complex_scene () =
  let list = new HittableList.t in

  (* Create a more complex scene with multiple objects. *)
  let sphere1 = new Sphere.t ~center:(Point.make (-2.) 0. 0.) ~radius:1. in
  let sphere2 = new Sphere.t ~center:(Point.make 0. 0. 0.) ~radius:1. in
  let sphere3 = new Sphere.t ~center:(Point.make 2. 0. 0.) ~radius:1. in
  let list_with_spheres = List.fold_left (fun l -> l#add) list [sphere1; sphere2; sphere3] in

  (* Ray hitting the leftmost sphere. *)
  let ray1 = Ray.make ~origin:(Point.make (-5.) 0. 0.) ~direction:(Vec3.make 1. 0. 0.) in
  match list_with_spheres#hit ray1 ~t_min:0. ~t_max:Float.infinity with
  | None -> Alcotest.fail "Ray should hit the leftmost sphere"
  | Some hr ->
      Alcotest.check float_eps "Complex scene -- left ray: correct distance" 2. hr.t;
      Alcotest.check point "Complex scene -- left ray: correct point"
        (Point.make (-3.) 0. 0.) hr.point;

  (* Ray hitting the middle sphere. *)
  let ray2 = Ray.make ~origin:(Point.make 0. 0. 5.) ~direction:(Vec3.make 0. 0. (-1.)) in
  match list_with_spheres#hit ray2 ~t_min:0. ~t_max:Float.infinity with
  | None -> Alcotest.fail "Ray should hit the middle sphere"
  | Some hr ->
      Alcotest.check float_eps "Complex scene -- center ray: correct distance" 4. hr.t;
      Alcotest.check point "Complex scene -- center ray: correct point"
        (Point.make 0. 0. 1.) hr.point;

  (* Ray hitting the rightmost sphere. *)
  let ray3 = Ray.make ~origin:(Point.make 5. 0. 0.) ~direction:(Vec3.make (-1.) 0. 0.) in
  match list_with_spheres#hit ray3 ~t_min:0. ~t_max:Float.infinity with
  | None -> Alcotest.fail "Ray should hit the rightmost sphere"
  | Some hr ->
      Alcotest.check float_eps "Complex scene -- right ray: correct distance" 2. hr.t;
      Alcotest.check point "Complex scene -- right ray: correct point"
        (Point.make 3. 0. 0.) hr.point

(** Test for a ray that misses all objects. *)
let test_ray_miss_all () =
  let list = new HittableList.t in

  (* Add two spheres that won't be hit. *)
  let sphere1 = new Sphere.t ~center:(Point.make (-2.) 0. 0.) ~radius:1. in
  let sphere2 = new Sphere.t ~center:(Point.make 2. 0. 0.) ~radius:1. in
  let list_with_spheres = List.fold_left (fun l -> l#add) list [sphere1; sphere2] in

  (* Ray passing between the spheres. *)
  let ray = Ray.make ~origin:(Point.make 0. 5. 0.) ~direction:(Vec3.make 0. (-1.) 0.) in
  let result = list_with_spheres#hit ray ~t_min:0. ~t_max:Float.infinity in
  Alcotest.(check bool) "Ray should miss all objects" true (Option.is_none result)

let () =
  let open Alcotest in
  run "HittableList" [
    "hit", [
      test_case "empty list" `Quick test_empty_list;
      test_case "single object" `Quick test_single_object;
      test_case "hit closest object" `Quick test_hit_closest;
      test_case "hit with t_min" `Quick test_hit_with_t_min;
      test_case "hit with t_max" `Quick test_hit_with_t_max;
      test_case "complex scene" `Quick test_complex_scene;
      test_case "ray miss all" `Quick test_ray_miss_all;
    ];
  ]
