let main () =
  (* Image *)
  let image_width = 256
  and image_height = 256
  in

  (* Render *)
  Printf.printf "P3\n%d %d\n255\n" image_width image_height;

  for j = 0 to image_height - 1 do
    Printf.fprintf stderr "\rScanlines remaining: %d %!" (image_height - j);
    for i = 0 to image_width - 1 do
      let r = float i /. (float image_width -. 1.)
      and g = float j /. (float image_height -. 1.)
      and b = 0.0 in

      let ir = int_of_float (255.999 *. r)
      and ig = int_of_float (255.999 *. g)
      and ib = int_of_float (255.999 *. b) in

      Printf.printf "%d %d %d\n" ir ig ib
    done
  done;
  Printf.fprintf stderr "\rDone.                 \n"

let () = main ()
