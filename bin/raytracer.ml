open Raytracing_in_one_weekend

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
      let pixel_color = Color.make
        (float i /. (float image_width -. 1.))
        (float j /. (float image_height -. 1.))
        0.0
      in
      Printf.printf "%a\n" Color.output pixel_color
    done
  done;
  Printf.fprintf stderr "\rDone.                 \n"

let () = main ()
