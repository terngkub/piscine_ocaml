let repeat_x n =
  if n < 0 then "Error"
  else begin
    let rec loop i ret =
      if i = 0 then ret
      else loop (i - 1) (ret ^ "x")
    in
    loop n ""
  end


let main () =
  print_endline (repeat_x (-1));
  print_endline (repeat_x 0);
  print_endline (repeat_x 1);
  print_endline (repeat_x 2);
  print_endline (repeat_x 5)


let () = main ()
