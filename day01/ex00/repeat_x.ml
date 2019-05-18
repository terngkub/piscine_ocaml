let repeat_x n =
  let rec tail_repeat_x i str =
    if i = 0 then str
    else if i < 0 then "Error"
    else tail_repeat_x (i - 1) (str ^ "x")
  in
  tail_repeat_x n ""


let main () =
  print_endline (repeat_x (-1));
  print_endline (repeat_x 0);
  print_endline (repeat_x 1);
  print_endline (repeat_x 2);
  print_endline (repeat_x 5)


let () = main ()
