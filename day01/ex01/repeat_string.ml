let repeat_string ?(str="x") n =
  let rec repeat_string_tail i ret =
    if i = 0 then ret
    else if i < 0 then "Error"
    else repeat_string_tail (i - 1) (ret ^ str)
  in
  repeat_string_tail n ""


let main () =
  print_endline (repeat_string (-1));
  print_endline (repeat_string 0);
  print_endline (repeat_string ~str:"Toto" 1);
  print_endline (repeat_string 2);
  print_endline (repeat_string ~str:"a" 5);
  print_endline (repeat_string ~str:"what" 3)


let () = main ()
