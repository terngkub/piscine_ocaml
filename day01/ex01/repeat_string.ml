let repeat_string ?(str="x") n =
  if n < 0 then "Error"
  else begin
    let rec loop i ret =
      if i = 0 then ret
      else loop (i - 1) (ret ^ str)
    in
    loop n ""
  end


let main () =
  print_endline (repeat_string (-1));
  print_endline (repeat_string 0);
  print_endline (repeat_string ~str:"Toto" 1);
  print_endline (repeat_string 2);
  print_endline (repeat_string ~str:"a" 5);
  print_endline (repeat_string ~str:"what" 3);
  print_endline (repeat_string ~str:"" 3)


let () = main ()
