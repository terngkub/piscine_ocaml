let iter f x n =
  if n < 0 then -1
  else if n = 0 then x
  else begin
    let rec loop i ret =
      if i = 0 then ret
      else loop (i - 1) (f ret)
    in
    loop n x
  end


let main () =
  print_int (iter (fun x -> x * x) 2 (-1));
  print_char '\n';
  print_int (iter (fun x -> x * x) 2 4);
  print_char '\n';
  print_int (iter (fun x -> x * 2) 2 4);
  print_char '\n'


let () = main ()
