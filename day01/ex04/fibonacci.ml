let fibonacci n =
  if n < 0 then (-1)
  else begin
    let rec loop i a b =
      if i = n then a
      else loop (i + 1) b (a + b)
    in
    loop 0 0 1
  end


let main () = 
  print_int (fibonacci (-1));
  print_char '\n';
  print_int (fibonacci 0);
  print_char '\n';
  print_int (fibonacci 1);
  print_char '\n';
  print_int (fibonacci 2);
  print_char '\n';
  print_int (fibonacci 3);
  print_char '\n';
  print_int (fibonacci 4);
  print_char '\n';
  print_int (fibonacci 5);
  print_char '\n';
  print_int (fibonacci 6);
  print_char '\n';
  print_int (fibonacci 7);
  print_char '\n';
  print_int (fibonacci 100);
  print_char '\n'


let () = main ()
