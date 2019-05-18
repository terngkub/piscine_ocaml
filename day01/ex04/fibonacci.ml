let fibonacci n =
  if n < 0 then (-1)
  else begin
    let rec fibonacci_tail i a b =
      if i = n then a
      else fibonacci_tail (i + 1) b (a + b)
    in
    fibonacci_tail 0 0 1
  end


let main () = 
  print_int (fibonacci 50);
  print_char '\n'


let () = main ()
