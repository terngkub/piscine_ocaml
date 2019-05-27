let sum (one:float) (two:float) :float =
  one +. two


let main () =
  print_float (sum 1.0 2.0);
  print_char '\n'


let () = main ()
