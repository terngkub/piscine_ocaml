let rec ft_power base degree =
  if degree = 0
  then 1
  else base * (ft_power base (degree - 1))

let main () =
  print_int (ft_power 2 4);
  print_char '\n';
  print_int (ft_power 3 0);
  print_char '\n';
  print_int (ft_power 0 5);
  print_char '\n'

let () = main ()