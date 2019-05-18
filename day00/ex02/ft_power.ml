let ft_power base degree =
  let rec ft_power_tail degree ret =
    if degree = 0 then ret
    else ft_power_tail (degree - 1) (ret * base)
  in
  ft_power_tail degree 1


let main () =
  print_int (ft_power 2 4);
  print_char '\n';
  print_int (ft_power 3 0);
  print_char '\n';
  print_int (ft_power 0 5);
  print_char '\n'


let () = main ()
