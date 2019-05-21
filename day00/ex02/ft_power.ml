let ft_power base degree =
  let rec loop i ret =
    if i = 0 then ret
    else loop (i - 1) (ret * base)
  in
  loop degree 1


let main () =
  print_int (ft_power 2 4);
  print_char '\n';
  print_int (ft_power 3 0);
  print_char '\n';
  print_int (ft_power 0 5);
  print_char '\n'


let () = main ()
