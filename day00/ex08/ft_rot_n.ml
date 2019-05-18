let ft_rot_n n str =
  let rot_char c =
    if (c >= 'a' && c < 'z') || (c >= 'A' && c < 'Z') then
      char_of_int (int_of_char(c) + 1)
    else if (c == 'z') || (c == 'Z') then
        char_of_int (int_of_char(c) - 25)
    else c
  in
  let rec loop i =
    if i <= 0 then str
    else String.map rot_char (loop (i - 1))
  in
  loop n


let main () =
  print_endline (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz");
  print_endline (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz");
  print_endline (ft_rot_n 42 "0123456789");
  print_endline (ft_rot_n 2 "OI2EAS67B9");
  print_endline (ft_rot_n 0 "Damned !");
  print_endline (ft_rot_n 42 "");
  print_endline (ft_rot_n 1 "NBzlk qnbjr !")


let () = main ()
