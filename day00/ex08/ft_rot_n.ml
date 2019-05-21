let ft_rot_n n str =
  let rot_char c =
    if (c >= 'a') && (c <= 'z') then begin
      let int_a = int_of_char 'a' in
      let int_c = int_of_char c in
      char_of_int (int_a + ((int_c - int_a + n) mod 26))
    end else if (c >= 'A') && (c <= 'Z') then begin
      let int_A = int_of_char 'A' in
      let int_c = int_of_char c in
      char_of_int (int_A + ((int_c - int_A + n) mod 26))
    end else c
  in
  String.map rot_char str


let main () =
  print_endline (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz");
  print_endline (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz");
  print_endline (ft_rot_n 42 "0123456789");
  print_endline (ft_rot_n 2 "OI2EAS67B9");
  print_endline (ft_rot_n 0 "Damned !");
  print_endline (ft_rot_n 42 "");
  print_endline (ft_rot_n 1 "NBzlk qnbjr !")


let () = main ()
