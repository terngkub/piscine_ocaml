let ft_print_alphabet () =
  let rec loop i =
    if i <= (int_of_char 'z') then begin
      print_char (char_of_int i);
      loop (i + 1)
    end
  in
  loop (int_of_char 'a');
  print_char '\n'
  

let main () =
  ft_print_alphabet()


let () = main ()
