let ft_print_alphabet () =
  let rec loop curr =
    if curr <= (int_of_char 'z') then begin
      print_char (char_of_int curr);
      loop (curr + 1)
    end
  in
  loop (int_of_char 'a');
  print_char '\n'
  

let main () =
  ft_print_alphabet()


let () = main ()
