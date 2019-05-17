let ft_print_alphabet () =
  let int_of_a = int_of_char 'a' in
  let int_of_z = int_of_char 'z' in
  let rec loop int_of_current_char =
    if int_of_current_char <= int_of_z
    then
      begin
        print_char (char_of_int int_of_current_char);
        loop (int_of_current_char + 1)
      end
  in
  loop int_of_a;
  print_char '\n'
  
let main () =
  ft_print_alphabet()

let () = main ()