let rotate_n n c =
  char_of_int (((int_of_char c) + n) mod 128)


let rot42 str =
  String.map (rotate_n 42) str


let caesar n str =
  String.map (rotate_n n) str


let xor n str =
  let xor_char c =
    char_of_int ((int_of_char c) lxor n)
  in
  String.map xor_char str


let ft_crypt str func_list =
  let rec loop lst ret =
    match lst with
    | [] -> ret
    | head :: tail -> loop tail (head str)
  in loop func_list ""


let main () =
  print_endline (ft_crypt "abc" [(caesar 200)])


let () = main ()
