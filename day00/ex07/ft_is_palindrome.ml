let ft_is_palindrome str =
  let rec loop i j =
    if i >= j then true
    else if (String.get str i) <> (String.get str j) then false
    else loop (i + 1) (j - 1)
  in
  loop 0 ((String.length str) - 1)


let print_bool bool =
  if bool then print_endline "true"
  else print_endline "false"


let main () =
  print_bool (ft_is_palindrome "radar");
  print_bool (ft_is_palindrome "madam");
  print_bool (ft_is_palindrome "car");
  print_bool (ft_is_palindrome "")


let () = main ()
