let ft_string_all func str =
  let rec loop i =
    if i < 0 then true
    else if func (String.get str i) then loop (i - 1)
    else false
  in
  loop ((String.length str) - 1)


let is_digit c =
  (c >= '0') && (c <= '9')


let print_bool bool =
  if bool then print_endline "true"
  else print_endline "false"


let main () =
  print_bool (ft_string_all is_digit "0123456789");
  print_bool (ft_string_all is_digit "012EAS67B9")


let () = main ()
