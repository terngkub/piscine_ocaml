let ft_string_all func str =
  let rec loop i =
    if i < 0
    then
      true
    else
      begin
        let c = (String.get str i) in
        if (func c) = false then
          false
        else
          loop (i - 1)
      end
  in
  let len = String.length str in
  loop (len - 1)


let is_digit c =
  c >= '0' && c <= '9'


let print_bool bool =
  if bool then
    print_string "true\n"
  else
    print_string "false\n"


let main () =
  print_bool (ft_string_all is_digit "0123456789");
  print_bool (ft_string_all is_digit "012EAS67B9")


let () = main ()