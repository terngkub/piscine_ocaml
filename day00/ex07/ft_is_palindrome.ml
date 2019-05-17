let ft_is_palindrome str =
  let rec loop i j =
    if i >= j then
      true
    else
      begin
        let ci = String.get str i in
        let cj = String.get str j in
        if ci <> cj then
          false
        else
          loop (i + 1) (j - 1)
      end
  in
  let len = String.length str in
  loop 0 (len - 1)

let print_bool bool =
  if bool then
    print_string "true\n"
  else
    print_string "false\n"

let main () =
  print_bool (ft_is_palindrome "radar");
  print_bool (ft_is_palindrome "madam");
  print_bool (ft_is_palindrome "car");
  print_bool (ft_is_palindrome "")

let () = main ()