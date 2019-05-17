let ft_print_rev str =
  let rec loop i =
    if i >= 0 then
    begin
      print_char (String.get str i);
      loop (i - 1)
    end
  in
  let len = String.length str in
  loop (len - 1);
  print_char '\n'

let main () =
  ft_print_rev "Hello World !";
  ft_print_rev ""

let () = main ()