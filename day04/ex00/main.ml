let rec print_color_list = function
| [] -> print_char '\n'
| head :: [] -> print_string (Color.toString head); print_char '\n'
| head :: tail -> print_string (Color.toString head); print_char ' '; print_color_list tail
  

let rec print_color_list_verbose = function
| [] -> print_char '\n'
| head :: [] -> print_string (Color.toStringVerbose head); print_char '\n'
| head :: tail -> print_string (Color.toStringVerbose head); print_char ' '; print_color_list_verbose tail


let main () =

  let all = Color.all in
  print_color_list all;
  print_color_list_verbose all

let () = main ()
