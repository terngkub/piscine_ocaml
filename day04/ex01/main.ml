let rec print_value_list = function
| [] -> print_char '\n'
| head :: [] -> print_string (Value.toString head); print_char '\n'
| head :: tail -> print_string (Value.toString head); print_char ' '; print_value_list tail
  

let rec print_value_list_verbose = function
| [] -> print_char '\n'
| head :: [] -> print_string (Value.toStringVerbose head); print_char '\n'
| head :: tail -> print_string (Value.toStringVerbose head); print_char ' '; print_value_list_verbose tail


let main () =

  let all = Value.all () in
  print_value_list all;
  print_value_list_verbose all


let () = main ()
