let rec contain lst elem =
  match lst with
  | [] -> false
  | head :: tail ->
    if head = elem then true
    else contain tail elem


let crossover list1 list2 =
  let rec loop lst ret =
    match lst with
    | [] -> ret
    | head :: tail ->
      if (contain list2 head) && (not (contain ret head)) then loop tail (head :: ret)
      else loop tail ret
  in
  loop list1 []


(*
* Test suite
*)
  
let print_int_list list =
  let rec loop lst =
    match lst with
    | [] -> ()
    | head :: tail -> print_int head; print_char ' '; loop tail
  in
  loop list;
  print_char '\n'


let main () =
  print_int_list (crossover [] []);
  print_int_list (crossover [1; 2; 3; 4] [3; 2; 5; 0]);
  print_int_list (crossover [1; 2; 3; 3; 4] [3; 2; 5; 0]);
  print_int_list (crossover [1; 2; 3; 4] [3; 2; 5; 3; 0])


let () = main ()
