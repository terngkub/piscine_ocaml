let crossover list1 list2 =
  let rec search lst elem =
    match lst with
    | [] -> false
    | h::t ->
    if h = elem then true
    else search t elem
  in
  let rec crossover_tail lst ret =
    match lst with
    | [] -> ret
    | h::t ->
    if (search list2 h) then crossover_tail t (h::ret)
    else crossover_tail t ret
  in
  crossover_tail list1 []

  
let print_int_list list =
  let rec print_tail lst =
    match lst with
    | [] -> ()
    | h::t ->
    print_int h;
    print_char ' ';
    print_tail t
  in
  print_tail list;
  print_char '\n'


let main () =
  print_int_list (crossover [] []);
  print_int_list (crossover [1; 2; 3; 4] [3; 2; 5; 0])


let () = main ()
