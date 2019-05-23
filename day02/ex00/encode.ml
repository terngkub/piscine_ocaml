let reverse_list list =
  let rec loop lst ret =
    match lst with
    | [] -> ret
    | head :: tail -> loop tail (head :: ret)
  in
  loop list []


let encode list =
  let rec loop lst count ret =
    match lst with
    | [] -> ret
    | head::[] -> (count, head) :: ret
    | head :: next :: tail ->
      if head = next then
        loop (next :: tail) (count + 1) ret
      else
        loop (next :: tail) 1 ((count, head) :: ret)
  in
  let rev = loop list 1 [] in
  reverse_list rev


(*
* Test suite
*)

let print_char_tuple_list list =
  let rec loop lst =
    match lst with
    | [] -> ()
    | (value, key) :: tail -> print_int value; print_char key; loop tail
  in
  loop list;
  print_char '\n'


let print_int_tuple_list list =
  let rec loop lst =
    match lst with
    | [] -> ()
    | (value, key) :: tail -> print_int value; print_int key; loop tail
  in
  loop list;
  print_char '\n'


let main () =
  print_char_tuple_list (encode []);
  print_char_tuple_list (encode ['a']);
  print_char_tuple_list (encode ['a'; 'b']);
  print_char_tuple_list (encode ['a'; 'a']);
  print_char_tuple_list (encode ['a'; 'a'; 'b'; 'b'; 'c'; 'c'; 'a'; 'a']);
  print_int_tuple_list (encode [1; 2; 2; 1])


let () = main ()
