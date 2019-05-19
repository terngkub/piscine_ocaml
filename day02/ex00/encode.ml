let reverse_list list =
  let rec reverse_list_tail lst ret =
    match lst with
    | [] -> ret
    | head::tail -> reverse_list_tail tail (head::ret)
  in
  reverse_list_tail list []


let encode list =
  let rec encode_tail lst count ret =
    match lst with
    | [] -> ret
    | head::[] -> (count, head)::ret
    | head::next::tail ->
      if head = next then
        encode_tail (next::tail) (count + 1) ret
      else
        encode_tail (next::tail) 1 ((count, head)::ret)
  in
  let rev = encode_tail list 1 [] in
  reverse_list rev


let print_char_tuple_list list =
  let rec print_loop lst =
    match lst with
    | [] -> ()
    | (v, k)::t -> print_int v; print_char k; print_loop t
  in
  print_loop list;
  print_char '\n'


let print_int_tuple_list list =
  let rec print_loop lst =
    match lst with
    | [] -> ()
    | (v, k)::t -> print_int v; print_int k; print_loop t
  in
  print_loop list;
  print_char '\n'


let () =
  print_char_tuple_list (encode []);
  print_char_tuple_list (encode ['a']);
  print_char_tuple_list (encode ['a'; 'b']);
  print_char_tuple_list (encode ['a'; 'a']);
  print_char_tuple_list (encode ['a'; 'a'; 'b'; 'b'; 'c'; 'c'; 'a'; 'a']);
  print_int_tuple_list (encode [1; 2; 2; 1])
