let concat_front str list =
  let rec loop lst ret =
    match lst with
    | [] -> ret
    | head :: tail -> loop tail ((str ^ head) :: ret)
  in
  loop list []


let combine_list list1 list2 =
  let rec loop lst ret =
    match lst with
    | [] -> ret
    | head :: tail -> loop tail (head :: ret)
  in
  loop list1 list2


let rec print_str_list list =
  match list with
  | [] -> print_char '\n'
  | head :: [] -> print_string head; print_str_list []
  | head :: tail -> print_string head; print_char ' '; print_str_list tail


let gray n =
  let rec loop i ret =
    if i <= 0 then ret
    else
      let l1 = concat_front "0" ret in
      let l2 = concat_front "1" ret in
      loop (i - 1) (combine_list l1 l2)
  in
  print_str_list (loop n [""])


let main () =
  gray (-1);
  gray 0;
  gray 1;
  gray 2;
  gray 3


let () = main ()
