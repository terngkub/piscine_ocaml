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
    | head::[] -> (head, count) :: ret
    | head :: next :: tail ->
      if head#equals next then
        loop (next :: tail) (count + 1) ret
      else
        loop (next :: tail) 1 ((head, count) :: ret)
  in
  let rev = loop list 1 [] in
  reverse_list rev