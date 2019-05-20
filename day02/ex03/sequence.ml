let encode list =
  let rec loop lst count ret =
    match lst with
    | [] -> ret
    | head :: [] -> (head :: count :: ret)
    | head :: next :: tail ->
      if head = next then
        loop (next :: tail) (count + 1) ret
      else
        loop (next :: tail) 1 (head :: count :: ret)
  in
  loop list 1 []


let int_list_to_str list =
  let rec loop lst ret =
    match lst with
    | [] -> ret
    | head :: tail -> loop tail ((string_of_int head) ^ ret)
  in
  loop list ""


let sequence n =
  if n <= 0 then ""
  else begin
    let rec loop i ret =
      if i <= 0 then ret
      else loop (i - 1) (encode ret)
    in
    int_list_to_str (loop (n - 1) [1])
  end


let main () =
  print_endline (sequence 0);
  print_endline (sequence 1);
  print_endline (sequence 2);
  print_endline (sequence 3);
  print_endline (sequence 4)


let () =
  main ()
