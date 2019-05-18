let ft_print_comb () =
  let print_combination i j k =
    print_int i;
    print_int j;
    print_int k;
    if i <> 7 then print_string ", "
  in
  let rec loop_three i j k =
    if k <= 9 then begin
      print_combination i j k;
      loop_three i j (k + 1)
    end
  in
  let rec loop_two i j =
    if j <= 8 then begin
      loop_three i j (j + 1);
      loop_two i (j + 1)
    end
  in
  let rec loop_one i =
    if i <= 7 then begin
      loop_two i (i + 1);
      loop_one (i + 1)
    end
  in
  loop_one 0;
  print_string "\n"


let main () =
  ft_print_comb ()


let () = main ()
