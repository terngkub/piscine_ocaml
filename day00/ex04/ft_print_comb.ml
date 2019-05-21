let ft_print_comb () =
  let print i j k =
    print_int i;
    print_int j;
    print_int k;
    if i <> 7 then print_string ", "
  in
  let rec loop i j k =
    if i > 7 then ()
    else if j > 8 then loop (i + 1) (i + 2) (i + 3)
    else if k > 9 then loop i (j + 1) (j + 2)
    else begin
      print i j k; 
      loop i j (k + 1);
    end
  in
  loop 0 1 2;
  print_char '\n'


let main () =
  ft_print_comb ()


let () = main ()
