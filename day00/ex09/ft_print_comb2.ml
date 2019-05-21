let print_two_digit n =
  if n < 10 then print_char '0';
  print_int n


let print_pair i j =
  print_two_digit i;
  print_char ' ';
  print_two_digit j;
  if i <> 98 then begin
    print_char ',';
    print_char ' '
  end


let ft_print_comb2 () =
  let rec loop i j =
    if i > 98 then ()
    else if j > 99 then loop (i + 1) (i + 2)
    else begin
      print_pair i j;
      loop i (j + 1)
    end
  in
  loop 0 1;
  print_char '\n'


let main () =
  ft_print_comb2 ()


let () = main ()
