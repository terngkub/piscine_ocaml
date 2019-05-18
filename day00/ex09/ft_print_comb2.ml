let print_two_digit n =
  if n < 10 then
    print_char '0';
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
  let rec loop_one i =
    if i <= 98 then begin
      let rec loop_two j =
        if j <= 99 then begin
          print_pair i j;
          loop_two (j + 1)
        end
      in
      loop_two (i + 1);
      loop_one (i + 1)
    end
  in
  loop_one 0;
  print_char '\n'


let main () =
  ft_print_comb2 ()


let () = main ()