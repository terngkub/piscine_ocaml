let rec ft_countdown num =
  if num <= 0 then begin
      print_int 0;
      print_char '\n'
  end else begin
      print_int num;
      print_char '\n';
      ft_countdown (num - 1)
  end


let main () =
  ft_countdown 3;
  ft_countdown 0;
  ft_countdown (-1)


let () = main ()
