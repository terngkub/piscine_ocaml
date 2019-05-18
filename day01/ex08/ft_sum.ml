let ft_sum f l u =
  if u < l then nan
  else begin
    let rec loop i ret =
      if i >= u then ret +. (f i)
      else loop (i + 1) (ret +. (f i))
    in
    loop l 0.0
  end


let main () =
  print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10);
  print_char '\n';
  print_float (ft_sum (fun i -> float_of_int (i * i)) 10 1);
  print_char '\n'

let () = main ()
