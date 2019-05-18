let leibniz_pi delta =
  if delta < 0.0 then -1
  else begin
    let ref = 4.0 *. (atan 1.0) in
    let rec leibniz_tail i pi =
      if delta > abs_float (ref -. pi) then i
      else begin
        let neg =
          if (i mod 2) = 1 then -1.0
          else 1.0;
        in
        let float_i = float_of_int i in
        let numerator = 4.0 *. neg in
        let denominator = (2.0 *. float_i) +. 1.0 in
        let new_change = numerator /. denominator in
        let new_pi = pi +. new_change in
        leibniz_tail (i + 1) new_pi
      end
    in
    leibniz_tail 1 4.0
  end


let main () =
  print_int (leibniz_pi (-1.0));
  print_char '\n';
  print_int (leibniz_pi 1.0);
  print_char '\n';
  print_int (leibniz_pi 0.01);
  print_char '\n';
  print_int (leibniz_pi 0.0001);
  print_char '\n'


let () = main ()
