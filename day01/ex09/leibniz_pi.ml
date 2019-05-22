let leibniz_pi delta =
  if delta < 0.0 then -1
  else begin
    let ref = 4.0 *. (atan 1.0) in
    let rec loop i pi =
      let abs f =
        if f < 0.0 then (-.f)
        else f
      in
      if abs (ref -. pi) <= delta then i
      else begin
        let numerator =
          if (i mod 2) = 1 then -4.0
          else 4.0;
        in
        let denominator = (2.0 *. (float_of_int i)) +. 1.0 in
        let new_pi = pi +. (numerator /. denominator) in
        loop (i + 1) new_pi
      end
    in
    loop 1 4.0
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
