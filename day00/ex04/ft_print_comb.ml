let ft_print_comb () =
  let rec loop_one i =
    if i <= 7 then
      begin
        let rec loop_two j =
          if j <= 8 then
          begin
            let rec loop_three k =
              if k <= 9 then
              begin
                print_int i;
                print_int j;
                print_int k;
                if i <> 7 then
                begin
                  print_string ", "
                end;
                loop_three (k + 1)
              end
            in
            loop_three(j + 1);
            loop_two (j + 1)
          end
        in
        loop_two (i + 1);
        loop_one (i + 1)
      end
  in
  loop_one 0;
  print_string "\n"

  let main () =
    ft_print_comb ()

  let () = main ()