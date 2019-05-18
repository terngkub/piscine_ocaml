let converges f x n =
  if n = 0 then false
  else begin
    let rec loop i ret =
      if ret = (f ret) then true
      else if i = 0 then false
      else loop (i - 1) (f ret)
    in
    loop n x
  end


let main () =
  let print_bool bool =
    if bool then print_endline "true"
    else print_endline "false"
  in
  print_bool (converges (( * ) 2) 2 5);
  print_bool (converges (fun x -> x / 2) 2 3);
  print_bool (converges (fun x -> x / 2) 2 2)


let () = main ()
