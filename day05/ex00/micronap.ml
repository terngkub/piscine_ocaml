let my_sleep () = Unix.sleep 1

(* TODO handle parsing *)

let main () =
  let argv = Sys.argv in
  let argc = Array.length argv in
  if argc <> 2 then ()
  else
    try
      let duration = int_of_string argv.(1) in
      for i = 0 to duration - 1 do
        my_sleep ()
      done
    with Failure _ -> ()


let () = main ()
