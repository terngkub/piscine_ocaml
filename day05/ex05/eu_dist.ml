let eu_dist (one:float array) (two: float array) :float =
  let sum = ref 0. in
  for i = 0 to (Array.length one) - 1 do
    sum := !sum +. ((one.(i) -. two.(i)) ** 2.)
  done;
  sqrt !sum


let main () =
  let one = [| 1.; 2.; 3. |] in
  let two = [| 6.; 5.; 4. |] in
  print_float (eu_dist one two);
  print_char '\n'


let () = main ()
