let eu_dist (one:float array) (two: float array) :float =
  let sum = ref 0. in
  for i = 0 to (Array.length one) - 1 do
    sum := !sum +. ((one.(i) -. two.(i)) ** 2.)
  done;
  sqrt !sum


let convert_line (line:string) :float array * string =
  let lst = List.rev (String.split_on_char ',' line) in
  match lst with
  | str :: float_list -> begin
    let arr = Array.make (List.length float_list) 0. in
    let set_float i float_str =
      arr.(i) <- (float_of_string float_str)
    in
    List.iteri (set_float) (List.rev float_list);
    (arr, str)
  end
  | _ -> invalid_arg "invalid format"


let read_input (file_path:string) :string list =
  let ic = open_in file_path in
  let lst = ref [] in
  let continue = ref true in
  while !continue do
    try 
      let str = input_line ic in
      lst := (str :: !lst)
    with
    | End_of_file -> continue := false
  done;
  close_in ic;
  List.rev !lst


let examples_of_file (file_path:string) :(float array * string) list =
  let lst = read_input file_path in
  List.map convert_line lst


type radar = float array * string


let encode list =
  let rec loop lst count ret =
    match lst with
    | [] -> ret
    | head::[] -> (count, head) :: ret
    | head :: next :: tail ->
      if head = next then
        loop (next :: tail) (count + 1) ret
      else
        loop (next :: tail) 1 ((count, head) :: ret)
  in
  loop list 1 []
  

let k_nn (rl:radar list) (k:int) (r:radar) :string =

  (* distance list *)
  let r_point = fst r in
  let calculate_distance (curr:radar) =
    (eu_dist (fst curr) r_point, (snd curr))
  in
  let distance_list = List.map calculate_distance rl in

  (* sort distance list *)
  let compare_distance one two =
    compare (fst one) (fst two)
  in
  let distance_list = List.sort compare_distance distance_list in

  (* sorted k list *)
  let make_k_list list =
    let rec loop lst i ret =
      match lst with
      | head :: tail when i < k -> loop tail (i + 1) ((snd head) :: ret)
      | _ -> ret
    in loop list 0 []
  in
  let k_list = make_k_list distance_list in
  let k_list = List.sort compare k_list in

  (* count k list *)
  let count_list = encode k_list in
  let compare_count one two =
    compare (fst two) (fst one)
  in
  let count_list = List.sort compare_count count_list in
  match count_list with
  | [] -> ""
  | head :: _ -> snd head


let main () =
  let tuples = examples_of_file "basic.csv" in
  let p1 = k_nn tuples 4 ([|0.15; 0.15; 0.15|], "a") in
  let p2 = k_nn tuples 4 ([|0.29; 0.29; 0.29|], "a") in
  let p3 = k_nn tuples 4 ([|0.31; 0.31; 0.31|], "a") in
  let p4 = k_nn tuples 5 ([|0.3; 0.3; 0.3|], "a") in
  let p5 = k_nn tuples 2 ([|0.6; 0.5; 0.3|], "a") in
  print_endline p1;
  print_endline p2;
  print_endline p3;
  print_endline p4;
  print_endline p5

let () = main ()
