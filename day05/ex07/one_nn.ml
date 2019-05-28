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


let one_nn (rl:radar list) (r:radar) :string =
  let r_point = fst r in
  let min_str = ref "" in
  let min_dis = ref 2147483647. in
  let check_min (curr:radar) =
    let dis = eu_dist (fst curr) r_point in
    if dis < !min_dis then begin
      min_dis := dis;
      min_str := snd curr
    end
  in
  List.iter check_min rl;
  !min_str
  

let main () =
  print_endline "Basic Test:";
  let lst = examples_of_file "basic.csv" in
  let str1 = one_nn lst ([|0.1; 0.1; 0.1|], "") in
  print_endline str1;
  let str2 = one_nn lst ([|0.4; 0.4; 0.4|], "") in
  print_endline str2;
  let str3 = one_nn lst ([|0.8; 0.8; 0.8|], "") in
  print_endline str3


let () = main ()
