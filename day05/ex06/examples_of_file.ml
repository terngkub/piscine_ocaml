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


let print_line (arr, str) =
  let print_elem f =
    print_float f;
    print_string ","
  in
  Array.iter print_elem arr;
  print_endline str


let main () =
  let argv = Sys.argv in
  let argc = Array.length argv in
  if argc <> 2 then print_endline "Usage: ./a.out input_file"
  else begin
    let lst = examples_of_file argv.(1) in
    List.iter print_line lst
  end


let () = main ()
