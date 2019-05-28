let print_joke (jokes:string array) :unit =
  if (Array.length jokes) = 0 then begin
    print_endline "Error: the file is empty"
  end else begin
    Random.self_init ();
    print_endline jokes.(Random.int (Array.length jokes))
  end


let get_jokes (jokes_list:string list) :string array =
  let jokes_arr = Array.make (List.length jokes_list) "" in
  List.iteri (Array.set jokes_arr) jokes_list;
  jokes_arr


let read_input (file_name:string) :string list =
  let ic = open_in file_name in
  let lst = ref [] in
  try 
    while true do
      let str = input_line ic in
      lst := (str :: !lst)
    done;
    close_in ic; !lst
  with
  | End_of_file -> close_in ic; !lst


let main () =
  let argv = Sys.argv in
  let argc = Array.length argv in
  if argc <> 2 then print_endline "Usage: ./a.out input_file"
  else try
    let jokes_list = read_input argv.(1) in
    let jokes_arr = get_jokes jokes_list in
    print_joke jokes_arr
  with
  | Sys_error err -> print_endline ("Error: " ^ err); ()


let () = main ()
