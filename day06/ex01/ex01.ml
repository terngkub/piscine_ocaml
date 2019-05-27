module HashedString =
struct

  type t = string


  let equal (one:t) (two:t) :bool = 
    one = two


  (* djb2 hashing *)
  let hash (str:t) :int =
    let rec loop i ret =
      if i >= String.length str then ret
      else loop (i + 1) (ret * 33 + (int_of_char (String.get str i)))
    in
    loop 0 5381

end


module StringHashtbl = Hashtbl.Make (HashedString)


let () =
  let ht = StringHashtbl.create 5 in
  let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
  let pairs = List.map (fun s -> (s, String.length s)) values in
  List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
  StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht