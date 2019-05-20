(*
* Utils
*)

let reverse_list list =
  let rec loop lst ret =
    match lst with
    | [] -> ret
    | head :: tail -> loop tail (head :: ret)
  in
  loop list []


(*
* Type: phosphate
* Type: deoxyribose
*)

type phosphate = string
type deoxyribose = string


(*
* Type: nucleobase
*)

type nucleobase = A | T | C | G | U | None


let nucleobase_of_char c =
  match c with
  | 'A' -> A
  | 'T' -> T
  | 'C' -> C
  | 'G' -> G
  | 'U' -> U
  | _ -> None


let string_of_nucleobase base =
  match base with
  | A -> "A"
  | T -> "T"
  | C -> "C"
  | G -> "G"
  | U -> "U"
  | None -> "None"


(*
* Type: nucleotide
*)

type nucleotide = {
  phosphate : phosphate;
  deoxyribose : deoxyribose;
  nucleobase : nucleobase
}


let generate_nucleotide c =
  {
    phosphate = "phosphate";
    deoxyribose = "deoxyribose";
    nucleobase = nucleobase_of_char c
  }


let print_nucleotide nuc =
  print_endline ("phosephat: " ^ nuc.phosphate);
  print_endline ("deoxyribose: " ^ nuc.deoxyribose);
  print_endline ("nucleobase: " ^ (string_of_nucleobase nuc.nucleobase))


(*
* Type: helix
*)

type helix = nucleotide list


let generate_helix n =
  let rec loop i ret =
    if i <= 0 then ret
    else begin
      let rand = Random.int 4 in
      let nuc =
        match rand with
        | 0 -> generate_nucleotide 'A'
        | 1 -> generate_nucleotide 'T'
        | 2 -> generate_nucleotide 'C'
        | _ -> generate_nucleotide 'G'
      in
      loop (i - 1) (nuc :: ret)
    end
  in
  loop n []


let helix_to_string list =
  let rec loop lst ret =
    match lst with
    | [] -> ret
    | head :: tail -> loop tail ((string_of_nucleobase head.nucleobase) ^ ret)
  in
  loop list ""


let complementary_helix list =
  let complementary_nucleotide base =
    let complementary_nucleobase = 
      match base with
      | A -> 'T'
      | T -> 'A'
      | C -> 'G'
      | G -> 'C'
      | _ -> 'X'
    in
    generate_nucleotide complementary_nucleobase
  in
  let rec loop lst ret =
    match lst with
    | [] -> ret
    | head :: tail -> loop tail ((complementary_nucleotide head.nucleobase) :: ret)
  in
  let comp_helix = loop list [] in
  reverse_list comp_helix


(*
* Type: rna
*)

type rna = nucleobase list


let generate_rna list =
  let get_rna_nucleobase base =
    let rna_nucleobase = 
      match base with
      | A -> 'U'
      | T -> 'A'
      | C -> 'G'
      | G -> 'C'
      | _ -> 'X'
    in
    nucleobase_of_char rna_nucleobase
  in
  let rec loop lst ret =
    match lst with
    | [] -> ret
    | head :: tail -> loop tail ((get_rna_nucleobase head.nucleobase) :: ret)
  in
  let rna = loop list [] in
  reverse_list rna


let rna_to_string list =
  let rec loop lst ret =
    match lst with
    | [] -> ret
    | head :: tail -> loop tail ((string_of_nucleobase head) ^ ret)
  in
  loop list ""

(*
* Test suite
*)

let main () =
  let helix = generate_helix 16 in
  let rna = generate_rna helix in
  print_endline (helix_to_string helix);
  print_endline (rna_to_string rna)


let () = main ()
