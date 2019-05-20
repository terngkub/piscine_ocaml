(*
* Type: phosphate
* Type: deoxyribose
*)

type phosphate = string
type deoxyribose = string


(*
* Type: nucleobase
*)

type nucleobase = A | T | C | G | None


let nucleobase_of_char c =
  match c with
  | 'A' -> A
  | 'T' -> T
  | 'C' -> C
  | 'G' -> G
  | _ -> None


let string_of_nucleobase base =
  match base with
  | A -> "A"
  | T -> "T"
  | C -> "C"
  | G -> "G"
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
* Test suite
*)

let main () =
  print_nucleotide (generate_nucleotide 'A');
  print_nucleotide (generate_nucleotide 'T');
  print_nucleotide (generate_nucleotide 'C');
  print_nucleotide (generate_nucleotide 'G');
  print_nucleotide (generate_nucleotide 'X')


let () = main ()