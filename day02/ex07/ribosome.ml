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
  loop (reverse_list list) ""


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
  loop (reverse_list list) ""


(*
* Type: protein
*)

type aminoacid = Stop | Ala | Arg | Asn | Asp | Cys | Gln | Glu | Gly | His | Ile | Leu | Lys | Met | Phe | Pro | Ser | Thr | Trp | Tyr | Val
type protein = aminoacid list

let string_of_aminoacid amino =
  match amino with
  | Stop -> "End of translation"
  | Ala -> "Alanine"
  | Arg -> "Arginine"
  | Asn -> "Asparagine"
  | Asp -> "Aspartique"
  | Cys -> "Cysteine"
  | Gln -> "Glutamine"
  | Glu -> "Glutamique"
  | Gly -> "Glycine"
  | His -> "Histidine"
  | Ile -> "Isoleucine"
  | Leu -> "Leucine"
  | Lys -> "Lysine"
  | Met -> "Methionine"
  | Phe -> "Phenylalanine"
  | Pro -> "Proline"
  | Ser -> "Serine"
  | Thr -> "Threonine"
  | Trp -> "Tryptophane"
  | Tyr -> "Tyrosine"
  | Val -> "Valine"


let string_of_protein list =
  let rec loop lst ret =
    match lst with
    | [] -> ret
    | head :: [] -> (string_of_aminoacid head) ^ ret
    | head :: tail -> loop tail (" " ^ (string_of_aminoacid head) ^ ret)
  in
  loop (reverse_list list) ""


let generate_bases_triplets list =
  let rec loop lst ret =
    match lst with
    | [] -> ret
    | one :: [] -> ret
    | one :: two :: [] -> ret
    | one :: two :: three :: tail -> loop tail ((one, two, three) :: ret)
  in
  loop list []


let which_aminoacid triplet =
  match triplet with
  | (U, A, A) | (U, A, G) | (U, G, A) -> Stop
  | (G, C, A) | (G, C, C) | (G, C, G) | (G, C, U) -> Ala
  | (A, G, A) | (A, G, G) | (C, G, A) | (C, G, C) | (C, G, G) | (C, G, U) -> Arg
  | (A, A, C) | (A, A, U) -> Asn
  | (G, A, C) | (G, A, U) -> Asp
  | (U, G, C) | (U, G, U) -> Cys
  | (C, A, A) | (C, A, G) -> Gln
  | (G, A, A) | (G, A, G) -> Glu
  | (G, G, A) | (G, G, C) | (G, G, G) | (G, G, U) -> Gly
  | (C, A, C) | (C, A, U) -> His
  | (A, U, A) | (A, U, C) | (A, U, U) -> Ile
  | (C, U, A) | (C, U, C) | (C, U, G) | (C, U, U) | (U, U, A) | (U, U, G) -> Leu
  | (A, A, A) | (A, A, G) -> Lys
  | (A, U, G) -> Met
  | (U, U, C) | (U, U, U) -> Phe
  | (C, C, C) | (C, C, A) | (C, C, G) | (C, C, U) -> Pro
  | (U, C, A) | (U, C, C) | (U, C, G) | (U, C, U) | (A, G, U) | (A, G, C) -> Ser
  | (A, C, A) | (A, C, C) | (A, C, G) | (A, C, U) -> Thr
  | (U, G, G) -> Trp
  | (U, A, C) | (U, A, U) -> Tyr
  | (G, U, A) | (G, U, C) | (G, U, G) | (G, U, U) -> Val
  | _ -> Stop


let decode_arn rna =
  let triplet_list = generate_bases_triplets rna in
  let rec loop lst ret =
    match lst with
    | [] -> ret
    | head :: tail -> begin
      let amino = which_aminoacid head in
      if amino = Stop then ret
      else loop tail (amino :: ret)
    end
  in
  loop triplet_list []
  

(*
* Test suite
*)

let main () =
  let helix = generate_helix 16 in
  let rna = generate_rna helix in
  let protein = decode_arn rna in
  print_endline (helix_to_string helix);
  print_endline (rna_to_string rna);
  print_endline (string_of_protein protein)


let () = main ()
