class alkane n =
object (this)

  inherit Molecule.molecule
  (
    match n with
    | 1 -> "Methane"
    | 2 -> "Ethane"
    | 3 -> "Propane"
    | 4 -> "Butane"
    | 5 -> "Pentane"
    | 6 -> "Hexane"
    | 7 -> "Heptane"
    | 8 -> "Octane"
    | 9 -> "Nonane"
    | 10 -> "Decane"
    | 11 -> "Undecane"
    | 12 -> "Dodecane"
    | _ -> "Some alkane"
  )
  (
      let add_atom lst atom n =
        let rec loop i ret =
          if i >= n then ret
          else loop (i + 1) (atom :: ret)
        in
        loop 0 lst
      in
      let hydrogen_list = add_atom [] (new Atom.hydrogen) (n * 2 + 2) in
      let carbon_list = add_atom hydrogen_list (new Atom.carbon) n in
      carbon_list
  )

end

let get_alkane_n alkane =
    match alkane#name with
    | "Methane" -> 1
    | "Ethane" -> 2
    | "Propane" -> 3
    | "Butane" -> 4
    | "Pentane" -> 5
    | "Hexane" -> 6
    | "Heptane" -> 7
    | "Octane" -> 8
    | "Nonane" -> 9
    | "Decane" -> 10
    | "Undecane" -> 11
    | "Dodecane" -> 12
    | _ -> 0


let compare (this:alkane) (that:alkane) =
  let this_n = get_alkane_n this in
  let that_n = get_alkane_n that in
  if this_n > that_n then 1
  else if this_n < that_n then (-1)
  else 0


class methane =
object
  inherit alkane 1
end


class ethane =
object
  inherit alkane 2
end


class octane =
object
  inherit alkane 8
end