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