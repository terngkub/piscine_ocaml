class alkane n =
object

  inherit Molecule.molecule

  method name =
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

  method formula =
    "C" ^ (string_of_int n) ^ "H" ^ (string_of_int ((n * 2) + 2))

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