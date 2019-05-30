class virtual atom =
object (this)

  method virtual name: string
  method virtual symbol: string
  method virtual atomic_number: int

  method to_string =
    (string_of_int this#atomic_number) ^ " " ^ this#name ^ " (" ^ this#symbol ^ ")"

  method equals (that:atom) :bool =
    (this#name = that#name) && (this#symbol = that#symbol) && (this#atomic_number = that#atomic_number)

end


class hydrogen =
object
  inherit atom
  method name = "Hydrogen"
  method symbol = "H"
  method atomic_number = 1
end


class helium =
object
  inherit atom
  method name = "Helium"
  method symbol = "He"
  method atomic_number = 2
end


class lithium =
object
  inherit atom
  method name = "Lithium"
  method symbol = "Li"
  method atomic_number = 3
end


class beryllium =
object
  inherit atom
  method name = "Beryllium"
  method symbol = "Be"
  method atomic_number = 4
end


class boron =
object
  inherit atom
  method name = "Boron"
  method symbol = "B"
  method atomic_number = 5
end


class carbon =
object
  inherit atom
  method name = "Carbon"
  method symbol = "C"
  method atomic_number = 6
end


class nitrogen =
object
  inherit atom
  method name = "Nitrogen"
  method symbol = "N"
  method atomic_number = 7
end


class oxygen =
object
  inherit atom
  method name = "Oxygen"
  method symbol = "O"
  method atomic_number = 8
end


class fluorine =
object
  inherit atom
  method name = "Fluorine"
  method symbol = "F"
  method atomic_number = 9
end