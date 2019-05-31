class virtual molecule (atom_name:string) (atom_list:Atom.atom list) =
object (this)

  method private hill_list =
    let sorted_list = List.sort Atom.compare atom_list in
    Utils.encode sorted_list

  method name = atom_name

  method formula =
    let rec loop lst ret =
      match lst with
      | head :: tail when snd head = 1 -> loop tail (ret ^ (fst head)#symbol)
      | head :: tail when snd head <> 1 -> loop tail (ret ^ (fst head)#symbol ^ (string_of_int (snd head)))
      | _ -> ret
    in
    loop this#hill_list ""

  method to_string =
    this#name ^ " (" ^ this#formula ^ ")"
    
  method equals (that:molecule) =
    (this#name = that#name) && (this#formula = that#formula)

end


class water =
object
  inherit molecule "Water" [new Atom.hydrogen; new Atom.hydrogen; new Atom.oxygen]
end


class carbon_dioxide =
object
  inherit molecule "Carbon dioxide" [new Atom.carbon; new Atom.oxygen; new Atom.oxygen]
end


class carbon_monoxide =
object
  inherit molecule "Carbon monoxide" [new Atom.carbon; new Atom.oxygen]
end

class oxygen =
object
  inherit molecule "Oxygen" [new Atom.oxygen; new Atom.oxygen]
end

class glucose =
object
  inherit molecule "Glucose" [new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen]
end


class alcohol =
object
  inherit molecule "Alcohol" [new Atom.carbon; new Atom.carbon; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.oxygen]
end