class virtual reaction =
object
  method virtual get_start: (Molecule.molecule * int) list
  method virtual get_result: (Molecule.molecule * int) list
  method virtual balance: reaction
  method virtual is_balanced: bool
end


class alkane_combustion (alkane_list:Alkane.alkane list) =
object (this)

  inherit reaction


  method private carbon = 
    let rec loop lst n =
      match lst with
      | [] -> n
      | head :: tail -> loop tail ((Alkane.get_alkane_n head) + n)
    in
    loop alkane_list 0


  method private hydrogen =
    let rec loop lst n =
      match lst with
      | [] -> n
      | head :: tail -> loop tail ((Alkane.get_alkane_n head) * 2 + 2 + n)
    in
    loop alkane_list 0


  method private oxygen = (this#carbon * 2) + (this#hydrogen / 2)


  method get_start =
    if List.length alkane_list = 0 then invalid_arg "reaction list is empty"
    else if not this#is_balanced then failwith "reaction is not balanced"
    else
      let sorted_list = List.sort Alkane.compare alkane_list in
      let reaction_list = Utils.encode sorted_list in
      reaction_list @ [(new Molecule.oxygen, this#oxygen / 2)]


  method get_result =
    if List.length alkane_list = 0 then invalid_arg "reaction list is empty"
    else if not this#is_balanced then failwith "reaction is not balanced"
    else [(new Molecule.carbon_dioxide, this#carbon); (new Molecule.water, this#hydrogen / 2)]


  method private unique_alkane_pair_list =
    let sorted_list = List.sort Alkane.compare alkane_list in
    let reaction_list = Utils.encode sorted_list in
    let rec loop lst ret =
      match lst with
      | [] -> ret
      | (alkane, _) :: tail -> loop tail ((alkane, 1) :: ret)
    in
    List.rev (loop reaction_list [])


  method balance =
    if List.length alkane_list = 0 then invalid_arg "reaction list is empty"
    else begin
      let unique_pair = this#unique_alkane_pair_list in
      let (unique_list, _) = List.split unique_pair in
      let combustion = new alkane_combustion(unique_list) in
      if combustion#is_balanced then combustion
      else begin
        let balanced_list =
          match unique_list with
          | head :: tail when (Alkane.get_alkane_n head) mod 2 = 0 -> head :: head :: tail
          | first :: second :: tail -> first :: second :: second :: tail
          | _ -> failwith "error"
        in
        new alkane_combustion(balanced_list)
      end
    end


  method is_balanced = 
    if List.length alkane_list = 0 then invalid_arg "reaction list is empty"
    else (this#oxygen mod 2 = 0)

end