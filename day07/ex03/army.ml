class ['a] army =
object

  val mutable member:('a list) = []


  method add (new_member:'a) =
    member <- new_member :: member


  method delete =
    match member with
    | _ :: tail -> member <- tail
    | [] -> ()

  
  method get_member = member

end