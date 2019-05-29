class ['a] army (member_instance:'a) =
object

  val member:('a list) = [member_instance]

  (* TODO add instance *)
  method add =
    {< member = member >}


  method delete =
    match member with
    | _ :: tail -> {< member = tail >}
    | [] -> {< member = [] >}

end