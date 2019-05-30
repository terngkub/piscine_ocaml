class people =
object

  val name:string = "Someone"
  val mutable hp:int = 100

  method set_hp new_hp =
    hp <- new_hp

  method to_string =
    name ^ " (hp: " ^ (string_of_int hp) ^ ")"

  method talk =
    print_endline ("I'm " ^ name ^ "! Do you know the Doctor?")
  
  method die =
    print_endline "Aaaarghh!"

  initializer
    print_endline "People is initialized."

end