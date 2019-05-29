class people =
object

  val name:string = "Someone"
  val hp:int = 100

  method to_string =
    name ^ " (hp: " ^ (string_of_int hp) ^ ")"

  method talk =
    print_endline ("I'm " ^ name ^ "! Do you know the Doctor?")
  
  method die =
    print_endline "Aaaarghh!"

  initializer
    print_endline "People is initialized."

end