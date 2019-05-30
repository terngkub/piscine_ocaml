class dalek =
object

  val name =
    Random.self_init ();
    let c1 = char_of_int ((int_of_char 'A') + Random.int 26) in
    let c2 = char_of_int ((int_of_char 'a') + Random.int 26) in
    let c3 = char_of_int ((int_of_char 'a') + Random.int 26) in
    "Dalek" ^ (String.make 1 c1) ^ (String.make 1 c2) ^ (String.make 1 c3)
  val mutable hp:int = 100
  val mutable shield:bool = true
  
  method set_hp (new_hp:int) =  
    hp <- new_hp

  method set_shield b =
    shield <- b

  method to_string =
    name
    ^ " ("
    ^ "hp: " ^ (string_of_int hp)
    ^ ", sield: " ^ (string_of_bool shield)
    ^ ")"

  method talk =
    Random.self_init ();
    let rand_num = Random.int 4 in
    match rand_num with
    | 0 -> print_endline "Explain! Explain!"
    | 1 -> print_endline "Exterminate! Exterminate!"
    | 2 -> print_endline "I obey!"
    | _ -> print_endline "You are the doctor! You are the enemy of the Daleks!"

  method exterminate (people:People.people) =
    people#die;
    shield <- not shield

  method die =
    print_endline "Emergency Temporal Shift!"

end
