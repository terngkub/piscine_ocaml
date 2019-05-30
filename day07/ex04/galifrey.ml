class galifrey (people_army:People.people list) (doctor_army:Doctor.doctor list) (dalek_army:Dalek.dalek list) =
object

  val mutable people_list:People.people list = people_army
  val mutable doctor_list:Doctor.doctor list = doctor_army
  val mutable dalek_list:Dalek.dalek list = dalek_army

  method do_time_war =

    let dalek_attack i (dalek:Dalek.dalek) =
      if List.length people_list = 0 then ()
      else if Random.int 2 = 0 then
        print_endline ("Dalek number " ^ (string_of_int i) ^ " fail to exterminate.")
      else begin
        print_string "Dalek exterminate people: ";
        dalek#exterminate (List.hd people_list);
        people_list <- List.tl people_list
      end
    in

    let doctor_attack i (doctor:Doctor.doctor) =
      if List.length dalek_list = 0 then ()
      else if Random.int 2 = 0 then
        print_endline ("Doctor number " ^ (string_of_int i) ^ " fail to exterminate.")
      else begin
        print_string "Doctor attack: ";
        doctor#use_sonic_screwdriver;
        let dalek_head = List.hd dalek_list in
        if dalek_head#get_shield = true then begin
          dalek_head#set_shield false;
          dalek_list <- dalek_head :: List.tl dalek_list;
          print_endline "Destroy dalek shield"
        end else begin
          dalek_list <- List.tl dalek_list;
          dalek_head#die
        end
      end
    in

    let rec loop () =
      print_char '\n';
      List.iteri dalek_attack dalek_list;
      List.iteri doctor_attack doctor_list;
      print_endline ("Number of people: " ^ (string_of_int (List.length people_list)));
      print_endline ("Number of dalek: " ^ (string_of_int (List.length dalek_list)));
      if List.length people_list = 0 then print_endline "\nHumanity is destroyed !!!"
      else if List.length dalek_list = 0 then print_endline "\nHumanity is now safe !!!"
      else loop ()
    in

    Random.self_init ();
    print_endline ("Number of people: " ^ (string_of_int (List.length people_list)));
    print_endline ("Number of doctor: " ^ (string_of_int (List.length doctor_list)));
    print_endline ("Number of dalek: " ^ (string_of_int (List.length dalek_list)));
    loop ()

end