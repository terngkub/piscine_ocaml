let () =
  print_endline "People army";
  let people_army = new Army.army in
  people_army#add (new People.people);
  people_army#add (new People.people);
  people_army#add (new People.people);
  print_string "Number of people: ";
  print_int (List.length people_army#get_member);

  print_endline "\n\nDeleteing people";
  people_army#delete;
  people_army#delete;
  people_army#delete;
  print_string "Number of people: ";
  print_int (List.length people_army#get_member);

  print_endline "\n________________________";

  print_endline "\nDoctor army";
  let doctor_army = new Army.army in
  doctor_army#add (new Doctor.doctor);
  doctor_army#add (new Doctor.doctor);
  doctor_army#add (new Doctor.doctor);
  print_string "Number of doctor: ";
  print_int (List.length doctor_army#get_member);

  print_endline "\n\nDeleteing doctor";
  doctor_army#delete;
  doctor_army#delete;
  doctor_army#delete;
  print_string "Number of doctor: ";
  print_int (List.length doctor_army#get_member);

  print_endline "\n________________________";

  print_endline "\nDalek army";
  let dalek_army = new Army.army in
  dalek_army#add (new Dalek.dalek);
  dalek_army#add (new Dalek.dalek);
  dalek_army#add (new Dalek.dalek);
  print_string "Number of dalek: ";
  print_int (List.length dalek_army#get_member);

  print_endline "\n\nDeleteing dalek";
  dalek_army#delete;
  dalek_army#delete;
  dalek_army#delete;
  print_string "Number of dalek: ";
  print_int (List.length dalek_army#get_member);
  print_char '\n'