let () =
  let doctor = new Doctor.doctor in
  let dalek = new Dalek.dalek in

  print_char '\n';
  print_endline doctor#to_string;
  print_endline dalek#to_string;

  print_endline "\nDoctor attack !!!";
  doctor#use_sonic_screwdriver;
  dalek#set_shield false;
  print_endline dalek#to_string;

  print_endline "\nDalek kill doctor side_kick and gain shield !!!";
  dalek#talk;
  dalek#exterminate doctor#get_sidekick;
  doctor#set_sidekick_hp 0;
  print_endline doctor#to_string;
  print_endline dalek#to_string;

  print_endline "\nDoctor go back in time to safe his sidekick !!!";
  doctor#travel_in_time 2019 2018;
  doctor#set_sidekick_hp 100;
  print_endline doctor#to_string;

  print_endline "\nDalek attack doctor";
  dalek#talk;
  doctor#set_hp 30;
  print_endline doctor#to_string;

  print_endline "\nDoctor do a double attack at dalek back.";
  doctor#use_sonic_screwdriver;
  dalek#set_shield false;
  print_endline dalek#to_string;
  doctor#use_sonic_screwdriver;
  dalek#set_hp 0;
  print_endline dalek#to_string;
  dalek#die
