let () =
  let doctor = new Doctor.doctor in
  print_endline doctor#to_string;
  doctor#talk;
  doctor#travel_in_time 10 20;
  print_endline doctor#to_string;
  doctor#use_sonic_screwdriver
