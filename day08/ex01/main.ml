let () =

  let water = new Molecule.water in
  let carbon_dioxide = new Molecule.carbon_dioxide in
  let carbon_monoxide = new Molecule.carbon_monoxide in
  let glucose = new Molecule.glucose in
  let alcohol = new Molecule.alcohol in

  print_endline water#to_string;
  print_endline carbon_dioxide#to_string;
  print_endline carbon_monoxide#to_string;
  print_endline glucose#to_string;
  print_endline alcohol#to_string;
  print_char '\n';

  print_endline ("water equals water:" ^ (string_of_bool (water#equals water)));
  print_endline ("water equals alcohol:" ^ (string_of_bool (water#equals alcohol)));
  print_endline ("carbon_dioxide equals carbon_dioxide:" ^ (string_of_bool (carbon_dioxide#equals carbon_dioxide)));
  print_endline ("carbon_dioxide equals alcohol:" ^ (string_of_bool (carbon_dioxide#equals alcohol)));