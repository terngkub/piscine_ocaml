let () =

  let h = new Atom.hydrogen in
  let he = new Atom.helium in
  let li = new Atom.lithium in
  let be = new Atom.beryllium in
  let b = new Atom.boron in
  let c = new Atom.carbon in
  let n = new Atom.nitrogen in
  let o = new Atom.oxygen in
  let f = new Atom.fluorine in

  print_endline h#to_string;
  print_endline he#to_string;
  print_endline li#to_string;
  print_endline be#to_string;
  print_endline b#to_string;
  print_endline c#to_string;
  print_endline n#to_string;
  print_endline o#to_string;
  print_endline f#to_string;
  print_char '\n';

  print_endline ("H equals H:" ^ (string_of_bool (h#equals h)));
  print_endline ("H equals F:" ^ (string_of_bool (h#equals f)));
  print_endline ("C equals C:" ^ (string_of_bool (c#equals c)));
  print_endline ("C equals F:" ^ (string_of_bool (c#equals f)));
  print_endline ("O equals O:" ^ (string_of_bool (o#equals o)));
  print_endline ("O equals F:" ^ (string_of_bool (o#equals f)));