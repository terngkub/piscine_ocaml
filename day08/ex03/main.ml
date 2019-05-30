let () =

  let methane = new Alkane.methane in
  let ethane = new Alkane.ethane in
  let propane = new Alkane.alkane 3 in
  let butane = new Alkane.alkane 4 in
  let pentane = new Alkane.alkane 5 in
  let hexane = new Alkane.alkane 6 in
  let heptane = new Alkane.alkane 7 in
  let octane = new Alkane.octane in
  let nonane = new Alkane.alkane 9 in
  let decane = new Alkane.alkane 10 in
  let undecane = new Alkane.alkane 11 in
  let dodecane = new Alkane.alkane 12 in


  print_endline methane#to_string;
  print_endline ethane#to_string;
  print_endline propane#to_string;
  print_endline butane#to_string;
  print_endline pentane#to_string;
  print_endline hexane#to_string;
  print_endline heptane#to_string;
  print_endline octane#to_string;
  print_endline nonane#to_string;
  print_endline decane#to_string;
  print_endline undecane#to_string;
  print_endline dodecane#to_string;
  print_char '\n';

  print_endline ("methane equals methane:" ^ (string_of_bool (methane#equals methane)));
  print_endline ("methane equals alcohol:" ^ (string_of_bool (methane#equals dodecane)));
  print_endline ("ethane equals ethane:" ^ (string_of_bool (ethane#equals ethane)));
  print_endline ("ethane equals alcohol:" ^ (string_of_bool (ethane#equals dodecane)));
  print_endline ("octane equals octane:" ^ (string_of_bool (octane#equals octane)));
  print_endline ("octane equals alcohol:" ^ (string_of_bool (octane#equals dodecane)));