let () =
  let w1 :Watchtower.hour = 6 in
  let w2 :Watchtower.hour = 7 in
  let add_them :Watchtower.hour = Watchtower.add w1 w2 in
  let sub_them :Watchtower.hour = Watchtower.sub w1 w2 in
  print_string "add 6 7: ";
  print_int add_them;
  print_string "\nsub 6 7: ";
  print_int sub_them;
  print_char '\n'