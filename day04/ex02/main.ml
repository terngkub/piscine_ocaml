let print_card card =
  print_string (Card.toString card);
  print_char ' '

let print_card_verbose card =
  print_string (Card.toStringVerbose card);
  print_char ' '

let main () =
  print_endline "allSpades:";
  List.iter print_card Card.allSpades;
  print_endline "\n\nallHearts:";
  List.iter print_card Card.allHearts;
  print_endline "\n\nallDiamonds:";
  List.iter print_card Card.allDiamonds;
  print_endline "\n\nallClubs:";
  List.iter print_card Card.allClubs;
  print_endline "\n\nall:";
  List.iter print_card Card.all;
  print_endline "\n\nall verbose:";
  List.iter print_card_verbose Card.all


let () = main ()
