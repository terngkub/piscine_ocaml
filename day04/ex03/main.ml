let print_with_space str =
  print_string str;
  print_char ' '


let main () =
  let deck = Deck.newDeck () in
  List.iter print_with_space (Deck.toStringListVerbose deck);
  print_endline "\n";
  List.iter print_with_space (Deck.toStringList deck);
  print_endline "\n";

  let (card, new_deck) = Deck.drawCard deck in
  print_string "Draw: ";
  print_endline (Deck.Card.toString card);
  print_string "Left: ";
  List.iter print_with_space (Deck.toStringList new_deck);
  print_endline "\n";

  let (card2, new_deck2) = Deck.drawCard new_deck in
  print_string "Draw: ";
  print_endline (Deck.Card.toString card2);
  print_string "Left: ";
  List.iter print_with_space (Deck.toStringList new_deck2);
  print_char '\n'


let () = main ()
