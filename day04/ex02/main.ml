let print_card card =
  print_string (Card.toString card);
  print_char ' '


let print_card_verbose card =
  print_string (Card.toStringVerbose card);
  print_char ' '


let print_bool bool =
  match bool with
  | true -> print_string "true"
  | false -> print_string "false"


let main () =
  (* test all *)
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
  List.iter print_card_verbose Card.all;
  print_endline "\n";
  
  let five_heart = Card.newCard T5 Heart in
  let nine_club = Card.newCard T9 Club in
  let nine_heart = Card.newCard T9 Heart in

  (* test value and color *)
  print_endline ("T5H value: " ^ (Card.Value.toString (Card.getValue five_heart)));
  print_endline ("T5H color: " ^ (Card.Color.toString (Card.getColor five_heart)));
  print_endline ("T9C value: " ^ (Card.Value.toString (Card.getValue nine_club)));
  print_endline ("T9C color: " ^ (Card.Color.toString (Card.getColor nine_club)));
  print_endline ("T9H value: " ^ (Card.Value.toString (Card.getValue nine_heart)));
  print_endline ("T9H color: " ^ (Card.Color.toString (Card.getColor nine_heart)));

  (* test compare *)
  print_string "\ncompare T5H with T9C: ";
  print_int (compare five_heart nine_club);
  print_char ' ';
  print_int (Card.compare five_heart nine_club);

  print_string "\ncompare T9C with T9H: ";
  print_int (compare nine_club nine_heart);
  print_char ' ';
  print_int (Card.compare nine_club nine_heart);

  print_string "\ncompare T5H with T5H: ";
  print_int (compare five_heart five_heart);
  print_char ' ';
  print_int (Card.compare five_heart five_heart);
  print_char '\n';

  (* test max and min *)
  print_string "\nmax/min T5H with T9C: ";
  print_card (max five_heart nine_club);
  print_char ' ';
  print_card (min five_heart nine_club);

  print_string "\nmax/min T9H with T9C: ";
  print_card (max nine_heart nine_club);
  print_char ' ';
  print_card (min nine_heart nine_club);

  print_string "\nmax/min T5H with T5H: ";
  print_card (max five_heart five_heart);
  print_char ' ';
  print_card (min five_heart five_heart);

  (* test best *)
  print_string "\n\nbest all: ";
  print_card (Card.best Card.all);
  
  (* isOf *)
  print_string "\n\nisOf T5H Heart: ";
  print_bool (Card.isOf five_heart Heart);
  print_string "\nisOf T9C Heart: ";
  print_bool (Card.isOf nine_club Heart);
  print_string "\nisOf T5H Spade: ";
  print_bool (Card.isSpade five_heart);
  print_string "\nisOf T5H Heart: ";
  print_bool (Card.isHeart five_heart);
  print_string "\nisOf T5H Diamond: ";
  print_bool (Card.isDiamond five_heart);
  print_string "\nisOf T5H Club: ";
  print_bool (Card.isClub five_heart);
  print_char '\n'


let () = main ()
