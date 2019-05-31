let print_pair pair =
  if snd pair <> 1 then begin
    print_int (snd pair);
    print_char '*'
  end;
  print_string (fst pair)#formula;
  print_char ' '

let () =

  let methane = new Alkane.methane in
  let ethane = new Alkane.ethane in
  let propane = new Alkane.alkane 3 in
  let butane = new Alkane.alkane 4 in


  let combustion1 = new Reaction.alkane_combustion [methane] in
  print_endline "methane";
  print_endline ("is_balance: " ^ (string_of_bool combustion1#is_balanced));
  print_string "start:  ";
  (
    try List.iter print_pair combustion1#get_start
    with Failure _ -> print_string "not balanced"
  );
  print_string "\nresult: ";
  (
    try List.iter print_pair combustion1#get_result
    with Failure _ -> print_string "not balanced"
  );
  print_endline "\n";


  let combustion2 = new Reaction.alkane_combustion [ethane] in
  print_endline "ethane";
  print_endline ("is_balance: " ^ (string_of_bool combustion2#is_balanced));
  print_string "start:  ";
  (
    try List.iter print_pair combustion2#get_start
    with Failure _ -> print_string "not balanced"
  );
  print_string "\nresult: ";
  (
    try List.iter print_pair combustion2#get_result
    with Failure _ -> print_string "not balanced"
  );
  print_endline "\n";

  let balanced_combustion2 = combustion2#balance in
  print_endline "balanced -> ethane";
  print_endline ("is_balance: " ^ (string_of_bool balanced_combustion2#is_balanced));
  print_string "start:  ";
  (
    try List.iter print_pair balanced_combustion2#get_start
    with Failure _ -> print_string "not balanced"
  );
  print_string "\nresult: ";
  (
    try List.iter print_pair balanced_combustion2#get_result
    with Failure _ -> print_string "not balanced"
  );
  print_endline "\n";


  let combustion3 = new Reaction.alkane_combustion [ethane; methane] in
  print_endline "ethane, methane";
  print_endline ("is_balance: " ^ (string_of_bool combustion3#is_balanced));
  print_string "start:  ";
  (
    try List.iter print_pair combustion3#get_start
    with Failure _ -> print_string "not balanced"
  );
  print_string "\nresult: ";
  (
    try List.iter print_pair combustion3#get_result
    with Failure _ -> print_string "not balanced"
  );
  print_endline "\n";


  let balanced_combustion3 = combustion3#balance in
  print_endline "balanced -> ethane, methane";
  print_endline ("is_balance: " ^ (string_of_bool balanced_combustion3#is_balanced));
  print_string "start:  ";
  (
    try List.iter print_pair balanced_combustion3#get_start
    with Failure _ -> print_string "not balanced"
  );
  print_string "\nresult: ";
  (
    try List.iter print_pair balanced_combustion3#get_result
    with Failure _ -> print_string "not balanced"
  );
  print_endline "\n";


  let combustion4 = new Reaction.alkane_combustion [ethane; ethane] in
  print_endline "ethane, ethane";
  print_endline ("is_balance: " ^ (string_of_bool combustion4#is_balanced));
  print_string "start:  ";
  (
    try List.iter print_pair combustion4#get_start
    with Failure _ -> print_string "not balanced"
  );
  print_string "\nresult: ";
  (
    try List.iter print_pair combustion4#get_result
    with Failure _ -> print_string "not balanced"
  );
  print_endline "\n";


  let combustion5 = new Reaction.alkane_combustion [ethane; methane; ethane] in
  print_endline "ethane, methane, ethane";
  print_endline ("is_balance: " ^ (string_of_bool combustion5#is_balanced));
  print_string "start:  ";
  (
    try List.iter print_pair combustion5#get_start
    with Failure _ -> print_string "not balanced"
  );
  print_string "\nresult: ";
  (
    try List.iter print_pair combustion5#get_result
    with Failure _ -> print_string "not balanced"
  );
  print_endline "\n";


  let combustion6 = new Reaction.alkane_combustion [propane; ethane; methane; butane; butane; propane] in
  print_endline "propane; ethane; methane; butane; butane; propane";
  print_endline ("is_balance: " ^ (string_of_bool combustion6#is_balanced));
  print_string "start:  ";
  (
    try List.iter print_pair combustion6#get_start
    with Failure _ -> print_string "not balanced"
  );
  print_string "\nresult: ";
  (
    try List.iter print_pair combustion6#get_result
    with Failure _ -> print_string "not balanced"
  );
  print_endline "\n";


  let balanced_combustion6 = combustion6#balance in
  print_endline "balanced -> propane; ethane; methane; butane; butane; propane";
  print_endline ("is_balance: " ^ (string_of_bool balanced_combustion3#is_balanced));
  print_string "start:  ";
  (
    try List.iter print_pair balanced_combustion6#get_start
    with Failure _ -> print_string "not balanced"
  );
  print_string "\nresult: ";
  (
    try List.iter print_pair balanced_combustion6#get_result
    with Failure _ -> print_string "not balanced"
  );
  print_char '\n'
