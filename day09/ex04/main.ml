let print_set (set:'a Set.t) =
  let rec loop lst =
    match lst with
    | [] -> print_char '\n'
    | head :: tail -> print_int head; print_char ' '; loop tail
  in
  loop set


let () =
  let t1 = Set.return 1 in
  print_string "t1: ";
  print_set t1;

  let t2 :'a Set.t = [1; 2; 3; 4] in
  print_string "t2: ";
  print_set t2;

  let t3 :'a Set.t = [3; 4; 5; 6] in
  print_string "t3: ";
  print_set t3;

  print_string "t2 bind (+ 1): ";
  print_set (Set.bind t2 (fun x -> [x + 1]));

  print_string "t2 union: ";
  print_set (Set.union t2 t3);

  print_string "t2 inter: ";
  print_set (Set.inter t2 t3);

  print_string "t2 diff: ";
  print_set (Set.diff t2 t3);

  print_string "t2 filter (mod 2 = 0): ";
  print_set (Set.filter t2 (fun x -> (x mod 2) = 0));
  
  print_string "t2 foreach (print_int): ";
  Set.foreach t2 print_int;
  print_char '\n';

  print_string "t2 for_all (< 5): ";
  print_endline (string_of_bool (Set.for_all t2 (fun x -> x < 5)));
  print_string "t3 for_all (< 5): ";
  print_endline (string_of_bool (Set.for_all t3 (fun x -> x < 5)));

  print_string "t2 exists (5): ";
  print_endline (string_of_bool (Set.exists t2 (fun x -> x = 5)));
  print_string "t3 exists (5): ";
  print_endline (string_of_bool (Set.exists t3 (fun x -> x = 5)));

