let print_joke (jokes:string array) =
  Random.self_init ();
  print_endline jokes.(Random.int (Array.length jokes))

let main () =
  let jokes = [|
    "Joke 1";
    "Joke 2";
    "Joke 3";
    "Joke 4";
    "Joke 5";
  |] in
  print_joke jokes


let () = main ()
