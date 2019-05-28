let print_joke (jokes:string array) =
  Random.self_init ();
  print_endline jokes.(Random.int (Array.length jokes))

let main () =
  let jokes = [|
    "Joke 1. I'm lazy, you know.";
    "Joke 2. I'm lazy, you know.";
    "Joke 3. I'm lazy, you know.";
    "Joke 4. I'm lazy, you know.";
    "Joke 5. I'm lazy, you know.";
  |] in
  print_joke jokes


let () = main ()
