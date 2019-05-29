let () =
  let dalek = new Dalek.dalek in
  let people = new People.people in
  print_endline dalek#to_string;
  dalek#talk;
  dalek#exterminate people;
  dalek#die
