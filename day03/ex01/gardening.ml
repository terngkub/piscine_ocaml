type 'a tree = Nil | Node of 'a * 'a tree * 'a tree


let rec size tree =
  match tree with
  | Nil -> 0
  | Node (_, left, right) -> 1 + (size left) + (size right)


let rec height tree =
  match tree with
  | Nil -> 0
  | Node (_, left, right) -> 1 + (max (height left) (height right))


let main () =
  let g = Node ("G", Nil, Nil) in
  let f = Node ("F", Nil, Nil) in
  let e = Node ("E", g, Nil) in
  let d = Node ("D", Nil, Nil) in
  let c = Node ("C", Nil, f) in
  let b = Node ("B", d, e) in
  let a = Node ("A", b, c) in
  print_string "Size: ";
  print_int (size a);
  print_char '\n';
  print_string "Height: ";
  print_int (height a);
  print_char '\n'


let () = main ()
