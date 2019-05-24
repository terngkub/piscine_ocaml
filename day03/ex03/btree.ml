type 'a tree = Nil | Node of 'a * 'a tree * 'a tree


let rec list_of_tree tree =
  match tree with
  | Nil -> []
  | Node (value, left, right) -> (list_of_tree left) @ (value :: (list_of_tree right))


let rec is_sorted list =
  match list with
  | [] -> true
  | head :: [] -> true
  | first :: second :: _ when first >= second -> false
  | _ :: tail -> is_sorted tail


let rec is_bst tree =
  let list = list_of_tree tree in
  is_sorted list


let rec height tree =
  match tree with
  | Nil -> 0
  | Node (_, left, right) -> 1 + (max (height left) (height right))


let rec is_perfect tree =
  match tree with
  | Nil -> true
  | Node (_, left, Nil) when left <> Nil -> false
  | Node (_, Nil, right) when right <> Nil -> false
  | Node (_, left, right) when height left <> height right -> false
  | Node (_, left, right) -> (is_perfect left) && (is_perfect right)


let rec is_balanced tree =
  match tree with
  | Nil -> true
  | Node (_, left, right) when abs (height left) - (height right) > 1 -> false
  | Node (_, left, right) -> (is_balanced left) && (is_balanced right)


let rec search_bst value tree =
  match tree with
  | Nil -> false
  | Node (v, _, _) when v = value -> true
  | Node (_, left, right) -> (search_bst value left) || (search_bst value right)


let rec add_bst value tree =
  match tree with 
  | Nil -> Node (value, Nil, Nil)
  | Node (v, left, right) when value < v -> Node (v, (add_bst value left), right)
  | Node (v, left, right) when value > v -> Node (v, left, (add_bst value right))
  | Node (_, _, _) -> failwith "Error: duplicate value in the tree"
  

let rec min_node tree =
  match tree with
  | Nil -> failwith "Error: tree is empty"
  | Node (_, left, _) when left <> Nil -> min_node left
  | Node (v, _, _) -> v


let rec delete_bst value tree =
  match tree with
  | Node (v, left, right) when value < v -> Node (v, (delete_bst value left), right)
  | Node (v, left, right) when value > v -> Node (v, left, (delete_bst value right))
  | Node (v, Nil, Nil) when v = value -> Nil
  | Node (v, left, Nil) when v = value -> left
  | Node (v, Nil, right) when v = value-> right
  | Node (v, left, right) when v = value ->
    let new_value = min_node right in
    Node (new_value, left, (delete_bst new_value right))
  | _ -> failwith "Error: can't find value"


let print_tree tree =
  print_string "tree: ";
  if tree = Nil then print_endline "Empty"
  else begin
    let rec loop node =
      match node with
      | Nil -> ()
      | Node (v, left, right) ->
        begin
          loop left;
          print_int v;
          print_char ' ';
          loop right
        end
    in loop tree;
    print_char '\n'
  end


let print_bool bool =
  if bool then print_string "true"
  else print_string "false"


let main () =
  print_endline "Empty tree";
  print_tree Nil;
  print_string "is_bst    : "; print_bool (is_bst Nil);
  print_string "\nis_perfect: "; print_bool (is_perfect Nil);
  print_string "\nis_balance: "; print_bool (is_balanced Nil);
  print_string "\nsearch 1  : "; print_bool (search_bst 1 Nil);
  print_string "\nsearch 5  : "; print_bool (search_bst 5 Nil);

  print_endline "\n\nOne Node tree";
  let tree = Node (1, Nil, Nil) in
  print_tree tree;
  print_string "is_bst    : "; print_bool (is_bst tree);
  print_string "\nis_perfect: "; print_bool (is_perfect tree);
  print_string "\nis_balance: "; print_bool (is_balanced tree);
  print_string "\nsearch 1  : "; print_bool (search_bst 1 tree);
  print_string "\nsearch 5  : "; print_bool (search_bst 5 tree);

  print_endline "\n\nPerfect BST tree";
  let one = Node (1, Nil, Nil) in
  let three = Node (3, Nil, Nil) in
  let two = Node (2, one, three) in
  let six = Node (6, Nil, Nil) in
  let eight = Node (8, Nil, Nil) in
  let seven = Node (7, six, eight) in
  let tree = Node (4, two, seven) in
  print_tree tree;
  print_string "is_bst    : "; print_bool (is_bst tree);
  print_string "\nis_perfect: "; print_bool (is_perfect tree);
  print_string "\nis_balance: "; print_bool (is_balanced tree);
  print_string "\nsearch 1  : "; print_bool (search_bst 1 tree);
  print_string "\nsearch 5  : "; print_bool (search_bst 5 tree);

  print_endline "\n\nBalanced Not BST tree";
  let one = Node (1, Nil, Nil) in
  let nine = Node (9, Nil, Nil) in
  let three = Node (3, Nil, nine) in
  let two = Node (2, one, three) in
  let six = Node (6, Nil, Nil) in
  let eight = Node (8, Nil, Nil) in
  let seven = Node (7, six, eight) in
  let tree = Node (4, two, seven) in
  print_tree tree;
  print_string "is_bst    : "; print_bool (is_bst tree);
  print_string "\nis_perfect: "; print_bool (is_perfect tree);
  print_string "\nis_balance: "; print_bool (is_balanced tree);
  print_string "\nsearch 1  : "; print_bool (search_bst 1 tree);
  print_string "\nsearch 5  : "; print_bool (search_bst 5 tree);
  
  print_endline "\n\nNot Balanced BST tree";
  let one = Node (1, Nil, Nil) in
  let five = Node (5, Nil, Nil) in
  let four = Node (4, Nil, five) in
  let three = Node (3, Nil, four) in
  let two = Node (2, one, three) in
  let seven = Node (7, Nil, Nil) in
  let nine = Node (9, Nil, Nil) in
  let eight = Node (8, seven, nine) in
  let tree = Node (6, two, eight) in
  print_tree tree;
  print_string "is_bst    : "; print_bool (is_bst tree);
  print_string "\nis_perfect: "; print_bool (is_perfect tree);
  print_string "\nis_balance: "; print_bool (is_balanced tree);
  print_string "\nsearch 1  : "; print_bool (search_bst 1 tree);
  print_string "\nsearch 5  : "; print_bool (search_bst 5 tree);

  (* add and delete *)
  print_endline "\n\nAdd and Delete";
  let tree = Nil in

  let tree = add_bst 5 tree in
  print_endline "add 5";
  print_tree tree;

  let tree = add_bst 2 tree in
  print_endline "add 2";
  print_tree tree;

  let tree = add_bst 8 tree in
  print_endline "add 8";
  print_tree tree;

  let tree = add_bst 4 tree in
  print_endline "add 4";
  print_tree tree;

  let tree = add_bst 3 tree in
  print_endline "add 3";
  print_tree tree;

  let tree = add_bst 1 tree in
  print_endline "add 1";
  print_tree tree;

  let tree = add_bst 9 tree in
  print_endline "add 9";
  print_tree tree;

  let tree = delete_bst 1 tree in
  print_endline "delete 1";
  print_tree tree;

  let tree = delete_bst 8 tree in
  print_endline "delete 8";
  print_tree tree;

  let tree = delete_bst 4 tree in
  print_endline "delete 4";
  print_tree tree;

  let tree = delete_bst 2 tree in
  print_endline "delete 2";
  print_tree tree


let () = main ()
