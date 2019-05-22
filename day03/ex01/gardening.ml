type 'a tree = Nil | Node of 'a * 'a tree * 'a tree


let rec size tree =
  match tree with
  | Nil -> 0
  | Node (_, left, right) -> 1 + (size left) + (size right)


let rec height tree =
  match tree with
  | Nil -> 0
  | Node (_, left, right) -> 1 + (max (height left) (height right))


let draw_rect_text x y str =
  let padding_x = 30 in
  let padding_y = 15 in
  Graphics.moveto (x + padding_x) (y + padding_y);
  Graphics.lineto (x + padding_x) (y - padding_y);
  Graphics.lineto (x - padding_x) (y - padding_y);
  Graphics.lineto (x - padding_x) (y + padding_y);
  Graphics.lineto (x + padding_x) (y + padding_y);
  Graphics.moveto (x - 15) (y - 5);
  Graphics.draw_string str;
  ()


let draw_tree tree =

  (* height *)
  let tree_height = height tree in

  (* box *)
  let box_size_x = 60 in
  let box_size_y = 30 in
  let box_margin_x = 40 in
  let box_margin_y = 50 in

  (* window *)
  let window_margin_x = 50 in
  let window_margin_y = 50 in
  let window_size_x = (window_margin_x * 2) + (tree_height * box_size_x) + ((tree_height - 1) * box_margin_x) in
  let window_size_y = (window_margin_y * 2) + (tree_height * box_size_y) + ((tree_height - 1) * box_margin_y) in

  (* open graph *)
  let window_str = " " ^ (string_of_int window_size_x) ^ "x" ^ (string_of_int window_size_y) in
  Graphics.open_graph window_str;


  (* draw *)
  let rec draw_node x upper_y lower_y node =
    let middle_y = (upper_y + lower_y) / 2 in
    match node with
    | Nil -> draw_rect_text x middle_y "Nil"
    | Node (value, left, right) ->
      begin
        draw_rect_text x middle_y value;
        if left <> Nil then begin
          Graphics.moveto (x + (box_size_x / 2)) middle_y;
          Graphics.lineto (x + (box_size_x / 2) + box_margin_x) ((upper_y + middle_y) / 2);
          draw_node (x + box_size_x + box_margin_x) upper_y middle_y left
        end;
        if right <> Nil then begin
          Graphics.moveto (x + (box_size_x / 2)) middle_y;
          Graphics.lineto (x + (box_size_x / 2) + box_margin_x) ((middle_y + lower_y) / 2);
          draw_node (x + box_size_x + box_margin_x) middle_y lower_y right
        end
      end
  in
  draw_node window_margin_x 0 window_size_y tree;

  (* wait *)
  ignore (Graphics.read_key ())


let main () =
  let h = Node ("H", Nil, Nil) in
  let g = Node ("G", Nil, Nil) in
  let f = Node ("F", Nil, Nil) in
  let e = Node ("E", g, h) in
  let d = Node ("D", Nil, Nil) in
  let c = Node ("C", Nil, f) in
  let b = Node ("B", d, e) in
  let a = Node ("A", b, c) in
  print_string "Size: ";
  print_int (size a);
  print_char '\n';
  print_string "Height: ";
  print_int (height a);
  print_char '\n';
  draw_tree a;
  ()


let () = main ()
