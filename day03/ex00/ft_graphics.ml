type 'a tree = Nil | Node of 'a * 'a tree * 'a tree


let draw_square x y size =
  let padding = size / 2 in
  Graphics.moveto (x + padding) (y + padding);
  Graphics.lineto (x + padding) (y - padding);
  Graphics.lineto (x - padding) (y - padding);
  Graphics.lineto (x - padding) (y + padding);
  Graphics.lineto (x + padding) (y + padding)


let draw_rect_text x y str =
  let padding_x = 30 in
  let padding_y = 15 in
  Graphics.moveto (x + padding_x) (y + padding_y);
  Graphics.lineto (x + padding_x) (y - padding_y);
  Graphics.lineto (x - padding_x) (y - padding_y);
  Graphics.lineto (x - padding_x) (y + padding_y);
  Graphics.lineto (x + padding_x) (y + padding_y);
  Graphics.moveto (x - 15) (y - 5);
  Graphics.draw_string str


let draw_tree_node node =
  match node with
  | Node (v, Nil, Nil) ->
  begin
    draw_rect_text 150 150 v;
    Graphics.moveto 180 150;
    Graphics.lineto 220 100;
    draw_rect_text 250 100 "Nil";
    Graphics.moveto 180 150;
    Graphics.lineto 220 200;
    draw_rect_text 250 200 "Nil"
  end
  | Nil | _ -> draw_rect_text 200 150 "Nil"


let main () =
  (* draw_square *)
  Graphics.open_graph " 400x300";
  draw_square 50 50 20;
  ignore (read_line ());

  (* draw_tree_node Node *)
  Graphics.open_graph " 400x300";
  draw_tree_node (Node ("hello", Nil, Nil));
  ignore (read_line ());

  (* draw_tree_node Nil *)
  Graphics.open_graph " 400x300";
  draw_tree_node (Nil);
  ignore (read_line ())


let () = main ()
