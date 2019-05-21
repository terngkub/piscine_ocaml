type 'a tree = Nil | Node of 'a * 'a tree * 'a tree


let draw_square x y size =
  let margin = size / 2 in
  Graphics.moveto x y;
  Graphics.draw_string "center";
  Graphics.lineto (x + margin) y;
  Graphics.lineto (x + margin) (y + margin);
  Graphics.lineto x (y + margin);
  Graphics.lineto x y


let draw_tree_node node =
  match node with
  | Node (v, Nil, Nil) ->
  begin
    Graphics.moveto 100 100;
    Graphics.draw_string v
  end
  | Nil | _ -> ()

let main () =
  Graphics.open_graph " 800x600";
  draw_square 100 100 200;
  let node = Node ("hello", Nil, Nil) in
  draw_tree_node node;
  Graphics.read_key ()

let _ = main ()