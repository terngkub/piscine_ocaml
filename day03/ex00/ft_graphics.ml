type 'a tree = Nil | Node of 'a * 'a tree * 'a tree


let draw_square x y size =
  let margin = size / 2 in
  Graphics.moveto (x + margin) (y + margin);
  Graphics.lineto (x + margin) (y - margin);
  Graphics.lineto (x - margin) (y - margin);
  Graphics.lineto (x - margin) (y + margin);
  Graphics.lineto (x + margin) (y + margin)


let draw_rect x y size_x size_y =
  let margin_x = size_x / 2 in
  let margin_y = size_y / 2 in
  Graphics.moveto (x + margin_x) (y + margin_y);
  Graphics.lineto (x + margin_x) (y - margin_y);
  Graphics.lineto (x - margin_x) (y - margin_y);
  Graphics.lineto (x - margin_x) (y + margin_y);
  Graphics.lineto (x + margin_x) (y + margin_y)


let draw_node x y str =
  draw_rect x y 60 30;
  Graphics.moveto x y;
  Graphics.draw_string v;


let draw_tree_node node =
  match node with
  | Node (v, Nil, Nil) ->
  begin
    Graphics.open_graph " 600x400";
    draw_rect 100 100 60 30;
    draw_rect 200 200 60 30;
    draw_rect 200 300 60 30;
    Graphics.moveto 100 100;
    Graphics.draw_string v;
    ignore (Graphics.read_key ());
    ()
  end
  | Nil | _ -> ()

let main () =
  let node = Node ("hello", Nil, Nil) in
  draw_tree_node node

let _ = main ()