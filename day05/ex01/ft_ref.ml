type 'a ft_ref = {mutable contents : 'a}


let return (contents:'a) :'a ft_ref =
  {contents = contents}


let get (reference:'a ft_ref) :'a =
  reference.contents


let set (reference:'a ft_ref) (contents:'a) :unit =
  reference.contents <- contents


let bind (reference:'a ft_ref) (func :'a -> 'b ft_ref) :'b ft_ref =
  func reference.contents


let ft_ref_float_of_int (i:int) :float ft_ref =
  {contents = (float_of_int i)}


let main () =
  let x = return 42 in
  print_int (get x);
  print_char '\n';
  set x 21;
  print_int (get x);
  print_char '\n';
  let y = bind x ft_ref_float_of_int in
  print_float (get y);
  print_char '\n'


let () = main ()
