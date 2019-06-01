module type MONOID =
sig
  type element
  val zero1 : element
  val zero2 : element
  val add : element -> element -> element
  val sub : element -> element -> element
  val mul : element -> element -> element
  val div : element -> element -> element
end


module INT : (MONOID with type element = int) =
struct
  type element = int
  let zero1 = 0
  let zero2 = 1
  let add :element -> element -> element = ( + )
  let sub :element -> element -> element = ( - )
  let mul :element -> element -> element = ( * )
  let div :element -> element -> element = ( / )
end


module FLOAT : (MONOID with type element = float) =
struct
  type element = float
  let zero1 = 0.
  let zero2 = 1.
  let add :element -> element -> element = ( +. )
  let sub :element -> element -> element = ( -. )
  let mul :element -> element -> element = ( *. )
  let div :element -> element -> element = ( /. )
end


module Calc =
  functor (M: MONOID) ->
  struct
    let add :M.element -> M.element -> M.element = M.add
    let sub :M.element -> M.element -> M.element = M.sub
    let mul :M.element -> M.element -> M.element = M.mul
    let div :M.element -> M.element -> M.element = M.div

    let power (base:M.element) (degree:int) :M.element =
      if degree = 0 then M.zero2
      else begin
        let rec loop i ret =
          if i <= 0 then ret
          else loop (i - 1) (M.mul base ret)
        in
        loop degree M.zero2
      end

    
    let fact (n:M.element) :M.element =
      let rec loop (i:M.element) ret =
        if i <= M.zero1 then ret
        else loop (M.sub i M.zero2) (M.mul i ret)
      in
      loop n M.zero2
    
  end


module Calc_int = Calc (INT)
module Calc_float = Calc (FLOAT)


let () =
  print_endline "subject:";
  print_endline (string_of_int (Calc_int.power 3 3));
  print_endline (string_of_float (Calc_float.power 3.0 3));
  print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
  print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0));

  print_endline "\nint:";
  print_endline (string_of_int (Calc_int.add 21 3));
  print_endline (string_of_int (Calc_int.sub 21 3));
  print_endline (string_of_int (Calc_int.mul 21 3));
  print_endline (string_of_int (Calc_int.div 21 3));
  print_endline (string_of_int (Calc_int.fact 4));

  print_endline "\nfloat:";
  print_endline (string_of_float (Calc_float.add 21. 3.));
  print_endline (string_of_float (Calc_float.sub 21. 3.));
  print_endline (string_of_float (Calc_float.mul 21. 3.));
  print_endline (string_of_float (Calc_float.div 21. 3.));
  print_endline (string_of_float (Calc_float.fact 4.));