module type FIXED =
sig
  type t
  val of_float  : float -> t
  val of_int    : int -> t
  val to_float  : t -> float
  val to_int    : t -> int
  val to_string : t -> string
  val zero      : t
  val one       : t
  val succ      : t -> t
  val pred      : t -> t
  val min       : t -> t -> t
  val max       : t -> t -> t
  val gth       : t -> t -> bool
  val lth       : t -> t -> bool
  val gte       : t -> t -> bool
  val lte       : t -> t -> bool
  val eqp       : t -> t -> bool
  val eqs       : t -> t -> bool
  val add       : t -> t -> t
  val sub       : t -> t -> t
  val mul       : t -> t -> t
  val div       : t -> t -> t
  val foreach   : t -> t -> (t -> unit) -> unit
end


module type FRACTIONNAL_BITS =
sig
  val bits : int
end


module type MAKE =
  functor (FractionalBits : FRACTIONNAL_BITS) -> FIXED


module Make : MAKE =
  functor (FractionalBits : FRACTIONNAL_BITS) ->
  struct

    type t = int

    let of_float (f:float) :t =
      int_of_float (f *. (2. ** (float_of_int FractionalBits.bits)))


    let of_int (i:int) :t =
      i lsl FractionalBits.bits


    let to_float (fixed:t) :float =
      (float_of_int fixed) /. (2. ** (float_of_int FractionalBits.bits))


    let to_int (fixed:t) :int =
      fixed lsr FractionalBits.bits


    let to_string (fixed:t) :string =
      string_of_float (to_float fixed)

    
    let zero:t = 0


    let one:t = of_int 1


    let succ (fixed:t) :t =
      fixed + (1 lsl FractionalBits.bits)
    

    let pred (fixed:t) :t =
      fixed - (1 lsl FractionalBits.bits)


    let min (one:t) (two:t) :t =
      if one < two then one
      else two

    
    let max (one:t) (two:t) :t =
      if one > two then one
      else two


    let gth (one:t) (two:t) :bool = one > two


    let lth (one:t) (two:t) :bool = one < two


    let gte (one:t) (two:t) :bool = one >= two


    let lte (one:t) (two:t) :bool = one <= two

    
    let eqp (one:t) (two:t) :bool = one == two

    
    let eqs (one:t) (two:t) :bool = (one = two)

    
    let add (one:t) (two:t) :t = one + two

    
    let sub (one:t) (two:t) :t = one - two

    
    let mul (one:t) (two:t) :t = one * two

    
    let div (one:t) (two:t) :t = one / two


    let foreach (start:t) (stop:t) (func:(t -> unit)) :unit =
      let rec loop i =
        if i > stop then ()
        else begin (func i); loop (i + 1) end
      in
      loop start

  end


module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
  let x8 = Fixed8.of_float 21.10 in
  let y8 = Fixed8.of_float 21.32 in
  let r8 = Fixed8.add x8 y8 in
  print_endline (Fixed8.to_string r8);
  Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f))