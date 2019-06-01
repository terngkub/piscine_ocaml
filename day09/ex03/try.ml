  type 'a t = Success of 'a | Failure of exn


  let return (x:'a) :'a t = Success x


  let bind (x:'a t) (f:'a -> 'b t) :'b t =
    match x with
    | Success y ->
      begin
        try f y
        with error -> Failure error
      end
    | _ -> x


  let recover (x:'a t) (f:exn -> 'a t) :'a t =
    match x with
    | Failure y -> f y
    | _ -> x


  let filter (x:'a t) (f:'a -> bool) :'a t =
    match x with
    | Success y when not (f y) -> Failure (Failure "error")
    | _ -> x


  let flatten (x:'a t t) :'a t =
    match x with
    | Success y ->
      begin
        match y with
        | Success z -> Success z
        | Failure z -> Failure z
      end
    | Failure y -> Failure y