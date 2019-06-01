module Try =
struct

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

end


let () =
  let t1 = Try.return 1 in

  let t2 : 'a Try.t = Try.bind t1 (fun x -> Try.return (x / 0)) in
  (match t2 with
    Try.Success(e) -> print_endline (string_of_int e)
  | Try.Failure(e) -> print_endline "Error");

  let t3 = Try.recover t2 (fun x -> Try.return 0) in
  (match t3 with
      Try.Success(e) -> print_endline (string_of_int e)
    | Try.Failure(e) -> print_endline "Error" );

  let t1 = Try.return (Try.return 1) in
  let t2 = Try.flatten t1 in
  (match t2 with
      Try.Success(e) -> print_endline (string_of_int e)
    | Try.Failure(e) -> print_endline "Error" );

  let t1 = Try.return 1 in
  let t2 = Try.filter t1 (fun x -> x = 1) in
  (match t2 with
      Try.Success(e) -> print_endline "true"
    | Try.Failure(e) -> print_endline "false" );

  let t2 = Try.filter t1 (fun x -> x <> 1) in
  (match t2 with
      Try.Success(e) -> print_endline "true"
    | Try.Failure(e) -> print_endline "false" )