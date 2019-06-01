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