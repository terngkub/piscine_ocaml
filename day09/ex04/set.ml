type 'a t = 'a list


let return (x:'a) :'a t = [x]


let bind (x:'a t) (f:'a -> 'b t) :'b t =
  let rec loop lst ret =
    match lst with
    | [] -> ret
    | head :: tail -> loop tail (ret @ f head)
  in
  loop x []


let union (x:'a t) (y:'a t) :'a t =
  let rec loop lst ret =
    match lst with
    | [] -> ret
    | head :: tail -> begin
      try ignore @@ List.find (fun x -> x = head) x; loop tail ret
      with Not_found -> loop tail (ret @ [head])
    end
  in
  loop y x


let inter (x:'a t) (y:'a t) :'a t =
  let rec loop lst ret =
    match lst with
    | [] -> ret
    | head :: tail -> begin
      try ignore @@ List.find (fun x -> x = head) y; loop tail (ret @ [head])
      with Not_found -> loop tail ret
    end
  in
  loop x []


let diff (x:'a t) (y:'a t) :'a t =
  let rec loop lst ret =
    match lst with
    | [] -> ret
    | head :: tail -> begin
      try ignore @@ List.find (fun x -> x = head) y; loop tail ret
      with Not_found -> loop tail (ret @ [head])
    end
  in
  loop x []


let filter (x:'a t) (f:'a -> bool) :'a t =
  List.filter f x


let foreach (x:'a t) (f:'a -> unit) =
  List.iter f x


let for_all (x:'a t) (f:'a -> bool) :bool =
  List.for_all f x


let exists (x:'a t) (f:'a -> bool) :bool =
  List.exists f x