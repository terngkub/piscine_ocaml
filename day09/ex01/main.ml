let print_proj ((str, status, grade):App.project) =
  print_endline ("(" ^ str ^ ", " ^ status ^ ", " ^ (string_of_int grade) ^ ")")


let () =
  let a :App.project = ("a", "succeed", 100) in
  let b :App.project = ("b", "failed", 70) in
  let c :App.project = ("c", "failed", 40) in
  let d = App.combine a b in
  let e = App.combine a c in
  let f = App.fail a in
  let g = App.success a in
  print_proj d;
  print_proj e;
  print_proj f;
  print_proj g