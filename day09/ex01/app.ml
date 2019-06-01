type project = string * string * int


let zero :project = ("", "", 0)


let combine ((a_str, a_status, a_grade):project) ((b_str, b_status, b_grade):project) :project =
  let avg_grade = (a_grade + b_grade) / 2 in
  let new_status =
    if avg_grade >= 80 then "succeed"
    else "failed"
  in
  (a_str ^ b_str, new_status, avg_grade)


let fail ((str, _, _):project) :project = (str, "failed", 0)


let success ((str, _, _):project) :project = (str, "succeed", 80)
