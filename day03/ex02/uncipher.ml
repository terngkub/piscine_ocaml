let rotn n c =
  if (c >= 'a') && (c <= 'z') then begin
    let int_a = int_of_char 'a' in
    let int_c = int_of_char c in
    char_of_int (int_a + ((int_c - int_a + n) mod 26))
  end else if (c >= 'A') && (c <= 'Z') then begin
    let int_A = int_of_char 'A' in
    let int_c = int_of_char c in
    char_of_int (int_A + ((int_c - int_A + n) mod 26))
  end else c


let uncaesar n str =
  if n < 0 then "Error: key is negative"
  else if n = 0 then str
  else 
    let new_n = 26 - (n mod 26) in
    String.map (rotn new_n) str


let unrot42 str =
  String.map (rotn 10) str


let is_printable c =
  if c >= ' ' && c <= '~' then true
  else false


let xor_char n c =
  let new_c = char_of_int (((int_of_char c) lxor n) mod 256) in
  if is_printable new_c then new_c
  else c


let xor n str =
  if n < 0 then "Error: key is negative"
  else if n = 0 then str
  else String.map (xor_char n) str


let ft_uncrypt (str:string) (func_list:(string->string)list) :string =
  let rec loop lst ret =
    match lst with
    | [] -> ret
    | head :: tail -> loop tail (head ret)
  in loop func_list str


let main () =
  print_endline "\n*************\nft_uncrypt\n*************\n";

  print_endline "basic:";
  print_endline (ft_uncrypt "cde_zab_CDE_ZAB" [uncaesar 2]);
  print_endline (ft_uncrypt "qrs_nop_QRS_NOP" [unrot42]);
  print_endline (ft_uncrypt "c`a]z{x]C@A]Z[X" [xor 2]);

  print_endline "\nmultiple:";
  print_endline (ft_uncrypt "stu_pqr_STU_PQR" [unrot42; uncaesar 2]);
  print_endline (ft_uncrypt "afg]xc`]AFG]XC@" [xor 2; uncaesar 2]);
  print_endline (ft_uncrypt "qvw]rsp]QVW]RSP" [xor 2; unrot42; uncaesar 2]);

  print_endline "\nbig key:";
  print_endline (ft_uncrypt "vwx_stu_VWX_STU" [uncaesar 12345]);
  print_endline (ft_uncrypt "X[ZfA@Cfx{zfa`c" [xor 12345]);

  print_endline "\nzero key:";
  print_endline (ft_uncrypt "abc_xyz_ABC_XYZ" [uncaesar 0]);
  print_endline (ft_uncrypt "abc_xyz_ABC_XYZ" [xor 0]);

  print_endline "\nnegative key:";
  print_endline (ft_uncrypt "abc_xyz_ABC_XYZ" [uncaesar (-2)]);
  print_endline (ft_uncrypt "abc_xyz_ABC_XYZ" [xor (-2)])


let () = main ()
