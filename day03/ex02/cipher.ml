let caesar str n =
  let rot_char c =
    if (c >= 'a') && (c <= 'z') then begin
      let int_a = int_of_char 'a' in
      let int_c = int_of_char c in
      char_of_int (int_a + ((int_c - int_a + n) mod 26))
    end else if (c >= 'A') && (c <= 'Z') then begin
      let int_A = int_of_char 'A' in
      let int_c = int_of_char c in
      char_of_int (int_A + ((int_c - int_A + n) mod 26))
    end else c
  in
  String.map rot_char str
  

let rot42 str =
  caesar str 42


let xor str n =
  let xor_char c =
    char_of_int (lxor (int_of_char c) n)
  in
  String.map xor_char str

