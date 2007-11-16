(* From http://osdir.com/ml/lang.ocaml.lib.devel/2004-04/msg00017.html *)

exception Invalid_char

external unsafe_char_of_int : int -> char = "%identity"

let chars = [|
 'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';
 'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'a';'b';'c';'d';'e';'f';
 'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';
 'w';'x';'y';'z';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'+';'/'
|]

let inv_chars =
 let a = Array.make 256 (-1) in
 for i = 0 to 63 do
  Array.unsafe_set a (int_of_char (Array.unsafe_get chars i)) i;
 done;
 a

let encode s =
 let data = ref 0 in
 let count = ref 0 in
 let b = Buffer.create 0 in
 let l = String.length s - 1 in
 for i = 0 to l do
  let c = int_of_char (String.unsafe_get s i) in
  data := (!data lsl 8) lor c;
  count := !count + 8;
  while !count >= 6 do
   count := !count - 6;
   let d = (!data asr !count) land 63 in
   Buffer.add_char b (Array.unsafe_get chars d)
  done;
 done;
 if !count > 0 then begin
  let d = (!data lsl (6 - !count)) land 63 in
  Buffer.add_char b (Array.unsafe_get chars d);
 end;
 Buffer.contents b

let decode s =
 let data = ref 0 in
 let count = ref 0 in
 let b = Buffer.create 0 in
 let l = String.length s - 1 in
 for i = 0 to l do
  let c = int_of_char (String.unsafe_get s i) in
  let c = Array.unsafe_get inv_chars c in
  if c = -1 then raise Invalid_char;
  data := (!data lsl 6) lor c;
  count := !count + 6;
  while !count >= 8 do
   count := !count - 8;
   let d = (!data asr !count) land 0xFF in
   Buffer.add_char b (unsafe_char_of_int d);
  done;
 done;
 Buffer.contents b
