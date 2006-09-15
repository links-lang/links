(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

type uchar = int

module Utf8 =
struct
  include Custom.String
  type uindex  = int

  let empty = ""


  let start_index s = 0
  let end_index s = String.length s
  let equal_index = (==)
  let mk s = s
  let mk_idx i = i
  let get_str s = s
  let get_idx i = i

(* TODO: handle 5,6 bytes chars; report malformed UTF-8 *)
  let rec check s i j = 
    (i = j) ||
    (i < j) &&
    match s.[i] with
      | '\000'..'\127' -> check s (i+1) j
      | '\192'..'\223' as c ->
	  (i+1 < j) &&
	  let n1 = Char.code c 
	  and n2 = Char.code s.[i+1] in
	  (n2 lsr 6 == 0b10) 
	  && let p = ((n1 land 0x1f) lsl 6) lor (n2 land 0x3f) in
	  (p >= 0x80) && (p < 0x800) 
	  && check s (i+2) j
      | '\224'..'\239' as c -> 
	  (i+2 < j) &&
	  let n1 = Char.code c 
	  and n2 = Char.code s.[i+1] 
	  and n3 = Char.code s.[i+2] in
	  (n2 lsr 6 == 0b10) && (n3 lsr 6 == 0b10) 
	  && let p = ((n1 land 0x0f) lsl 12)  lor ((n2 land 0x3f) lsl 6) 
	    lor (n3 land 0x3f) in
	  (p >= 0x800) 
	  && ((p < 0xd800) || (p >= 0xe000)) 
	  && ((p < 0xfffe) || (p >  0xffff)) 
	  &&  check s (i+3) j
      | '\240'..'\247' as c -> 
	  (i+3 < j) &&
	  let n1 = Char.code c 
	  and n2 = Char.code s.[i+1] 
	  and n3 = Char.code s.[i+2] 
	  and n4 = Char.code s.[i+3] in
	  (n2 lsr 6 == 0b10) && (n3 lsr 6 == 0b10) && (n4 lsr 6 == 0b10) 
	  && let p = ((n1 land 0x07) lsl 18) lor ((n2 land 0x3f) lsl 12) lor
            ((n3 land 0x3f) lsl 6) lor (n4 land 0x3f) in
	  (p >= 0x10000) && (p < 0x110000) 
	  && check s (i+4) j
      | _ -> false


  let is_valid s = check s 0 (String.length s)
  let check s = assert (is_valid s)
  let mk_check s =
    if is_valid s then Some (mk s) else None

  let get s i =
    match s.[i] with
      | '\000'..'\127' as c -> 
	  Char.code c
      | '\192'..'\223' as c ->
	  ((Char.code c land 0b11111) lsl 6) lor
	  ((Char.code s.[i+1] land 0b111111))
      | '\224'..'\239' as c ->
	  ((Char.code c land 0b11111) lsl 12) lor
	  ((Char.code s.[i+1] land 0b111111) lsl 6) lor
	  ((Char.code s.[i+2] land 0b111111))
      | '\240'..'\247' as c ->
	  ((Char.code c land 0b11111) lsl 18) lor
	  ((Char.code s.[i+1] land 0b111111) lsl 12) lor
	  ((Char.code s.[i+2] land 0b111111) lsl 6) lor
	  ((Char.code s.[i+3] land 0b111111))
      | _ -> failwith "Malformed UTF-8 bufffer"

  let next s i =
    match s.[i] with
      | '\000'..'\127' as c -> 
	  Char.code c, i + 1
      | '\192'..'\223' as c ->
	  ((Char.code c land 0b11111) lsl 6) lor
	  ((Char.code s.[i+1] land 0b111111)), i + 2
      | '\224'..'\239' as c ->
	  ((Char.code c land 0b1111) lsl 12) lor
	  ((Char.code s.[i+1] land 0b111111) lsl 6) lor
	  ((Char.code s.[i+2] land 0b111111)), i + 3
      | '\240'..'\247' as c ->
	  ((Char.code c land 0b111) lsl 18) lor
	  ((Char.code s.[i+1] land 0b111111) lsl 12) lor
	  ((Char.code s.[i+2] land 0b111111) lsl 6) lor
	  ((Char.code s.[i+3] land 0b111111)), i + 4
      | _ -> failwith "Malformed UTF-8 bufffer"

(* TODO: handle UTF-8 viewport *)
  let rec to_string_aux buf s i n =
    if (i = n) then ()
    else
      let (c,i) = next s i in
      if (c <= 255) then Buffer.add_char buf (Char.chr c)
      else Printf.bprintf buf"\\%i;" c;
      to_string_aux buf s i n
    
  let to_string s =
    let b = Buffer.create (String.length s) in
    to_string_aux b s 0 (String.length s);
    Buffer.contents b

  let print ppf s = 
    Format.fprintf ppf "%s" (to_string s)



  let advance s i =
    match s.[i] with
      | '\000'..'\127' -> i + 1
      | '\192'..'\223' -> i + 2
      | '\224'..'\239' -> i + 3 
      | '\240'..'\247' -> i + 4
      | _ -> failwith "Malformed UTF-8 bufffer"
(*
  let width = Array.create 256 1
  let () = 
    for i = 192 to 223 do width.(i) <- 2 done;
    for i = 224 to 249 do width.(i) <- 3 done;
    for i = 240 to 248 do width.(i) <- 4 done

  let len s i =
    Array.unsafe_get width (Char.code s.[i])
*)

  let store b p =
(* Adapted from Netstring's netconversion.ml/write_utf8 *)
    if p <= 127 then
      Buffer.add_char b (Char.chr p)
    else if p <= 0x7ff then (
      Buffer.add_char b (Char.chr (0xc0 lor (p lsr 6)));
      Buffer.add_char b (Char.chr (0x80 lor (p land 0x3f)))
    )
    else if p <= 0xffff then (
      (* Refuse writing surrogate pairs, and fffe, ffff *)
      if (p >= 0xd800 && p < 0xe000) || (p >= 0xfffe) then
	failwith "Encodings.Utf8.store";
      Buffer.add_char b (Char.chr (0xe0 lor (p lsr 12)));
      Buffer.add_char b (Char.chr (0x80 lor ((p lsr 6) land 0x3f)));
      Buffer.add_char b (Char.chr (0x80 lor (p land 0x3f)))
    )
    else if p <= 0x10ffff then (
      Buffer.add_char b (Char.chr (0xf0 lor (p lsr 18)));
      Buffer.add_char b (Char.chr (0x80 lor ((p lsr 12) land 0x3f)));
      Buffer.add_char b (Char.chr (0x80 lor ((p lsr 6)  land 0x3f)));
      Buffer.add_char b (Char.chr (0x80 lor (p land 0x3f)))
    )
    else
      (* Higher code points are not possible in XML: *)
      failwith "Encodings.Utf8.store"

  let rec mk_latin1_aux buf s i n =
    if (i = n) then ()
    else (store buf (Char.code s.[i]); mk_latin1_aux buf s (succ i) n)

  let mk_latin1 s =
    let b = Buffer.create (String.length s) in
    mk_latin1_aux b s 0 (String.length s);
    Buffer.contents b

  let copy b s i j =
    Buffer.add_substring b s i (j - i)

  let get_substr s i j =
    String.sub s i (j - i)

  let mk_char p =
    if p <= 127 then 
      String.make 1 (Char.chr p)
    else if p <= 0x7ff then
      let s = String.make 2 (Char.chr (0xc0 lor (p lsr 6))) in
      s.[1] <- Char.chr (0x80 lor (p land 0x3f));
      s
    else if p <= 0xffff then
      let s = String.make 3 (Char.chr (0xe0 lor (p lsr 12))) in
      s.[1] <- Char.chr (0x80 lor ((p lsr 6) land 0x3f));
      s.[2] <- Char.chr (0x80 lor (p land 0x3f));
      s
    else
      let s = String.make 4 (Char.chr (0xf0 lor (p lsr 18))) in
      s.[1] <- Char.chr (0x80 lor ((p lsr 12) land 0x3f));
      s.[2] <- Char.chr (0x80 lor ((p lsr 6)  land 0x3f));
      s.[3] <- Char.chr (0x80 lor (p land 0x3f));
      s

  let concat s1 s2 = s1 ^ s2
end
