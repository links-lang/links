(* Serialisation utilities *)
(*open Str*)
open Num
open List
(*open String*)

open Utility

type 'a serialiser = 'a -> string
and 'a deserialiser = string -> ('a * string)

(* Take a string represeting a sequence of serialised objects, return
   the type of the first object, the string containing the object
   itself (minus type and length header) and the rest of the string *)
let extract_object s : (char * string * string) =
  let tlv s = 
    let extract_length s = 
      (ignore (Str.search_forward (Str.regexp "[0-9]+") s 1); 
       Str.matched_string s)
    and extract_type s = s.[0]
    and n_bytes s n start = Str.string_before (Str.string_after s start) n
    in let t, l = extract_type s, extract_length s in 
      t, l, n_bytes s (int_of_string l) (2 + String.length l) in
  let t, l, v = tlv s in 
    t, v, Str.string_after s (1 + String.length l + 1 + String.length v)
and add_header typ obj = 
  (String.make 1 typ) ^ string_of_int (String.length obj) ^ "+" ^ obj

let primitive_serialisers (header : char) (convin : 'a -> string) (convout : string -> 'a) 
    : ('a serialiser * 'a deserialiser) =
  ((add_header header) -<- convin,
   fun s -> match extract_object s with
     | (h, s, rest) when h = header -> (convout s, rest)
     | (h, _, _) -> failwith ("Error deserialising primitive: expected header "^  (String.make 1 header) ^"; got "^ (String.make 1 h)))
    
let serialise_string, (deserialise_string : string deserialiser)  = primitive_serialisers 'h' identity identity
let serialise_bool, deserialise_bool = primitive_serialisers 'b' string_of_bool bool_of_string
let serialise_int, deserialise_int = primitive_serialisers 'i' string_of_num num_of_string
let serialise_oint, deserialise_oint = primitive_serialisers 's' string_of_int int_of_string
let serialise_char, deserialise_char = (fun s -> String.make 1 s), (fun s -> (s.[0], Str.string_after s 1))
let serialise_float, deserialise_float = primitive_serialisers 'f' string_of_float float_of_string

let invalid_header s c = failwith (Printf.sprintf "Error deserialising %s (unexpected header '%c')" s c)

let null_serialiser _ = ""
and null_deserialiser obj s = (obj, s)

let serialise_option (serialise : 'a serialiser) : 'a option serialiser = function
  | None   -> "n"
  | Some s -> "s" ^ serialise s
and deserialise_option (deserialise : 'a deserialiser) : 'a option deserialiser = 
  fun s ->
    match s.[0], StringLabels.sub s ~pos:1 ~len:(String.length s - 1) with
      | 'n', rest -> None, rest
      | 's', rest -> (let obj, rest = deserialise rest in 
                        Some obj, rest)
      | (c,_) -> invalid_header "option" c

(* Why, /yes/, I /would/ like generic tuple facilities. Why do you ask? *)
let serialise0 typ () () : string
    = add_header typ ""
and serialise1 typ (s1) (t1) : string
    = add_header typ (s1 t1)
and serialise2 typ (s1, s2) (t1, t2) : string
    = add_header typ (s1 t1 ^ s2 t2)
and serialise3 typ (s1, s2, s3) (t1, t2, t3) : string
    = add_header typ (s1 t1 ^ s2 t2 ^ s3 t3)
and serialise4 typ (s1, s2, s3, s4) (t1, t2, t3, t4) : string
    = add_header typ (s1 t1 ^ s2 t2 ^ s3 t3 ^ s4 t4)
and serialise5 typ (s1, s2, s3, s4, s5) (t1, t2, t3, t4, t5) : string
    = add_header typ (s1 t1 ^ s2 t2 ^ s3 t3 ^ s4 t4 ^ s5 t5)
and serialise6 typ (s1, s2, s3, s4, s5, s6) (t1, t2, t3, t4, t5, t6) : string
    = add_header typ (s1 t1 ^ s2 t2 ^ s3 t3 ^ s4 t4 ^ s5 t5 ^ s6 t6)
and serialise7 typ (s1, s2, s3, s4, s5, s6, s7) (t1, t2, t3, t4, t5, t6, t7) : string
    = add_header typ (s1 t1 ^ s2 t2 ^ s3 t3 ^ s4 t4 ^ s5 t5 ^ s6 t6 ^ s7 t7)
and serialise8 typ (s1, s2, s3, s4, s5, s6, s7, s8) (t1, t2, t3, t4, t5, t6, t7, t8) : string
    = add_header typ (s1 t1 ^ s2 t2 ^ s3 t3 ^ s4 t4 ^ s5 t5 ^ s6 t6 ^ s7 t7 ^ s8 t8)
and serialise9 typ (s1, s2, s3, s4, s5, s6, s7, s8, s9) (t1, t2, t3, t4, t5, t6, t7, t8, t9) : string
    = add_header typ (s1 t1 ^ s2 t2 ^ s3 t3 ^ s4 t4 ^ s5 t5 ^ s6 t6 ^ s7 t7 ^ s8 t8 ^ s9 t9)
and deserialiser0 () s = 
  (), s
and deserialiser1 (d1) s =
  let s1, rest = d1 s in
    (s1), rest
and deserialiser2 (d1, d2) s =
  let s1, rest = d1 s in
  let s2, rest = d2 rest in 
    (s1, s2), rest
and deserialiser3 (d1, d2, d3) s =
  let s1, rest = d1 s in
  let s2, rest = d2 rest in 
  let s3, rest = d3 rest in 
    (s1, s2, s3), rest
and deserialiser4 (d1, d2, d3, d4) s =
  let s1, rest = d1 s in
  let s2, rest = d2 rest in 
  let s3, rest = d3 rest in 
  let s4, rest = d4 rest in 
    (s1, s2, s3, s4), rest
and deserialiser5 (d1, d2, d3, d4, d5) s =
  let s1, rest = d1 s in
  let s2, rest = d2 rest in 
  let s3, rest = d3 rest in 
  let s4, rest = d4 rest in 
  let s5, rest = d5 rest in 
    (s1, s2, s3, s4, s5), rest
and deserialiser6 (d1, d2, d3, d4, d5, d6) s =
  let s1, rest = d1 s in
  let s2, rest = d2 rest in 
  let s3, rest = d3 rest in 
  let s4, rest = d4 rest in 
  let s5, rest = d5 rest in 
  let s6, rest = d6 rest in 
    (s1, s2, s3, s4, s5, s6), rest
and deserialiser7 (d1, d2, d3, d4, d5, d6, d7) s =
  let s1, rest = d1 s in
  let s2, rest = d2 rest in 
  let s3, rest = d3 rest in 
  let s4, rest = d4 rest in 
  let s5, rest = d5 rest in 
  let s6, rest = d6 rest in 
  let s7, rest = d7 rest in 
    (s1, s2, s3, s4, s5, s6, s7), rest
and deserialiser8 (d1, d2, d3, d4, d5, d6, d7, d8) s =
  let s1, rest = d1 s in
  let s2, rest = d2 rest in 
  let s3, rest = d3 rest in 
  let s4, rest = d4 rest in 
  let s5, rest = d5 rest in 
  let s6, rest = d6 rest in 
  let s7, rest = d7 rest in 
  let s8, rest = d8 rest in 
    (s1, s2, s3, s4, s5, s6, s7, s8), rest
and deserialiser9 (d1, d2, d3, d4, d5, d6, d7, d8, d9) s =
  let s1, rest = d1 s in
  let s2, rest = d2 rest in 
  let s3, rest = d3 rest in 
  let s4, rest = d4 rest in 
  let s5, rest = d5 rest in 
  let s6, rest = d6 rest in 
  let s7, rest = d7 rest in 
  let s8, rest = d8 rest in 
  let s9, rest = d9 rest in 
    (s1, s2, s3, s4, s5, s6, s7, s8, s9), rest

let deserialise0 = fun x y -> fst (deserialiser0 x y)
and deserialise1 = fun x y -> fst (deserialiser1 x y)
and deserialise2 = fun x y -> fst (deserialiser2 x y)
and deserialise3 = fun x y -> fst (deserialiser3 x y)
and deserialise4 = fun x y -> fst (deserialiser4 x y)
and deserialise5 = fun x y -> fst (deserialiser5 x y)
and deserialise6 = fun x y -> fst (deserialiser6 x y)
and deserialise7 = fun x y -> fst (deserialiser7 x y)
and deserialise8 = fun x y -> fst (deserialiser8 x y)
and deserialise9 = fun x y -> fst (deserialiser9 x y)

let serialise_list (serialiser : 'a serialiser) (list : 'a list)
    = serialise_string (String.concat "" (map serialiser list))
and deserialise_list (deserialiser : 'a deserialiser) : 'a list deserialiser =
  fun s -> 
    let items, rest = deserialise_string s in
    let rec extract items = function
      | "" -> rev items, rest
      | s -> let item, rest = deserialiser s in
          extract (item :: items) rest
    in extract [] items

let enumeration_serialisers (pairs : ('a * char) list) : ('a serialiser * 'a deserialiser) =
  let serialiser : 'a serialiser = fun a -> String.make 1 (assoc a pairs)
  and deserialiser : 'a deserialiser = 
    fun s -> (rassoc s.[0] pairs, String.sub s 1 (String.length s - 1))
  in serialiser, deserialiser

let make_deserialiser fn = 
  fun s -> 
    let t, obj, rest = extract_object s in
    let e = fn obj t in
      e, rest
