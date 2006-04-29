type 'a serialiser = 'a -> string
type 'a deserialiser = string -> ('a * string)

(* Take a string represeting a sequence of serialised objects, return
   the type of the first object, the string containing the object
   itself (minus type and length header) and the rest of the string *)
val extract_object : string -> char * string * string

val serialise_string : string serialiser
val deserialise_string : string deserialiser
val serialise_bool : bool serialiser
val deserialise_bool : bool deserialiser
val serialise_int : Num.num serialiser
val deserialise_int : Num.num deserialiser
val serialise_oint : int serialiser
val deserialise_oint : int deserialiser
val serialise_char : char serialiser
val deserialise_char : char deserialiser
val serialise_float : float serialiser
val deserialise_float : float deserialiser

val null_serialiser : 'a serialiser
val null_deserialiser : 'a -> 'a deserialiser

val serialise_option : 'a serialiser -> 'a option serialiser
val deserialise_option : 'a deserialiser -> 'a option deserialiser

(* Why, /yes/, I /would/ like generic tuple facilities. Why do you ask? *)
val serialise0 : char -> unit -> unit -> string
val serialise1 : char -> 'a serialiser -> 'a -> string
val serialise2 : char -> ('a serialiser * 'b serialiser) -> ('a * 'b) -> string
val serialise3 : char -> ('a serialiser * 'b serialiser * 'c serialiser) -> ('a * 'b * 'c) -> string
val serialise4 : char -> ('a serialiser * 'b serialiser * 'c serialiser * 'd serialiser) -> ('a * 'b * 'c * 'd) -> string
val serialise5 : char -> ('a serialiser * 'b serialiser * 'c serialiser * 'd serialiser * 'e serialiser) -> ('a * 'b * 'c * 'd * 'e) -> string
val serialise6 : char -> ('a serialiser * 'b serialiser * 'c serialiser * 'd serialiser * 'e serialiser* 'f serialiser) -> ('a * 'b * 'c * 'd * 'e * 'f) -> string
val serialise7 : char -> ('a serialiser * 'b serialiser * 'c serialiser * 'd serialiser * 'e serialiser * 'f serialiser * 'g serialiser) -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) -> string
(*
val serialise8
val serialise9
*)

(*
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
*)

val deserialiser0 : unit -> string -> (unit * string)
val deserialiser1 : ('a deserialiser) -> string -> ('a * string)
val deserialiser2 : ('a deserialiser * 'b deserialiser) -> string -> (('a * 'b) * string)
val deserialiser3 : ('a deserialiser * 'b deserialiser * 'c deserialiser) -> string -> (('a * 'b * 'c) * string)
val deserialiser4 : ('a deserialiser * 'b deserialiser * 'c deserialiser * 'd deserialiser) -> string -> (('a * 'b * 'c * 'd) * string)
val deserialiser5 : ('a deserialiser * 'b deserialiser * 'c deserialiser * 'd deserialiser * 'e deserialiser) -> string -> (('a * 'b * 'c * 'd * 'e) * string)

val deserialise0 : unit -> string -> unit
val deserialise1 : ('a deserialiser) -> string -> 'a
val deserialise2 : ('a deserialiser * 'b deserialiser) -> string -> ('a * 'b)
val deserialise3 : ('a deserialiser * 'b deserialiser * 'c deserialiser) -> string -> ('a * 'b * 'c)
val deserialise4 : ('a deserialiser * 'b deserialiser * 'c deserialiser * 'd deserialiser) -> string -> ('a * 'b * 'c * 'd)
val deserialise5 : ('a deserialiser * 'b deserialiser * 'c deserialiser * 'd deserialiser * 'e deserialiser) -> string -> ('a * 'b * 'c * 'd * 'e)
val deserialise6 : ('a deserialiser * 'b deserialiser * 'c deserialiser * 'd deserialiser * 'e deserialiser * 'f deserialiser) -> string -> ('a * 'b * 'c * 'd * 'e * 'f)
val deserialise7 : ('a deserialiser * 'b deserialiser * 'c deserialiser * 'd deserialiser * 'e deserialiser * 'f deserialiser * 'g deserialiser) -> string -> ('a * 'b * 'c * 'd * 'e * 'f * 'g)
(*
val deserialise8 : ('a deserialiser) -> string -> string
val deserialise9 : ('a deserialiser) -> string -> string
*)
val serialise_list : 'a serialiser -> 'a list serialiser
val deserialise_list : 'a deserialiser -> 'a list deserialiser

val enumeration_serialisers : ('a * char) list -> ('a serialiser * 'a deserialiser)

(*
val make_deserialiser : ?
*)

