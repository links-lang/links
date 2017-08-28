open Value
open Types
open Utility
open Proc
open Lib

let p5D fn =
  `PFun (fun req_data args ->
      match args with
        | [a;b;c;d;e] -> fn a b c d e req_data
        | _ -> assert false)

let p5 fn = p5D (fun x y z w u _ -> fn x y z w u)

let _ = 
	print_endline "adding Ocaml String module functions" ;
	let new_env : (string * (located_primitive * Types.datatype * pure)) list = [
    "string_length",
    (p1 (unbox_string ->- String.length ->- box_int),
    datatype "(String) -> Int",
    PURE);

    "string_get",
    (p2 (fun str i -> 
      box_char (String.get (unbox_string str) (unbox_int i))),
    datatype "(String, Int) -> Char",
    PURE);

    "string_make",
    (p2 (fun i ch ->
      box_string (String.make (unbox_int i) (unbox_char ch))),
    datatype "(Int, Char) -> String",
    PURE);

    "string_sub",
    (p3 (fun str s len ->
      box_string (String.sub (unbox_string str) (unbox_int s) (unbox_int len))),
    datatype "(String, Int, Int) -> String",
    PURE);

    "string_blit",
    (p5 (fun src soff dst doff len ->
      (let bdst = Bytes.of_string (unbox_string dst) in
        let () = String.blit (unbox_string src) (unbox_int soff) bdst 
        (unbox_int doff) (unbox_int len) in
        box_string (Bytes.to_string bdst))),
    datatype "(String, Int, String, Int, Int) -> String",
    PURE);

    "string_concat",
    (p2 (fun sep sl -> 
      box_string (String.concat (unbox_string sep) (List.map unbox_string (unbox_list sl)))),
    datatype "(String, [String]) -> String",
    PURE);

    "string_trim",
    (p1 (unbox_string ->- String.trim ->- box_string),
    datatype "(String) -> String",
    PURE);

    "string_escaped",
    (p1 (unbox_string ->- String.escaped ->- box_string),
    datatype "(String) -> String",
    PURE);

    "string_index",
    (p2 (fun s c -> 
      box_int (String.index (unbox_string s) (unbox_char c))),
    datatype "(String, Char) -> Int",
    PURE);

    "string_rindex",
    (p2 (fun s c -> 
      box_int (String.rindex (unbox_string s) (unbox_char c))),
    datatype "(String, Char) -> Int",
    PURE);

    "string_index_from",
    (p3 (fun s i c -> 
      box_int (String.index_from (unbox_string s) (unbox_int i) (unbox_char c))),
    datatype "(String, Int, Char) -> Int",
    PURE);

    "string_rindex_from",
    (p3 (fun s i c -> 
      box_int (String.rindex_from (unbox_string s) (unbox_int i) (unbox_char c))),
    datatype "(String, Int, Char) -> Int",
    PURE);

    "string_contains",
    (p2 (fun s c -> 
      box_bool (String.contains (unbox_string s) (unbox_char c))),
    datatype "(String, Char) -> Bool",
    PURE);

    "string_contains_from",
    (p3 (fun s i c -> 
      box_bool (String.contains_from (unbox_string s) (unbox_int i) (unbox_char c))),
    datatype "(String, Int, Char) -> Bool",
    PURE);

    "string_rcontains_from",
    (p3 (fun s i c -> 
      box_bool (String.rcontains_from (unbox_string s) (unbox_int i) (unbox_char c))),
    datatype "(String, Int, Char) -> Bool",
    PURE);

    "string_uppercase",
    (p1 (unbox_string ->- String.uppercase_ascii ->- box_string),
    datatype "(String) -> String",
    PURE);

    "string_lowercase",
    (p1 (unbox_string ->- String.lowercase_ascii ->- box_string),
    datatype "(String) -> String",
    PURE);

    "string_capitalize",
    (p1 (unbox_string ->- String.capitalize_ascii ->- box_string),
    datatype "(String) -> String",
    PURE);

    "string_uncapitalize",
    (p1 (unbox_string ->- String.uncapitalize_ascii ->- box_string),
    datatype "(String) -> String",
    PURE);

    "string_split_on_char",
    (p2 (fun ch s -> 
      box_list (List.map box_string (String.split_on_char (unbox_char ch) (unbox_string s)))),
    datatype "(Char, String) -> [String]",
    PURE);

  	] in
	env := List.append (!env) new_env ;;