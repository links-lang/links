open Value
open Types
open Utility
open Proc
open Lib

let _ = 
	print_endline "adding Ocaml Char module functions" ;
	let new_env : (string * (located_primitive * Types.datatype * pure)) list = [
	"char_code",
	(p1 (unbox_char ->- Char.code ->- box_int),
	datatype "(Char) -> Int",
	PURE);

	"char_chr",
	(p1 (unbox_int ->- Char.chr ->- box_char),
	datatype "(Int) -> Char",
	PURE);

	"char_escaped",
	(p1 (unbox_char ->- Char.escaped ->- box_string),
	datatype "(Char) -> String",
	PURE);

	"char_lowercase",
	(p1 (unbox_char ->- Char.lowercase_ascii ->- box_char),
	datatype "(Char) -> Char",
	PURE);

	"char_uppercase",
	(p1 (unbox_char ->- Char.uppercase_ascii ->- box_char),
	datatype "(Char) -> Char",
	PURE);

  	] in
	env := List.append (!env) new_env ;;