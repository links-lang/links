open Value
open Types
open Utility
open Proc
open Lib

let _ = 
	print_endline "adding Ocaml Printf module functions" ;
	let new_env : (string * (located_primitive * Types.datatype * pure)) list = [

  	] in
	env := List.append (!env) new_env ;;