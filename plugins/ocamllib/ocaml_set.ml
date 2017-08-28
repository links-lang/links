open Value
open Types
open Utility
open Proc
open Lib

let compare a b = 
  if a < b then (-1)
  else if a > b then 1
  else 0

let rcompare a b = 
  if a > b then (-1)
  else if a < b then 1
  else 0

let _ = 
	print_endline "adding Ocaml Set module functions" ;
	let new_env : (string * (located_primitive * Types.datatype * pure)) list = [


  	] in
	env := List.append (!env) new_env ;;