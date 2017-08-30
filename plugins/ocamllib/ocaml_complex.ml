open Value
open Types
open Utility
open Proc
open Lib

let to_complex rcd = 
	match (unbox_record rcd) with
	| [x; y] -> (
		let r = List.assoc "re" [x; y] in
		let i = List.assoc "im" [x; y] in
		{Complex.re = (unbox_float r); Complex.im = (unbox_float i)}
		)
	| _ -> assert false

let to_record c = 
	`Record [("im", (box_float c.Complex.im)); ("re", (box_float c.Complex.re))]

let _ = 
	print_endline "adding Ocaml Complex module functions" ;
	let new_env : (string * (located_primitive * Types.datatype * pure)) list = [
	"complex_gen",
	(p2 (fun r i -> 
		`Record [("re", r); ("im", i)]),
	datatype "(Float, Float) -> (im:Float,re:Float)",
	PURE);

	"complex_zero",
	(to_record Complex.zero,
	datatype "(im:Float,re:Float)",
	PURE);
	
	"complex_one",
	(to_record Complex.one,
	datatype "(im:Float,re:Float)",
	PURE);

	"complex_i",
	(to_record Complex.i,
	datatype "(im:Float,re:Float)",
	PURE);

	"complex_neg",
	(p1 (to_complex ->- Complex.neg ->- to_record),
	datatype "((im:Float,re:Float)) -> (im:Float,re:Float)",
	PURE);

	"complex_conj",
	(p1 (to_complex ->- Complex.conj ->- to_record),
	datatype "((im:Float,re:Float)) -> (im:Float,re:Float)",
	PURE);

	"complex_add",
	(p2 (fun x y ->
		to_record (Complex.add (to_complex x) (to_complex y))),
	datatype "((im:Float,re:Float), (im:Float,re:Float)) -> (im:Float,re:Float)",
	PURE);

	"complex_sub",
	(p2 (fun x y ->
		to_record (Complex.sub (to_complex x) (to_complex y))),
	datatype "((im:Float,re:Float), (im:Float,re:Float)) -> (im:Float,re:Float)",
	PURE);

	"complex_mul",
	(p2 (fun x y ->
		to_record (Complex.mul (to_complex x) (to_complex y))),
	datatype "((im:Float,re:Float), (im:Float,re:Float)) -> (im:Float,re:Float)",
	PURE);

	"complex_inv",
	(p1 (to_complex ->- Complex.inv ->- to_record),
	datatype "((im:Float,re:Float)) -> (im:Float,re:Float)",
	PURE);

	"complex_div",
	(p2 (fun x y ->
		to_record (Complex.div (to_complex x) (to_complex y))),
	datatype "((im:Float,re:Float), (im:Float,re:Float)) -> (im:Float,re:Float)",
	PURE);

	"complex_sqrt",
	(p1 (to_complex ->- Complex.sqrt ->- to_record),
	datatype "((im:Float,re:Float)) -> (im:Float,re:Float)",
	PURE);

	"complex_norm2",
	(p1 (to_complex ->- Complex.norm2 ->- box_float),
	datatype "((im:Float,re:Float)) -> Float",
	PURE);

	"complex_norm",
	(p1 (to_complex ->- Complex.norm ->- box_float),
	datatype "((im:Float,re:Float)) -> Float",
	PURE);

	"complex_arg",
	(p1 (to_complex ->- Complex.arg ->- box_float),
	datatype "((im:Float,re:Float)) -> Float",
	PURE);

	"complex_polar",
	(p2 (fun x y ->
		to_record (Complex.polar (unbox_float x) (unbox_float y))),
	datatype "(Float, Float) -> (im:Float,re:Float)",
	PURE);

	"complex_exp",
	(p1 (to_complex ->- Complex.exp ->- to_record),
	datatype "((im:Float,re:Float)) -> (im:Float,re:Float)",
	PURE);

	"complex_log",
	(p1 (to_complex ->- Complex.log ->- to_record),
	datatype "((im:Float,re:Float)) -> (im:Float,re:Float)",
	PURE);

	"complex_pow",
	(p2 (fun x y ->
		to_record (Complex.pow (to_complex x) (to_complex y))),
	datatype "((im:Float,re:Float), (im:Float,re:Float)) -> (im:Float,re:Float)",
	PURE);

  	] in
	env := List.append (!env) new_env ;;