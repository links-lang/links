open Value
open Types
open Utility
open Proc
open Lib

type st = Value.t Stack.t

let stack_num : int ref = ref 0
let stack_container : (int, st) Hashtbl.t = Hashtbl.create 10
let add_stack key stack = Hashtbl.add stack_container key stack
let find_stack key = 
	try Hashtbl.find stack_container key
	with NotFound s -> print_endline s; flush stdout; assert false

let print_hashtbl () = 
	Hashtbl.iter (fun a b -> 
		print_int a; print_endline ""; print_int (Stack.length b); print_endline "")
		stack_container 
		
let to_record : int -> Value.t = fun k ->
	`Record [("container", (box_string "Stack")); ("no", (box_int k))]

let to_key : Value.t -> int = fun r ->
	let l = unbox_record r in 
	unbox_int (List.assoc "no" l)

let _ = 
	print_endline "adding Ocaml Stack module functions" ;
	let new_env : (string * (located_primitive * Types.datatype * pure)) list = [

	"stack_create",
	(`PFun (fun _ _ -> (
		let s = Stack.create () in
		add_stack !stack_num s; 
		stack_num := !stack_num + 1;
		to_record (!stack_num - 1))),
	datatype "() -> (container:String,no:Int)",
	IMPURE);

	"stack_push",
	(p2 (fun ele sn -> 
		let k = (to_key sn) in
		let s = (find_stack k) in
        Stack.push ele s; 
    	`Record []),
	datatype "(a, (container:String,no:Int)) -> ()",
	IMPURE);

	"stack_pop",
	(p1 (to_key ->- find_stack ->- Stack.pop),
	datatype "((container:String,no:Int)) -> a",
	IMPURE);

	"stack_top",
	(p1 (to_key ->- find_stack ->- Stack.top),
	datatype "((container:String,no:Int)) -> a",
	IMPURE);

	"stack_clear",
	(p1 (fun sn -> 
		let k = to_key sn in
		let s = find_stack k in 
		Stack.clear s;
		`Record []),
	datatype "((container:String,no:Int)) -> ()",
	IMPURE);

	"stack_copy",
	(p1 (fun sn -> 
		let k = to_key sn in
		let s = find_stack k in 
		add_stack !stack_num (Stack.copy s); 
		stack_num := !stack_num + 1;
		to_record (!stack_num - 1)),
	datatype "((container:String,no:Int)) -> (container:String,no:Int)",
	IMPURE);

	"stack_is_empty",
	(p1 (fun sn ->
		let k = (to_key sn) in
		let s = (find_stack k) in
		box_bool (Stack.is_empty s)),
	datatype "((container:String,no:Int)) -> Bool",
	IMPURE);

	"stack_length",
	(p1 (to_key ->- find_stack ->- Stack.length ->- box_int),
	datatype "((container:String,no:Int)) -> Int",
	IMPURE);
	
  	] in
	env := List.append (!env) new_env ;;