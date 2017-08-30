open Value
open Types
open Utility
open Proc
open Lib

type qt = Value.t Queue.t

let queue_num : int ref = ref 0
let queue_container : (int, qt) Hashtbl.t = Hashtbl.create 10
let add_queue key queue = Hashtbl.add queue_container key queue
let find_queue key = 
	try Hashtbl.find queue_container key
	with NotFound s -> print_endline s; flush stdout; assert false

let print_hashtbl () = 
	Hashtbl.iter (fun a b -> 
		print_int a; print_endline ""; print_int (Queue.length b); print_endline "")
		queue_container 
		
let to_record : int -> Value.t = fun k ->
	`Record [("container", (box_string "Queue")); ("no", (box_int k))]

let to_key : Value.t -> int = fun r ->
	let l = unbox_record r in 
	unbox_int (List.assoc "no" l)

let _ = 
	print_endline "adding Ocaml Queue module functions" ;
	let new_env : (string * (located_primitive * Types.datatype * pure)) list = [

	"queue_create",
	(`PFun (fun _ _ -> (
		let q = Queue.create () in
		add_queue !queue_num q; 
		queue_num := !queue_num + 1;
		to_record (!queue_num - 1))),
	datatype "() -> (container:String,no:Int)",
	IMPURE);

	"queue_add",
	(p2 (fun ele qn -> 
		let k = (to_key qn) in
		let q = (find_queue k) in
        Queue.add ele q; 
    	`Record []),
	datatype "(a, (container:String,no:Int)) -> ()",
	IMPURE);

	"queue_push",
	(p2 (fun ele qn -> 
		let k = (to_key qn) in
		let q = (find_queue k) in
        Queue.push ele q; 
    	`Record []),
	datatype "(a, (container:String,no:Int)) -> ()",
	IMPURE);

	"queue_take",
	(p1 (to_key ->- find_queue ->- Queue.take),
	datatype "((container:String,no:Int)) -> a",
	IMPURE);

	"queue_pop",
	(p1 (to_key ->- find_queue ->- Queue.pop),
	datatype "((container:String,no:Int)) -> a",
	IMPURE);

	"queue_peek",
	(p1 (to_key ->- find_queue ->- Queue.peek),
	datatype "((container:String,no:Int)) -> a",
	IMPURE);

	"queue_top",
	(p1 (to_key ->- find_queue ->- Queue.top),
	datatype "((container:String,no:Int)) -> a",
	IMPURE);

	"queue_clear",
	(p1 (fun qn -> 
		let k = to_key qn in
		let q = find_queue k in 
		Queue.clear q;
		`Record []),
	datatype "((container:String,no:Int)) -> ()",
	IMPURE);

	"queue_copy",
	(p1 (fun qn -> 
		let k = to_key qn in
		let q = find_queue k in 
		add_queue !queue_num (Queue.copy q); 
		queue_num := !queue_num + 1;
		to_record (!queue_num - 1)),
	datatype "((container:String,no:Int)) -> (container:String,no:Int)",
	IMPURE);

	"queue_is_empty",
	(p1 (fun qn ->
		let k = (to_key qn) in
		let q = (find_queue k) in
		box_bool (Queue.is_empty q)),
	datatype "((container:String,no:Int)) -> Bool",
	IMPURE);

	"queue_length",
	(p1 (to_key ->- find_queue ->- Queue.length ->- box_int),
	datatype "((container:String,no:Int)) -> Int",
	IMPURE);

	"queue_transfer",
	(p2 (fun qn1 qn2 -> 
		let k1 = to_key qn1 in
		let k2 = to_key qn2 in 
		let q1 = find_queue k1 in
		let q2 = find_queue k2 in
		Queue.transfer q1 q2; 
		`Record []),
	datatype "((container:String,no:Int), (container:String,no:Int)) -> ()",
	IMPURE);

  	] in
	env := List.append (!env) new_env ;;