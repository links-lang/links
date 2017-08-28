open Value
open Types
open Utility
open Proc
open Lib

type ht = (Value.t, Value.t) Hashtbl.t

let hashtbl_num : int ref = ref 0
let hashtbl_container : (int, ht) Hashtbl.t = Hashtbl.create 10
let add_hashtbl key hashtbl = Hashtbl.add hashtbl_container key hashtbl
let find_hashtbl key = 
  try Hashtbl.find hashtbl_container key
  with NotFound s -> print_endline s; flush stdout; assert false

let print_hashtbl () = 
  Hashtbl.iter (fun a b -> 
    print_int a; print_endline ""; print_int (Hashtbl.length b); print_endline "")
    hashtbl_container 

let to_record : int -> Value.t = fun k ->
  `Record [("container", (box_string "Hashtbl")); ("no", (box_int k))]

let to_key : Value.t -> int = fun r ->
  let l = unbox_record r in 
  unbox_int (List.assoc "no" l)

let stats_to_record : Hashtbl.statistics -> Value.t = fun s ->
  `Record [("num_bindings", (box_int s.Hashtbl.num_bindings)); 
           ("num_buckets", (box_int s.Hashtbl.num_buckets)); 
           ("max_bucket_length", (box_int s.Hashtbl.max_bucket_length)); 
           ("bucket_histogram", (box_list (List.map box_int (Array.to_list s.Hashtbl.bucket_histogram))))]

let _ = 
	print_endline "adding Ocaml Hashtbl module functions" ;
	let new_env : (string * (located_primitive * Types.datatype * pure)) list = [
  "hashtbl_create",
  (p2 (fun rand size -> (
    let ht = Hashtbl.create ~random:(unbox_bool rand) (unbox_int size) in
    add_hashtbl !hashtbl_num ht; 
    hashtbl_num := !hashtbl_num + 1;
    to_record (!hashtbl_num - 1))),
  datatype "(Bool, Int) -> (container:String,no:Int)",
  IMPURE);

  "hashtbl_clear",
  (p1 (fun htn -> 
    let k = to_key htn in
    let ht = find_hashtbl k in 
    Hashtbl.clear ht;
    `Record []),
  datatype "((container:String,no:Int)) -> ()",
  IMPURE);

  "hashtbl_reset",
  (p1 (fun htn -> 
    let k = to_key htn in
    let ht = find_hashtbl k in 
    Hashtbl.reset ht;
    `Record []),
  datatype "((container:String,no:Int)) -> ()",
  IMPURE);

  "hashtbl_copy",
  (p1 (fun htn -> 
    let k = to_key htn in
    let ht = find_hashtbl k in 
    add_hashtbl !hashtbl_num (Hashtbl.copy ht); 
    hashtbl_num := !hashtbl_num + 1;
    to_record (!hashtbl_num - 1)),
  datatype "((container:String,no:Int)) -> (container:String,no:Int)",
  IMPURE);

  "hashtbl_add",
  (p3 (fun htn key v -> 
    let k = (to_key htn) in
    let ht = (find_hashtbl k) in
    Hashtbl.add ht key v; 
    `Record []),
  datatype "((container:String,no:Int), a, b) -> ()",
  IMPURE);

  "hashtbl_find",
  (p2 (fun htn key ->
    let k = to_key htn in
    let ht = find_hashtbl k in
    Hashtbl.find ht key),
  datatype "((container:String,no:Int), a) -> b",
  IMPURE);

  "hashtbl_find_all",
  (p2 (fun htn key ->
    let k = to_key htn in
    let ht = find_hashtbl k in
    box_list (Hashtbl.find_all ht key)),
  datatype "((container:String,no:Int), a) -> [b]",
  IMPURE);

  "hashtbl_mem",
  (p2 (fun htn key ->
    let k = to_key htn in
    let ht = find_hashtbl k in
    box_bool (Hashtbl.mem ht key)),
  datatype "((container:String,no:Int), a) -> Bool",
  IMPURE);

  "hashtbl_remove",
  (p2 (fun htn key ->
    let k = to_key htn in
    let ht = find_hashtbl k in
    Hashtbl.remove ht key;
    `Record []),
  datatype "((container:String,no:Int), a) -> ()",
  IMPURE);

  "hashtbl_replace",
  (p3 (fun htn key v ->
    let k = to_key htn in
    let ht = find_hashtbl k in
    Hashtbl.replace ht key v;
    `Record []),
  datatype "((container:String,no:Int), a, b) -> ()",
  IMPURE);

  "hashtbl_length",
  (p1 (to_key ->- find_hashtbl ->- Hashtbl.length ->- box_int),
  datatype "((container:String,no:Int)) -> Int",
  IMPURE);

  "hashtbl_randomize",
  (`PFun (fun _ _ -> 
    Hashtbl.randomize ();
    `Record []),
  datatype "() -> ()",
  IMPURE);

  "hashtbl_is_randomized",
  (`PFun (fun _ _ ->
    box_bool(Hashtbl.is_randomized ())),
  datatype "() -> Bool",
  IMPURE);

  "hashtbl_stats",
  (p1 (fun htn ->
    let k = to_key htn in
    let ht = find_hashtbl k in 
    let s = Hashtbl.stats ht in 
    stats_to_record s),
  datatype "((container:String,no:Int)) -> (bucket_histogram:[Int],max_bucket_length:Int,num_bindings:Int,num_buckets:Int)",
  IMPURE);

 	] in
	env := List.append (!env) new_env ;;