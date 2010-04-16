open Utility

module A = Algebra
module FieldEnv = Utility.StringMap

exception Runtime_error = Errors.Runtime_error

type tree = E of Xmlm.tag * tree list | D of string

let in_tree i = 
  let el tag childs = E (tag, childs)  in
  let data d = D d in
    Xmlm.input_doc_tree ~el ~data i

let filter_whitespace_els l =
  List.filter (function E _ -> true | D _ -> false) l

let find_el elements name =
  let p = function
    | E (((_, elname), _), _) -> name = elname
    | _ -> false
  in
    List.find p elements

let lookup_attr attributes name =
  let p ((_, n), _) = n = name in
    snd (List.find p attributes)

let collect_column = function
  | E (((_, "column"), attrs), []) ->
      let name = lookup_attr attrs "name" in
      let funct = lookup_attr attrs "function" in
	if funct = "item" then
	  let pos = 1 + (int_of_string (lookup_attr attrs "position")) in
	    `Item (pos, name)
	else if funct = "iter" then
	  `Iter (name)
	else
	  assert false
  | _ -> assert false

let collect_schema = function
  | E (((_, "schema"), _), childs) ->
      let cols = List.map collect_column (filter_whitespace_els childs) in
      let rec iter l = 
	(match l with
	   | (`Iter name) :: _ -> name
	   | (`Item _) :: xs -> iter xs
	   | [] -> assert false)
      in
      let rec item l =
	(match l with
	   | (`Iter _) :: xs -> item xs
	   | (`Item (pos, name)) :: xs -> (pos, name) :: (item xs)
	   | [] -> [])
      in
	(iter cols, item cols)
  | _ -> assert false

let collect_query = function
  | E (((_, "query"), _), [D query]) -> query
  | _ -> assert false

let collect_plan = function
  | E (((_, "query_plan"), attrs), childs) ->
      (try 
	let id = int_of_string (lookup_attr attrs "id") in
	let schema = collect_schema (find_el childs "schema") in
	let query = collect_query (find_el childs "query") in
	  (id, (schema, query))
      with NotFound _ -> 
	Debug.print "List.assoc";
	assert false)
  | _ -> assert false


let collect_plans = function
  | E (((_, "query_plan_bundle"), []), childs) ->
      List.map collect_plan (filter_whitespace_els childs)
  | _ -> assert false

exception ColumnMappingError of string
exception ItemAccessError of int

let rec is_record_type t = 
  match TypeUtils.concrete_type t with
    | `Record _ -> true
    | _ -> false

type accessor_functions = (int -> int -> string) * (int -> string) * int
type table_struct = Table of (accessor_functions * Cs.cs * ((int * table_struct) list))

(* This function creates functions which encapsulate the access to one table's 
   item and iter fields. 
   offsets_and_schema_names: maps Cs offsets to schema names from the XML plan bundle
   iter_schema_name: schema name for the iter column from the XML plan bundle.
   dbresult: representation of the table to be encapsulated *)
let table_access_functions (iter_schema_name, offsets_and_schema_names) dbvalue : accessor_functions = 
  let result_fields = fromTo 0 dbvalue#nfields in
  let result_names = List.map (fun i -> (dbvalue#fname i, i)) result_fields in
  let find_field col_name = 
    try
      let startswith s1 s2 = 
	let len = String.length s2 in
	  (String.sub s1 0 len) = s2
      in
      let pred (name, _) = startswith name col_name in
	snd (List.find pred result_names)
    with NotFound _ -> 
      raise (ColumnMappingError col_name)
  in
  let iter_field = find_field iter_schema_name in
  let offsets_to_fields = 
    List.map 
      (fun (offset, schema_name) ->
	 (offset, find_field schema_name))
      offsets_and_schema_names
  in
  let item row offset = 
    try 
      let field = List.assoc offset offsets_to_fields in
	dbvalue#getvalue row field
    with Not_found -> raise (ItemAccessError offset)
  in
  let iter row =
    dbvalue#getvalue row iter_field
  in
    (item, iter, dbvalue#ntuples)

let execute_query database query id =
  Debug.print ("executing query " ^ (string_of_int id));
  let dbresult = (database#exec query) in
    match dbresult#status with
      | `QueryError msg -> 
	  raise(Runtime_error("An error occurred executing the query " ^ query ^
                                ": " ^ msg))
      | _ -> dbresult

let reconstruct_itbls root_acc root_cs result_bundle : table_struct =
  let table_map = 
    List.fold_left
      (fun m (id, _, acc, _, cs) ->
	 IntMap.add id (Table (acc, cs, [])) m)
      IntMap.empty
      result_bundle
  in
  let table_map = IntMap.add 0 (Table (root_acc, root_cs, [])) table_map in
  let table_map_itbls =
    List.fold_left
      (fun m (id, (refid, refcol), _, _, _) ->
	 Debug.print (Printf.sprintf "%d.%d -> %d" refid refcol id);
	 let Table (acc, cs, itbls) = IntMap.find refid m in
	 let inner = IntMap.find id m in
	 let m = IntMap.add refid (Table (acc, cs, ((refcol, inner) :: itbls))) m in
	   IntMap.remove id m)
      table_map
      result_bundle
  in
    assert ((IntMap.size table_map_itbls) = 1);
    IntMap.find 0 table_map_itbls

let transform_and_execute database xml_sql_bundle algebra_bundle = 
  Debug.print "transform_and_execute";
  let (_, root_cs, sub_algebra_plans) = algebra_bundle in
  let xml_input = Xmlm.make_input (`String (0, xml_sql_bundle)) in
  let sql_plans = collect_plans (snd (in_tree xml_input)) in
  let rec merge sql_bundle =
    match sql_bundle with
      | (0, _) :: sql_bundle -> merge sql_bundle
      | (id, (schema, query)) :: sql_bundle ->
	  (try
	    let (refs, _, cs) = List.assoc id sub_algebra_plans in
	      (id, refs, query, schema, cs) :: (merge sql_bundle)
	  with NotFound _ -> assert false)
      | [] -> []
  in
  let (root_schema, root_query) =
    try
      List.assoc 0 sql_plans
    with Not_found -> assert false
  in
  let plan_bundle = merge sql_plans in
  let result_bundle = 
    List.map
      (fun (id, refs, query, schema, cs) ->
	 let dbresult = execute_query database query id in
	   Debug.print ("nr " ^ (string_of_int dbresult#ntuples));
	   (id, refs, (table_access_functions schema dbresult), schema, cs))
      plan_bundle
  in
  let root_result = execute_query database root_query 0 in
  let (_, _, nr) as root_access = table_access_functions root_schema root_result in
    Debug.print ("nr_root " ^ (string_of_int nr));
    reconstruct_itbls root_access root_cs result_bundle

let mk_primitive : string -> Types.datatype -> Value.t = Database.value_of_db_string 

let rec mk_record record_type cs (item : int -> string) itbls : Value.t =
  let fieldspecs = 
    match TypeUtils.concrete_type record_type with
      | `Record (field_map, _) -> field_map
      | _ -> failwith "mk_record called for non-record type"
  in
   let mk_field field_name (_, field_type) record =
     let field_cs = Cs.lookup_record_field cs field_name in
     let value = handle_row field_type item field_cs itbls in
       (field_name, value) :: record
   in
   let values = FieldEnv.fold mk_field fieldspecs [] in
     `Record values

and mk_innerlist = ()

and handle_row typ item cs itbls = 
  match TypeUtils.concrete_type typ with
    | `Primitive _ -> 
	let col =
	  (match cs with
	    | [Cs.Offset (i, _)] -> i
	    | _ -> failwith "Heapresult.handle_row: cs does not represent a primitive value")
	in
	let raw_value = item col in
	  mk_primitive raw_value typ
    | `Record _ as record_type -> 
	mk_record record_type cs item itbls
    | `Application (l, [`Type _element_type])
	when Eq.eq Types.Abstype.eq_t l Types.list ->
	failwith "Heapresult.handle_row: inner lists not implemented"
    | t -> failwith ("Heapresult.handle_row: type not supported: " ^
		       Show.show Types.show_datatype t)

and handle_table t (Table ((item, _, nr_tuples), cs, itbls)) = 
  match t with
    | t when (Types.is_base_type t || is_record_type t) ->
	assert (nr_tuples = 1);
	handle_row t (item 0) cs itbls
    | `Application (l, [`Type element_type])
	when Eq.eq Types.Abstype.eq_t l Types.list ->
	let rec loop_tuples i row_values =
	  if i = nr_tuples then
	    List.rev row_values
	  else
	    let row_value = handle_row element_type (item i) cs itbls in
	      loop_tuples (i + 1) (row_value :: row_values)
	in
	  `List (loop_tuples 0 [])
    | t -> failwith ("Heapresult.handle_table: type not supported: " ^
		       Show.show Types.show_datatype t)
	
(* and handle_inner_table t (nr_tuples, iter, item) cs surr_key = *)

