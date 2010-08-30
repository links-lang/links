open Printf
open Utility

open CompileQuery
open ExpressionToAlgebra

module A = Algebra
module FieldEnv = Utility.StringMap

exception Runtime_error = Errors.Runtime_error

type implementation_type = [ `List | `Atom ]

module XmlSqlPlan : sig
  val extract_queries : string -> (string * (int * string) list) * string
end =
struct
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
    | E (((_, "query_plan"), _attrs), childs) ->
	(try 
	   let schema = collect_schema (find_el childs "schema") in
	   let query = collect_query (find_el childs "query") in
	     (schema, query)
	 with NotFound _ -> 
	   assert false)
    | _ -> assert false

  let collect_plans = function
    | E (((_, "query_plan_bundle"), []), childs) ->
	let plans = List.map collect_plan (filter_whitespace_els childs) in
	  assert ((List.length plans) = 1);
	  List.hd plans
    | _ -> assert false

  let extract_queries xmlstring =
    let xml_input = Xmlm.make_input (`String (0, xmlstring)) in
      collect_plans (snd (in_tree xml_input))
end

exception ColumnMappingError of string
exception ItemAccessError of int

type item_access = int -> int -> string
type iter_access = int -> string
type cardinality = int

type accessor_functions = item_access * iter_access * cardinality

type tsr = (int * tblresult) list
and vsr = ((int * string) * (tblresult * implementation_type)) list
and tblresult = Tr of (accessor_functions * Cs.t * tsr * vsr)

(* Create functions which encapsulate the access to one table's 
   item and iter fields. *)
let table_access_functions (iter_schema_name, offsets_and_schema_names) dbvalue : accessor_functions = 
  let result_fields = fromTo 0 dbvalue#nfields in
  let result_names = List.map (fun i -> (dbvalue#fname i, i)) result_fields in
   (* List.iter (fun (s, i) -> Debug.f "%s = %d " s i) result_names;
    Debug.print ""; *)
  let nr_tuples = dbvalue#ntuples in
  let nr_fields = dbvalue#nfields in
  let find_field col_name = 
    try
      let startswith s1 s2 = 
	let len = String.length s2 in
	  (String.sub s1 0 len) = s2
      in
      let pred (name, _) = startswith name (col_name ^ "_") in
	snd (List.find pred result_names)
    with NotFound _ -> 
      raise (ColumnMappingError col_name)
  in
  let iter_field = find_field iter_schema_name in
    assert (iter_field < nr_fields);
    let offsets_to_fields = 
      List.map 
	(fun (offset, schema_name) ->
	   (offset, find_field schema_name))
	offsets_and_schema_names
    in
      (*let foo = List.map (fun (offset, col) -> sprintf "(%d -> %d)" offset col) offsets_to_fields in 
      Debug.print (mapstrcat " " (fun x -> x) foo);  *)
    let item row offset = 
      (* Debug.f "item access %d\n" offset; *)
      try 
	assert (row < nr_tuples);
	let field = List.assoc offset offsets_to_fields in
	  assert (field < nr_fields);
	  dbvalue#getvalue row field
      with Not_found -> raise (ItemAccessError offset)
    in
    let iter row =
      assert (row < nr_tuples);
      dbvalue#getvalue row iter_field
    in
      (item, iter, dbvalue#ntuples)

let execute_query database query =
  if Settings.get_value Basicsettings.Ferry.print_sql_queries then
    Debug.print (">>>> Executing query\n" ^ query);
  let dbresult = (database#exec query) in
    match dbresult#status with
      | `QueryError msg -> 
	  raise(Runtime_error("An error occurred executing the query " ^ query ^
                                ": " ^ msg))
      | _ -> dbresult

let rec execute_queries database ti =
  let xml_sql = Pf_toolchain.optimize_sql ti.q in
  let schema, query = XmlSqlPlan.extract_queries xml_sql in
  let result = execute_query database query in
  let acc = table_access_functions schema result in
  let ts' = alistmap (execute_queries database) ti.ts in
  let vs' = alistmap (fun (ti, itype) -> (execute_queries database ti, itype)) ti.vs in
    Tr (acc, ti.cs, ts', vs')
    
let rec execute_errors database q_error =
  let xml_sql = Pf_toolchain.optimize_sql q_error in
  let _, query = XmlSqlPlan.extract_queries xml_sql in
  let result = execute_query database query in
    if result#ntuples > 0 then false else true

let mk_primitive raw_value t = 
  match t with
    | `IntType -> Value.box_int (Num.num_of_string raw_value)
    | `StrType -> Value.string_as_charlist raw_value
    | `BoolType -> Value.box_bool ((int_of_string raw_value) = 1)
    | `CharType -> Value.box_char (String.get raw_value 0)
    | `FloatType -> (if raw_value = "" then Value.box_float 0.00      (* HACK HACK *)
                     else Value.box_float (float_of_string raw_value))
    | `Unit -> `Record []
    | `EmptyListLit -> `List []
    | `NatType -> failwith "Heapresult.mk_primitive: nat is not a Links type"
    | `Tag 
    | `Surrogate -> assert false 

module Offsets : sig
  type offsets
  type offset = int

  val lookup_ts_offset : offsets -> int -> offset
  val lookup_vs_offset : offsets -> (int * string) -> offset
  val update_ts_offset : offsets -> int -> offset -> offsets
  val update_vs_offset : offsets -> (int * string) -> offset -> offsets
  val empty : offsets
end = struct

  type offset = int
  type offsets = { ts : (int * offset) list; vs : ((int * string) * offset) list }

  let empty = { ts = []; vs = [] }

  let lookup_ts_offset offsets refcol = 
    try
      List.assoc refcol offsets.ts
    with NotFound _ -> 0

  let lookup_vs_offset offsets (refcol, tag) = 
    try
      List.assoc (refcol, tag) offsets.vs
    with NotFound _ -> 0

  let update_ts_offset offsets refcol new_offset = 
    { offsets with ts = (refcol, new_offset) :: (remove_keys offsets.ts [refcol])}

  let update_vs_offset offsets (refcol, tag) new_offset = 
    { offsets with vs = ((refcol, tag), new_offset) :: (remove_keys offsets.vs [(refcol, tag)])}
end

let rec mk_record itbl_offsets field_names cs (item : int -> string) tsr vsr =
   let mk_field (next_offsets, record) field_name =
     let field_cs = Cs.lookup_record_field cs field_name in
(*       Debug.print (Cs.print cs);
       Debug.print ("mk_record " ^ field_name); *)
     let (new_offsets, value) = handle_row next_offsets item field_cs tsr vsr in
       (new_offsets, ((field_name, value) :: record))
   in
   let (new_offsets, values) = List.fold_left mk_field (itbl_offsets, []) field_names in
     (new_offsets, `Record values)

and handle_row itbl_offsets item cs tsr vsr = 
    match cs with
      | Cs.Column (col, `Surrogate) ->
	  let offset = Offsets.lookup_ts_offset itbl_offsets col in
	  let itbl = 
	    try
	      List.assoc col tsr
	    with NotFound _ -> assert false
	  in
	  let surrogate_key = int_of_string (item col) in
	  let (next_offset, value) = handle_inner_table `List surrogate_key offset itbl in
	  let new_offsets = Offsets.update_ts_offset itbl_offsets col next_offset in
	    (new_offsets, value)
      | Cs.Column (col, t) ->
	  let raw_value = item col in
	    (itbl_offsets, mk_primitive raw_value t)
      | Cs.Mapping fields ->
	let field_names = dom fields in
	  mk_record itbl_offsets field_names cs item tsr vsr
      | Cs.Tag ((tagcol, `Tag), (refcol, `Surrogate)) ->
	(* Debug.print (Cs.show cs); *)
	let tagval = item tagcol in
	(* Debug.f "tagval %s" tagval; *)
	let refval_raw = item refcol in
	(* Debug.f "refval_raw %s" refval_raw; *)
	let refval = int_of_string refval_raw in
	let (itbl, itype) = 
	  try
	    List.assoc (refcol, tagval) vsr
	  with NotFound _ -> assert false
	in
	let offset = Offsets.lookup_vs_offset itbl_offsets (refcol, tagval) in
	let (next_offset, tagged_value) = handle_inner_table itype refval offset itbl in
	let variant = `Variant (tagval, tagged_value) in
	let new_offsets = Offsets.update_vs_offset itbl_offsets (refcol, tagval) next_offset in
	(new_offsets, variant)
      | _ -> assert false

and handle_table (Tr ((item, _, nr_tuples), cs, tsr, vsr)) result_type = 
  (* Debug.print "handle_table"; *)
  (* Debug.print (Cs.print cs); *)
  match result_type with
    | `Atom ->
      assert (nr_tuples = 1);
      snd (handle_row Offsets.empty (item 0) cs tsr vsr)
    | `List ->
      let rec loop_tuples i row_values offsets =
	if i = nr_tuples then
	  (offsets, List.rev row_values)
	else
	  let col_value = item i in
	  let (next_offsets, row_value) = handle_row offsets col_value cs tsr vsr in
	  loop_tuples (i + 1) (row_value :: row_values) next_offsets
      in
      `List (snd (loop_tuples 0 [] Offsets.empty))
	
and handle_inner_table itype surrogate_key offset (Tr ((item, iter, nr_tuples), cs, tsr, vsr)) =
  (* Debug.f "handle_inner_table surr_key %d offset %d nr_tuples %d" surrogate_key offset nr_tuples; *)
  (* Debug.print (Cs.show cs); *)
  let rec loop_tuples i row_values inner_offsets =
    (* Debug.f "loop_tuples i %d" i; *)
    if i = nr_tuples then
      if i = 0 then
	(0, (List.rev row_values))
      else
	(i - 1), (List.rev row_values)
    else
      let iter_val = int_of_string (iter i) in
	if surrogate_key < iter_val then
	  if i = 0 then
	    (0, (List.rev row_values))
	  else
	    (i - 1), (List.rev row_values)
	else if surrogate_key = iter_val then
	  let (new_inner_offsets, row_value) = handle_row inner_offsets (item i) cs tsr vsr in
	    loop_tuples (i + 1) (row_value :: row_values) new_inner_offsets
	else
	  loop_tuples (i + 1) row_values inner_offsets
  in
  let (next_offset, values) = loop_tuples offset [] Offsets.empty in
    match itype with
      | `Atom -> 
	  assert ((List.length values) = 1);
	  (next_offset, List.hd values)
      | `List ->
	  (next_offset, `List values)

let execute db imptype (result_algebra_bundle, error_bundle) =
  if opt_app (execute_errors db) true error_bundle then
    let result_bundle = execute_queries db result_algebra_bundle in
    Some (handle_table result_bundle imptype)
  else
    None
