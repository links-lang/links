open Printf
open Utility

open QrToAlgebraDefinitions

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
and vsr = ((int * nativeint) * (tblresult * implementation_type)) list
and tblresult = Tr of (accessor_functions * Cs.t * tsr * vsr)

(* Create functions which encapsulate the access to one table's 
   item and iter fields. *)
let table_access_functions (iter_schema_name, offsets_and_schema_names) dbvalue : accessor_functions = 
  let result_fields = fromTo 0 dbvalue#nfields in
  let result_names = List.map (fun i -> (dbvalue#fname i, i)) result_fields in
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
    let item row offset = 
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
  let schema, query = XmlSqlPlan.extract_queries xml_sql in
  let result = execute_query database query in
  let (item, _, ntuples) = table_access_functions schema result in
    if ntuples > 0 then
      Some (item 0 1)
    else 
      None

let mk_primitive raw_value t = 
  match t with
    | `IntType -> Value.box_int (Num.num_of_string raw_value)
    | `StrType -> Value.box_string raw_value
    | `BoolType -> Value.box_bool ((int_of_string raw_value) = 1)
    | `CharType -> Value.box_char (String.get raw_value 0)
    | `FloatType -> (if raw_value = "" then Value.box_float 0.00      (* HACK HACK *)
                     else Value.box_float (float_of_string raw_value))
    | `Unit -> `Record []
    | `EmptyRecord -> `Record []
    | `EmptyListLit -> `List []
    | `NatType -> failwith "Heapresult.mk_primitive: nat is not a Links type"
    | `Tag 
    | `Surrogate -> assert false 

let lookup itype m key =
  match IntMap.lookup key m with
    | Some valuelist -> 
	begin
	  match itype with
	    | `List -> `List (List.rev valuelist)
	    | `Atom -> 
		assert ((List.length valuelist) = 1);
		List.hd valuelist
	end
    | None -> `List []

let append key value m =
  match IntMap.lookup key m with
    | Some valuelist -> IntMap.add key (value :: valuelist) m
    | None -> IntMap.add key [value] m

let rec value_from_row item cs tsm vsm keytags = 
  match cs with
    | Cs.Column (col, `Surrogate) ->
	let m = 
	  try
	    List.assoc col tsm
	  with NotFound _ -> assert false
	in
	let surrogate_key = int_of_string (item col) in
	  lookup `List m surrogate_key
    | Cs.Column (col, t) ->
	  mk_primitive (item col) t
    | Cs.Mapping fields ->
	let field (field_name, field_cs) =
	  (field_name, value_from_row item field_cs tsm vsm keytags)
	in
	  `Record (List.map field fields)
    | Cs.Tag ((tagcol, `Tag), (refcol, `Surrogate)) ->
	let keyval = Nativeint.of_string (item tagcol) in
	let tagval = IntMap.find (Nativeint.to_int keyval) keytags in
	let surrogate_key = int_of_string (item refcol) in
	let (m, itype) = 
	  try
	    List.assoc (refcol, keyval) vsm
	  with NotFound _ -> assert false
	in
	let tagged_value = lookup itype m surrogate_key in
	  `Variant (tagval, tagged_value)
    | _ -> assert false

and value_from_table (Tr ((item, _, nr_tuples), cs, tsr, vsr)) result_type keytags = 
  let tsm = List.map (fun (col, tr) -> (col, valmap tr keytags)) tsr in
  let vsm = List.map (fun (k, (tr, itype)) -> (k, (valmap tr keytags, itype))) vsr in 
  match result_type with
    | `Atom ->
	assert (nr_tuples = 1);
	value_from_row (item 0) cs tsm vsm keytags
    | `List ->
      let rec loop_tuples i row_values =
	if i = nr_tuples then
	  List.rev row_values
	else
	  let row_value = value_from_row (item i) cs tsm vsm keytags in
	    loop_tuples (i + 1) (row_value :: row_values) 
      in
	`List (loop_tuples 0 [])

and valmap (Tr ((item, iter, nr_tuples), cs, tsr, vsr)) keytags =
  let tsm = List.map (fun (col, tr) -> (col, valmap tr keytags)) tsr in
  let vsm = List.map (fun (k, (tr, itype)) -> (k, (valmap tr keytags, itype))) vsr in 
  let rec loop_tuples i m =
    if i = nr_tuples then
      m
    else
      let iter_val = int_of_string (iter i) in
      let value = value_from_row (item i) cs tsm vsm keytags in
	loop_tuples (i + 1) (append iter_val value m)
  in
    loop_tuples 0 IntMap.empty

exception ErrorExc of string

type outcome = Result of Value.t | Error of string

let execute db imptype (result_algebra_bundle, error_plans, keytags) =
  let error q =
    if Settings.get_value Basicsettings.Ferry.check_error_plans then
      match execute_errors db q with 
	| Some errorstring -> raise (ErrorExc errorstring)
	| None -> ()
    else
      ()
  in
    try
      List.iter error error_plans;
      let result_bundle = execute_queries db result_algebra_bundle in
	Result (value_from_table result_bundle imptype keytags)
    with
	ErrorExc s -> Error s
