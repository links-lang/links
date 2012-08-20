open Utility

module A = Algebra
module ADag = Algebra_dag

(* string conversion functions for algebra elements *)

let escape_string s = (* SQL standard for escaping single quotes in a string *)
  Str.global_replace (Str.regexp "'") "''" s

let string_of_constant = function
  | A.Bool value -> string_of_bool value
  | A.Int value -> Num.string_of_num value
  | A.Char c -> "'"^ Char.escaped c ^"'" 
  | A.String s -> escape_string s
  | A.Float value   -> string_of_float value
  | A.Nat n -> Nativeint.to_string n

let string_of_attr_name = function
  | A.Iter 0 -> "iter"
  | A.Iter i -> "iter" ^ (string_of_int i)
  | A.Pos 0 -> "pos"
  | A.Pos i -> "pos" ^ (string_of_int i)
  | A.Item i -> "item" ^ (string_of_int i)

let string_of_sort_direction = function
  | A.Ascending -> "ascending"
  | A.Descending -> "descending"

let string_of_join_comparison = function
  | A.Eq -> "eq"
  | A.Gt -> "gt"
  | A.Ge -> "ge"
  | A.Lt -> "lt"
  | A.Le -> "le"
  | A.Ne -> "ne"

let string_of_pf_type = function
  | `IntType -> "int"
  | `StrType -> "str"
  | `BoolType -> "bool"
  | `CharType -> "str"
  | `FloatType -> "dbl"
  | `NatType -> "nat"

let typestring_of_constant = function
  | A.Float _ -> "dbl"
  | A.Int _ -> "int"
  | A.String _ -> "str"
  | A.Bool _ -> "bool"
  | A.Char _ -> "char"
  | A.Nat _ -> "nat"
    
let string_of_func = function
  | A.Add -> "add"
  | A.Subtract -> "subtract"
  | A.Multiply -> "multiply"
  | A.Divide -> "divide"
  | A.Modulo -> "modulo"
  | A.Contains -> "fn:contains"
  | A.SimilarTo -> "fn:similar_to"
  | A.Concat -> "fn:concat"

let string_of_aggr = function
  | A.Max -> "max"
  | A.Min -> "min"
  | A.Avg -> "avg"
  | A.Sum -> "sum"
  | A.All -> "all"

(* XML helper functions *)

let tag name = ("", name), []

let attr_list xml_attributes = 
  List.map 
    (fun (name, value) -> ("", name), value) 
    xml_attributes

let tag_attr name attributes = ("", name), (attr_list attributes)

let out_el out name xml_attributes =
  out (`El_start (tag_attr name xml_attributes));
  out `El_end

let out_el_childs out name xml_attributes child_fun =
  out (`El_start (tag_attr name xml_attributes));
  child_fun ();
  out `El_end

let out_col out xml_attributes = out_el out "column" xml_attributes

let out_col_childs out xml_attributes child_fun = 
  out_el_childs out "column" xml_attributes child_fun

let id_a i = "id", string_of_int i
let kind_a s = "kind", s

let out_edge out t = out_el out "edge" [("to", string_of_int t)]

let out_arg_pair out (left_arg, right_arg) =
  out_col out [("name", (string_of_attr_name left_arg)); ("new", "false"); ("position", "1")];
  out_col out [("name", (string_of_attr_name right_arg)); ("new", "false"); ("position", "2")]

(* XML output functions for algebra dag elements *)

let out_empty_tbl_info out schema =
  out (`El_start (tag "content"));
  List.iter
    (fun (name, typ) ->
       out_col out [("name", (string_of_attr_name name)); ("type", string_of_pf_type typ); ("new", "true")])
    schema;
  out `El_end

let out_binop_info out (result_attr, arg_pair) =
  out (`El_start (tag "content"));
  out_col out [("name", result_attr); ("new", "true")];
  out_arg_pair out arg_pair;
  out `El_end

let out_sort_infos out l =
  iteri
    (fun i (sort_attr_name, dir) ->
       let xml_attributes = [
	 ("name", (string_of_attr_name sort_attr_name));
	 ("direction", string_of_sort_direction dir);
	 ("position", string_of_int i);
	 ("function", "sort");
	 ("new", "false")]
       in
	 out_col out xml_attributes)
    l

let out_maybe_part_name out maybe_part_name =
  match maybe_part_name with
    | Some part_name ->
	let s = string_of_attr_name part_name in
	  out_col out [("name", s); ("function", "partition"); ("new", "false")]
    | None ->
	()

let out_rownum_info out (res_attr, sort_infos, maybe_part_name) =
  out (`El_start (tag "content"));
  out_col out [("name", (string_of_attr_name res_attr)); ("new", "true")];
  out_sort_infos out sort_infos;
  out_maybe_part_name out maybe_part_name;
  out `El_end

let out_rowid_info out res_attr =
  out (`El_start (tag "content"));
  out_col out [("name", (string_of_attr_name res_attr))];
  out `El_end

let out_rank_info out ((res_attr, sort_infos) : A.rank_info) =
  out (`El_start (tag "content"));
  out_col out [("name", (string_of_attr_name res_attr)); ("new", "true")];
  out_sort_infos out sort_infos;
  out `El_end

let out_project_info out (l : A.project_info) =
  let f (new_attr_name, old_attr_name) = 
    if old_attr_name = new_attr_name then
      out_col out [("name", (string_of_attr_name new_attr_name)); 
		   ("new", "false")]
    else
      out_col out [("name", (string_of_attr_name new_attr_name)); 
		   ("old_name", (string_of_attr_name old_attr_name)); 
		   ("new", "true")]
  in
    out (`El_start (tag "content"));
    List.iter f l;
    out `El_end

let out_select_info out sel_attr_name =
  out (`El_start (tag "content"));
  out_col out [("name", (string_of_attr_name sel_attr_name)); ("new", "false")];
  out `El_end

let out_pos out pos =
  out (`El_start (tag "position"));
  out (`Data (string_of_int pos));
  out `El_end

let out_pos_select_info out ((pos, sort_infos, maybe_part_attr) : A.pos_select_info) =
  out (`El_start (tag "content"));
  out_pos out pos;
  out_sort_infos out sort_infos;
  out_maybe_part_name out maybe_part_attr;
  out `El_end

let out_eqjoin_info out (arg_pair : A.eqjoin_info) =
  out (`El_start (tag "content"));
  out_arg_pair out arg_pair;
  out `El_end

let out_thetajoin_info out (l : A.thetajoin_info) =
  out (`El_start (tag "content"));
  List.iter
    (fun (comp, attributes) ->
       out (`El_start (tag_attr "comparison" [("kind", string_of_join_comparison comp)]));
       out_arg_pair out attributes;
       out `El_end)
    l;
  out `El_end

let out_lit_tbl_info out ((values_per_col, schema_infos) : A.lit_tbl_info) =
  out (`El_start (tag "content"));
  (try 
     List.iter2
       (fun values info ->
	  let c () =
	    List.iter 
	      (fun value ->
		 out_el_childs
		   out 
		   "value" 
		   [("type", typestring_of_constant value)]
		   (fun () -> out (`Data (string_of_constant value))))
	      values
	  in
	    out_col_childs out [("name", (string_of_attr_name (fst info))); ("new", "true")] c)
       values_per_col
       schema_infos
   with Invalid_argument _ -> 
     failwith "out_lit_tbl_info: list lengths do not match");
  out `El_end

let out_attach_info out (result_attr, value) =
  out (`El_start (tag "content"));
  let xml_attrs = [("name", (string_of_attr_name result_attr)); ("new", "true")] in
  let f () = 
    out (`El_start (tag_attr "value" [("type", typestring_of_constant value)]));
    out (`Data (string_of_constant value));
    out `El_end
  in
    out_col_childs out xml_attrs f;
    out `El_end

let out_cast_info out (result_attr, name, base_type) =
  out (`El_start (tag "content"));
  out_col out [("name", (string_of_attr_name result_attr)); ("new", "true")];
  out_col out [("name", (string_of_attr_name name)); ("new", "false")];
  out_el out "type" [("name", string_of_pf_type base_type)];
  out `El_end

let out_binop_info out (result_attr, arg_pair) =
  out (`El_start (tag "content"));
  out_col out [("name", (string_of_attr_name result_attr)); ("new", "true")];
  out_arg_pair out arg_pair;
  out `El_end

let out_unop_info out (result_attr, arg_attr) =
  out (`El_start (tag "content"));
  out_col out [("name", (string_of_attr_name result_attr)); ("new", "true")];
  out_col out [("name", (string_of_attr_name arg_attr)); ("new", "false")];
  out `El_end

let out_fun_1to1_info out (f, result_attr, arg_list) =
  out (`El_start (tag "content"));
  out_el out "kind" [("name", string_of_func f)];
  out_col out [("name", (string_of_attr_name result_attr)); ("new", "true")];
  iteri
    (fun i arg_attr ->
       out_col out [("name", (string_of_attr_name arg_attr)); 
		    ("new", "false"); 
		    ("position", string_of_int i)])
    arg_list;
  out `El_end

(* TODO: aggr is unused. ferryc code does not conform to the wiki spec. *)
let out_fun_aggr_info out info =
  let (_aggr, (result_attr, arg_attr), maybe_part_attr) = info in
    out (`El_start (tag "content"));
    out_col out [("name", (string_of_attr_name result_attr)); ("new", "true")];
    out_col out [("name", (string_of_attr_name arg_attr)); ("new", "false"); ("function", "item")];
    out_maybe_part_name out maybe_part_attr;
    out `El_end
      
let out_fun_aggr_count_info out (result_attr, maybe_part_attr) =
  out (`El_start (tag "content"));
  out_col out [("name", (string_of_attr_name result_attr)); ("new", "true")];
  out_maybe_part_name out maybe_part_attr;
  out `El_end

let out_serialize_rel_info out (iter, pos, items) =
  out (`El_start (tag "content"));
  out_col out [("name", (string_of_attr_name iter)); ("new", "false"); ("function", "iter")];
  out_col out [("name", (string_of_attr_name pos)); ("new", "false"); ("function", "pos")];
  iteri
    (fun i item ->
       out_col out [("name", (string_of_attr_name item)); 
		    ("new", "false"); 
		    ("function", "item"); 
		    ("position", string_of_int i)])
    items;
    out `El_end

let out_tbl_ref_info out (tbl_name, attr_infos, key_infos) =
  out (`El_start (tag "properties"));
  out (`El_start (tag "keys"));
  List.iter
    (fun key ->
       let c () =
	 iteri
	   (fun i attr -> 
	      out_col out [("name", (string_of_attr_name attr)); ("position", string_of_int i)])
	   key
       in
	 out_el_childs out "key" [] c)
    key_infos;
  out `El_end;
  out `El_end;
  out (`El_start (tag "content"));
  let c () =
    List.iter 
      (fun (external_name, internal_name, typ) ->
	 out_col out [("name", (string_of_attr_name external_name)); 
		      ("tname", internal_name); 
		      ("type", string_of_pf_type typ)])
      attr_infos
  in
    out_el_childs out "table" [("name", tbl_name)] c;
    out `El_end

let out_nullary_op out op id =
  match op with
    | A.LitTbl lit_tbl_info ->
	let xml_attrs = [id_a id; kind_a "table"] in
	  out (`El_start (tag_attr "node" xml_attrs));
	  out_lit_tbl_info out lit_tbl_info;
	  out `El_end
    | A.EmptyTbl info ->
	let xml_attrs = [id_a id; kind_a "empty_tbl"] in
	  out (`El_start (tag_attr "node" xml_attrs));
	  out_empty_tbl_info out info;
	  out `El_end
    | A.TblRef info ->
	let xml_attrs = [id_a id; kind_a "ref_tbl"] in
	  out (`El_start (tag_attr "node" xml_attrs));
	  out_tbl_ref_info out info;
	  out `El_end
    | A.Nil ->
	let xml_attrs = [id_a id; kind_a "nil"] in
	  out (`El_start (tag_attr "node" xml_attrs));
	  out `El_end

let out_unary_op out op id child_id =
  let e () = out_edge out child_id in
  let n kind info_fun =
    let xml_attrs = [id_a id; kind_a kind] in
      out (`El_start (tag_attr "node" xml_attrs));
      info_fun ();
      e ();
      out `El_end
  in
    match op with
      | A.RowNum rownum_info ->
	  n "rownum" (fun () -> out_rownum_info out rownum_info)
      | A.RowID rowid_info ->
	  n "rowid" (fun () -> out_rowid_info out rowid_info)
      | A.RowRank rank_info ->
	  n "rowrank" (fun () -> out_rank_info out rank_info)
      | A.Rank rank_info ->
	  n "rank" (fun () -> out_rank_info out rank_info)
      | A.Project project_info ->
	  n "project" (fun () -> out_project_info out project_info)
      | A.Select select_info ->
	  n "select" (fun () -> out_select_info out select_info)
      | A.PosSelect pos_select_info ->
	  n "pos_select" (fun () -> out_pos_select_info out pos_select_info)
      | A.Distinct ->
	  n "distinct" (fun () -> ())
      | A.Attach attach_info ->
	  n "attach" (fun () -> out_attach_info out attach_info)
      | A.Cast cast_info ->
	  n "cast" (fun () -> out_cast_info out cast_info)
      | A.FunNumEq binop_info ->
	  n "eq" (fun () -> out_binop_info out binop_info)
      | A.FunNumGt binop_info ->
	  n "gt" (fun () -> out_binop_info out binop_info)
      | A.Fun1to1 fun_1to1_info ->
	  n "fun" (fun () -> out_fun_1to1_info out fun_1to1_info)
      | A.FunBoolAnd binop_info ->
	  n "and" (fun () -> out_binop_info out binop_info)
      | A.FunBoolOr binop_info ->
	  n "or" (fun () -> out_binop_info out binop_info)
      | A.FunBoolNot unop_info ->
	  n "not" (fun () -> out_unop_info out unop_info)
      | A.FunAggr ((f, _op, _part) as fun_aggr_info) ->
	  let name = string_of_aggr f in
	    n name (fun () -> out_fun_aggr_info out fun_aggr_info)
      | A.FunAggrCount fun_aggr_count_info ->
	  n "count" (fun () -> out_fun_aggr_count_info out fun_aggr_count_info)

let out_binary_op out op id left_child_id right_child_id =
  let e () =
    out_edge out left_child_id;
    out_edge out right_child_id
  in
  let n kind info_fun =
    let xml_attrs = [id_a id; kind_a kind] in
      out (`El_start (tag_attr "node" xml_attrs));
      info_fun ();
      e ();
      out `El_end
  in
    match op with
      | A.EqJoin  eqjoin_info ->
	  n "eqjoin" (fun () -> out_eqjoin_info out eqjoin_info)
      | A.SemiJoin  eqjoin_info ->
	  n "semijoin" (fun () -> out_eqjoin_info out eqjoin_info)
      | A.ThetaJoin  thetajoin_info ->
	  n "thetajoin" (fun () -> out_thetajoin_info out thetajoin_info)
      | A.DisjunctUnion ->
	  n "union" (fun () -> ())
      | A.Difference ->
	  n "difference" (fun () -> ())
      | A.SerializeRel  serialize_rel_info ->
	  n "serialize relation" (fun () -> out_serialize_rel_info out serialize_rel_info)
      | A.Cross ->
	  n "cross" (fun () -> ())

let rec out_dag (out, node, visited) =
  match !node with
    | ADag.NullaryNode (op, id) ->
	let visited = IntSet.add id visited in
	  out_nullary_op out op id;
	  visited
    | ADag.UnaryNode (op, id, child) ->
	let child_id = ADag.id_of_node !child in
	let visited = IntSet.add id visited in
	let visited = 
	  if not (IntSet.mem child_id visited) then
	    out_dag (out, child, visited)
	  else
	    visited
	in
	  out_unary_op out op id child_id;
	  visited
    | ADag.BinaryNode (op, id, lchild, rchild) ->
	let lchild_id = ADag.id_of_node !lchild in
	let rchild_id = ADag.id_of_node !rchild in
	let visited = IntSet.add id visited in
	let visited =
	  if not (IntSet.mem lchild_id visited) then
	    out_dag (out, lchild, visited)
	  else
	    visited
	in
	let visited =
	  if not (IntSet.mem rchild_id visited) then
	    out_dag (out, rchild, visited)
	  else 
	    visited
	in
	  out_binary_op out op id lchild_id rchild_id;
	  visited

let export_plan buf dag =
  let o = Xmlm.make_output ~nl:true ~indent:(Some 2) (`Buffer buf) in
  let out = Xmlm.output o in
  let dag_pruned = ref (ADag.prune_empty !dag) in
    out (`Dtd None);
    out (`El_start (tag "query_plan_bundle"));
    out (`El_start (tag_attr "query_plan" [("id", "0")]));
    out (`El_start (tag_attr "logical_query_plan" [("unique_names", "true")]));
    ignore (out_dag (out, dag_pruned, IntSet.empty));
    out `El_end;
    out `El_end;
    out `El_end
