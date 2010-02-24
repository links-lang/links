open Utility

(*FIXME: should char constants be allowed *)
type base_type = IntType | StrType | BoolType | CharType | FloatType

(* aggregate functions *)
type aggr = Avg | Max | Min | Sum

type func = Add | Subtract | Multiply | Divide | Modulo | Contains

(* relation operators *)
type join_comparison = Eq | Gt | Ge | Lt | Le | Ne

type tbl_name = string

type attr_name =
  | Iter of int
  | Pos of int
  | Item of int

(* attribute names *)
type result_attr_name = attr_name
type partitioning_attr_name = attr_name
type selection_attr_name = attr_name
type sort_attr_name = attr_name
type new_attr_name = attr_name
type old_attr_name = attr_name
type left_attr_name = attr_name
type right_attr_name = attr_name

type sort_direction = Ascending | Descending
type sort_infos = (sort_attr_name * sort_direction) list
type schema_infos = (attr_name * base_type) list
type key_infos = attr_name list list
type tbl_attribute_infos = (attr_name * string * base_type) list

(* semantic informations on operator nodes *)
type rownum_info = result_attr_name * sort_infos * partitioning_attr_name option
type rowid_info = result_attr_name
type rank_info = result_attr_name * sort_infos
type project_info = (new_attr_name * old_attr_name) list
type select_info = selection_attr_name
type pos_select_info = int * sort_infos * partitioning_attr_name option
type eqjoin_info = left_attr_name * right_attr_name
type thetajoin_info = (join_comparison * (left_attr_name * right_attr_name)) list
type lit_tbl_info = Constant.constant list list * schema_infos
type attach_info = result_attr_name * Constant.constant
type cast_info = result_attr_name * attr_name * base_type
type binop_info = result_attr_name * (left_attr_name * right_attr_name)
type unop_info = result_attr_name * attr_name
type fun_1to1_info = func * result_attr_name * (attr_name list)
type fun_aggr_info = aggr * unop_info * partitioning_attr_name option
type fun_aggr_count_info = result_attr_name * partitioning_attr_name option
type serialize_rel_info = attr_name * attr_name * (attr_name list)
type tbl_ref_info = tbl_name * tbl_attribute_infos * key_infos
type empty_tbl_info = schema_infos

type binary_op =
  | EqJoin of eqjoin_info 
  | SemiJoin of eqjoin_info 
  | ThetaJoin of thetajoin_info 
  | DisjunctUnion
  | Difference 
  | SerializeRel of serialize_rel_info 
  | Cross 

type unary_op =
  | RowNum of rownum_info 
  | RowID of rowid_info 
  | RowRank of rank_info 
  | Rank of rank_info 
  | Project of project_info 
  | Select of select_info 
  | PosSelect of pos_select_info 
  | Distinct
  | Attach of attach_info 
  | Cast of cast_info 
  | FunNumEq of binop_info 
  | FunNumGt of binop_info 
  | Fun1to1 of fun_1to1_info 
  | FunBoolAnd of binop_info 
  | FunBoolOr of binop_info 
  | FunBoolNot of unop_info 
  | FunAggr of fun_aggr_info 
  | FunAggrCount of fun_aggr_count_info 

type nullary_op =
  | LitTbl of lit_tbl_info
  | EmptyTbl of schema_infos
  | TblRef of tbl_ref_info
  | Nil

type node =
  | BinaryNode of binary_op * node * node
  | UnaryNode of unary_op * node
  | NullaryNode of nullary_op

let string_of_attr_name = function
  | Iter 0 -> "iter"
  | Iter i -> "iter" ^ (string_of_int i)
  | Pos 0 -> "pos"
  | Pos i -> "pos" ^ (string_of_int i)
  | Item i -> "item" ^ (string_of_int i)

let cid i = Iter i

let string_of_sort_direction = function
  | Ascending -> "ascending"
  | Descending -> "descending"

let string_of_join_comparison = function
  | Eq -> "="
  | Gt -> ">"
  | Ge -> ">="
  | Lt -> "<"
  | Le -> "<="
  | Ne -> "<>"

let string_of_base_type = function
  | IntType -> "int"
  | StrType -> "str"
  | BoolType -> "bln"
  | CharType -> "str"
  | FloatType -> "dbl"

let typestring_of_constant = function
  | `Float _ -> "dbl"
  | `Int _ -> "int"
  | `String _ -> "string"
  | `Bool _ -> "bool"
  | `Char _ -> "char"
    
let string_of_func = function
  | Add -> "add"
  | Subtract -> "subtract"
  | Multiply -> "multiply"
  | Divide -> "divide"
  | Modulo -> "modulo"
  | Contains -> "fn:contains"

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

let out_empty_tbl_info out schema =
  out (`El_start (tag "content"));
  List.iter
    (fun (name, typ) ->
       out_col out [("name", (string_of_attr_name name)); ("type", string_of_base_type typ); ("new", "true")])
    schema;
  out `El_end

let out_binop_info out (result_attr, arg_pair) =
  out (`El_start (tag "content"));
  out_col out [("name", result_attr); ("new", "true")];
  out_arg_pair out arg_pair;
  out `El_end

let out_sort_infos out l =
  ignore 
    (List.fold_left
       (fun i (sort_attr_name, dir) ->
	  let xml_attributes = [
	    ("name", (string_of_attr_name sort_attr_name));
	    ("direction", string_of_sort_direction dir);
	    ("position", string_of_int i);
	    ("new", "false")]
	  in
	    out_col out xml_attributes;
	    (i + 1))
       0
       l)

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

let out_rank_info out ((res_attr, sort_infos) : rank_info) =
  out (`El_start (tag "content"));
  out_col out [("name", (string_of_attr_name res_attr)); ("new", "true")];
  out_sort_infos out sort_infos;
  out `El_end

let out_project_info out (l : project_info) =
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

let out_pos_select_info out ((pos, sort_infos, maybe_part_attr) : pos_select_info) =
  out (`El_start (tag "content"));
  out_pos out pos;
  out_sort_infos out sort_infos;
  out_maybe_part_name out maybe_part_attr;
  out `El_end

let out_eqjoin_info out (arg_pair : eqjoin_info) =
  out (`El_start (tag "content"));
  out_arg_pair out arg_pair;
  out `El_end

let out_thetajoin_info out (l : thetajoin_info) =
  out (`El_start (tag "content"));
  List.iter
    (fun (comp, attributes) ->
       out (`El_start (tag_attr "comparison" [("kind", string_of_join_comparison comp)]));
       out_arg_pair out attributes;
       out `El_end)
    l;
  out `El_end

let out_lit_tbl_info out ((values_per_col, schema_infos) : lit_tbl_info) =
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
		   (fun () -> out (`Data (typestring_of_constant value))))
	      values
	  in
	    out_col_childs out [("name", (string_of_attr_name (fst info)))] c)
       values_per_col
       schema_infos
   with Invalid_argument _ -> 
     failwith "out_lit_tbl_info: list lengths do not match");
  out `El_end

let out_attach_info out (result_attr, value) =
  out (`El_start (tag "content"));
  let xml_attrs = [("name", (string_of_attr_name result_attr)); ("new", "true")] in
  let f () = out (`Data (Constant.string_of_constant value)) in
    out_col_childs out xml_attrs f;
    out `El_end
      

let out_cast_info out (result_attr, name, base_type) =
  out (`El_start (tag "content"));
  out_col out [("name", (string_of_attr_name result_attr)); ("new", "true")];
  out_col out [("name", (string_of_attr_name name)); ("new", "false")];
  out_col out [("name", string_of_base_type base_type)];
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
  ignore (
    List.fold_left
      (fun i arg_attr ->
	 out_col out [("name", (string_of_attr_name arg_attr)); 
		      ("new", "false"); 
		      ("position", string_of_int i)];
	 (i + 1))
      0
      arg_list);
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
  out_col out [("name", (string_of_attr_name result_attr))];
  out_maybe_part_name out maybe_part_attr;
  out `El_end

let out_serialize_rel_info out (iter, pos, items) =
  out (`El_start (tag "content"));
  out_col out [("name", (string_of_attr_name iter)); ("new", "false"); ("function", "iter")];
  out_col out [("name", (string_of_attr_name pos)); ("new", "false"); ("function", "pos")];
  ignore (
    List.fold_left
      (fun i item ->
	 out_col out [("name", (string_of_attr_name item)); 
		      ("new", "false"); 
		      ("function", "item"); 
		      ("position", string_of_int i)];
	 (i + 1))
      0
      items);
  out `El_end

let out_tbl_ref_info out (tbl_name, attr_infos, key_infos) =
  out (`El_start (tag "properties"));
  out (`El_start (tag "keys"));
  List.iter
    (fun key ->
       let c () =
	 List.fold_left
	   (fun i attr -> 
	      out_col out [("name", (string_of_attr_name attr)); ("position", string_of_int i)];
	      (i + 1))
	   0
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
		      ("type", string_of_base_type typ)])
      attr_infos
  in
    out_el_childs out "table" [("name", tbl_name)] c;
    out `El_end

let out_nullary_op out op id =
  match op with
    | LitTbl lit_tbl_info ->
	let xml_attrs = [id_a id; kind_a "node"] in
	  out (`El_start (tag_attr "node" xml_attrs));
	  out_lit_tbl_info out lit_tbl_info;
	  out `El_end
    | EmptyTbl info ->
	let xml_attrs = [id_a id; kind_a "empty_tbl"] in
	  out (`El_start (tag_attr "node" xml_attrs));
	  out_empty_tbl_info out info;
	  out `El_end
    | TblRef info ->
	let xml_attrs = [id_a id; kind_a "ref_tbl"] in
	  out (`El_start (tag_attr "node" xml_attrs));
	  out_tbl_ref_info out info;
	  out `El_end
    | Nil ->
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
      | RowNum rownum_info ->
	  n "row_num" (fun () -> out_rownum_info out rownum_info)
      | RowID rowid_info ->
	  n "row_id" (fun () -> out_rowid_info out rowid_info)
      | RowRank rank_info ->
	  n "rowrank" (fun () -> out_rank_info out rank_info)
      | Rank rank_info ->
	  n "rank" (fun () -> out_rank_info out rank_info)
      | Project project_info ->
	  n "project" (fun () -> out_project_info out project_info)
      | Select select_info ->
	  n "select" (fun () -> out_select_info out select_info)
      | PosSelect pos_select_info ->
	  n "pos_select" (fun () -> out_pos_select_info out pos_select_info)
      | Distinct ->
	  n "distinct" (fun () -> ())
      | Attach attach_info ->
	  n "attach" (fun () -> out_attach_info out attach_info)
      | Cast cast_info ->
	  n "cast" (fun () -> out_cast_info out cast_info)
      | FunNumEq binop_info ->
	  n "eq" (fun () -> out_binop_info out binop_info)
      | FunNumGt binop_info ->
	  n "gt" (fun () -> out_binop_info out binop_info)
      | Fun1to1 fun_1to1_info ->
	  n "fun" (fun () -> out_fun_1to1_info out fun_1to1_info)
      | FunBoolAnd binop_info ->
	  n "and" (fun () -> out_binop_info out binop_info)
      | FunBoolOr binop_info ->
	  n "or" (fun () -> out_binop_info out binop_info)
      | FunBoolNot unop_info ->
	  n "not" (fun () -> out_unop_info out unop_info)
      | FunAggr fun_aggr_info ->
	  n "aggr" (fun () -> out_fun_aggr_info out fun_aggr_info)
      | FunAggrCount fun_aggr_count_info ->
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
      | EqJoin  eqjoin_info ->
	  n "eqjoin" (fun () -> out_eqjoin_info out eqjoin_info)
      | SemiJoin  eqjoin_info ->
	  n "semijoin" (fun () -> out_eqjoin_info out eqjoin_info)
      | ThetaJoin  thetajoin_info ->
	  n "thetajoin" (fun () -> out_thetajoin_info out thetajoin_info)
      | DisjunctUnion ->
	  n "union" (fun () -> ())
      | Difference ->
	  n "difference" (fun () -> ())
      | SerializeRel  serialize_rel_info ->
	  n "serialize relation" (fun () -> out_serialize_rel_info out serialize_rel_info)
      | Cross ->
	  n "cross" (fun () -> ())

module Dag :
sig
  type dag

  (* binary node constructors *)
  val mk_eqjoin : eqjoin_info -> dag ref -> dag ref -> dag
  val mk_semijoin : eqjoin_info -> dag ref -> dag ref -> dag
  val mk_thetajoin : thetajoin_info -> dag ref -> dag ref -> dag
  val mk_disjunion : dag ref -> dag ref -> dag
  val mk_difference : dag ref -> dag ref -> dag
  val mk_serializerel : serialize_rel_info -> dag ref -> dag ref -> dag
  val mk_cross : dag ref -> dag ref -> dag

  (* unary node constructors *)
  val mk_rownum : rownum_info -> dag ref -> dag
  val mk_rowid : rowid_info -> dag ref -> dag
  val mk_rowrank : rank_info -> dag ref -> dag
  val mk_rank : rank_info -> dag ref -> dag
  val mk_project : project_info -> dag ref -> dag
  val mk_select : select_info -> dag ref -> dag
  val mk_posselect : pos_select_info -> dag ref -> dag
  val mk_distinct : dag ref -> dag
  val mk_attach : attach_info -> dag ref -> dag
  val mk_cast : cast_info -> dag ref -> dag
  val mk_funnumeq : binop_info -> dag ref -> dag
  val mk_funnumgt : binop_info -> dag ref -> dag
  val mk_fun1to1 : fun_1to1_info -> dag ref -> dag
  val mk_funbooland : binop_info -> dag ref -> dag
  val mk_funboolor : binop_info -> dag ref -> dag
  val mk_funboolnot : unop_info -> dag ref -> dag
  val mk_funaggr : fun_aggr_info -> dag ref -> dag
  val mk_funaggrcount : fun_aggr_count_info -> dag ref -> dag

  (* nullary node constructors *)
  val mk_littbl : lit_tbl_info -> dag
  val mk_emptytbl : schema_infos -> dag
  val mk_tblref : tbl_ref_info -> dag
  val mk_nil : dag

  val prune_empty : dag -> dag
  val export_plan : string -> dag ref -> unit
end =
struct
  type node_id = int

  type dag =
    | BinaryNode of binary_op * node_id * dag ref * dag ref
    | UnaryNode of unary_op * node_id * dag ref
    | NullaryNode of nullary_op * node_id

  let next_id = ref 0

  let id () = 
    let s = !next_id in 
      incr next_id; 
      s

  let mkbinnode op left_child right_child =
    BinaryNode (op, (id ()), left_child, right_child)

  let mkunnode op child =
    UnaryNode (op, (id ()), child)

  let mknullnode op =
    NullaryNode (op, (id ()))

  let mk_eqjoin info = mkbinnode (EqJoin info)
  let mk_semijoin info = mkbinnode (SemiJoin info)
  let mk_thetajoin info = mkbinnode (ThetaJoin info)
  let mk_disjunion = mkbinnode DisjunctUnion
  let mk_difference = mkbinnode Difference
  let mk_serializerel info = mkbinnode (SerializeRel info)
  let mk_cross = mkbinnode (Cross)

  let mk_rownum info = mkunnode (RowNum info)
  let mk_rowid info = mkunnode (RowID info)
  let mk_rowrank info = mkunnode (RowRank info)
  let mk_rank info = mkunnode (Rank info)
  let mk_project info = mkunnode (Project info)
  let mk_select info = mkunnode (Select info)
  let mk_posselect info = mkunnode (PosSelect info)
  let mk_distinct = mkunnode (Distinct)
  let mk_attach info = mkunnode (Attach info)
  let mk_cast info = mkunnode (Cast info)
  let mk_funnumeq info = mkunnode (FunNumEq info)
  let mk_funnumgt info = mkunnode (FunNumGt info)
  let mk_fun1to1 info = mkunnode (Fun1to1 info)
  let mk_funbooland info = mkunnode (FunBoolAnd info)
  let mk_funboolor info = mkunnode (FunBoolOr info)
  let mk_funboolnot info = mkunnode (FunBoolNot info)
  let mk_funaggr info = mkunnode (FunAggr info)
  let mk_funaggrcount info = mkunnode (FunAggrCount info)

  let mk_littbl info = mknullnode (LitTbl info)
  let mk_emptytbl info = mknullnode (EmptyTbl info)
  let mk_tblref info = mknullnode (TblRef info)
  let mk_nil = mknullnode Nil

  let id_of_node = function
    | NullaryNode (_, id) -> id
    | UnaryNode (_, id, _) -> id
    | BinaryNode (_, id, _, _) -> id

  let rec out_dag (out, node, visited) =
    match !node with
      | NullaryNode (op, id) ->
	  let visited = IntSet.add id visited in
	    out_nullary_op out op id;
	    visited
      | UnaryNode (op, id, child) ->
	  let child_id = id_of_node !child in
	  let visited = IntSet.add id visited in
	  let visited = 
	    if not (IntSet.mem child_id visited) then
	      out_dag (out, child, visited)
	    else
	      visited
	  in
	    out_unary_op out op id child_id;
	    visited
      | BinaryNode (op, id, lchild, rchild) ->
	  let lchild_id = id_of_node !lchild in
	  let rchild_id = id_of_node !rchild in
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

  let is_emptytbl dag =
    match dag with
      | NullaryNode ((EmptyTbl _), _id) -> true
      | _ -> false

  let emptytbl_schema dag =
    match dag with
      | NullaryNode ((EmptyTbl schema), id) -> 
	  Some (schema, id)
      | _ ->
	  None
	  
  let rec prune_empty = function
    | BinaryNode (binop, _id, ch1, ch2) as n ->
	if not (is_emptytbl !ch1) then
	  ch1 := prune_empty !ch1;
	if not (is_emptytbl !ch2) then
	  ch2 := prune_empty !ch2;
	(match binop with
	  | EqJoin _
	  | SemiJoin _
	  | ThetaJoin _
	  | Cross ->
	      (match (emptytbl_schema !ch1), (emptytbl_schema !ch2) with
		| (Some (schema1, id1)), (Some (_schema2, _id2)) ->
		    NullaryNode ((EmptyTbl schema1, id1))
		| (Some (schema1, id1)), None ->
		    NullaryNode ((EmptyTbl schema1, id1))
		| None, (Some (schema2, id2)) ->
		    NullaryNode ((EmptyTbl schema2, id2))
		| None, None ->
		    n)
	  | DisjunctUnion ->
	      (match (emptytbl_schema !ch1), (emptytbl_schema !ch2) with
		 | (Some (schema1, id1)), (Some (_schema2, _id2)) ->
		     NullaryNode ((EmptyTbl schema1, id1))
		 | (Some (_schema1, _id1)), None ->
		     !ch1
		 | None, (Some (_schema2, _id2)) ->
		     !ch2
		 | None, None ->
		     n)
	  | Difference ->
	      (match (emptytbl_schema !ch1), (emptytbl_schema !ch2) with
		 | (Some (schema1, id1)), (Some (_schema2, _id2)) ->
		     NullaryNode ((EmptyTbl schema1, id1))
		 | (Some (schema1, id1)), None ->
		     NullaryNode ((EmptyTbl schema1, id1))
		 | None, (Some (_schema2, _id2)) ->
		     !ch1
		 | None, None ->
		     n)

	  | SerializeRel _ -> n)
    | UnaryNode (_unop, _id, ch) as n ->
	if not (is_emptytbl !ch) then
	  ch := prune_empty !ch;
	(match (emptytbl_schema !ch) with
	   | Some (schema, id) ->
	       NullaryNode ((EmptyTbl schema), id)
	   | None ->
	       n)
    | NullaryNode (_nullop, _id) as n ->
	n
	
	      
  let export_plan fname dag =
    let oc = open_out fname in
    let o = Xmlm.make_output ~nl:true ~indent:(Some 2) (`Channel oc) in
    let out = Xmlm.output o in
    let wrap arg =
      out (`Dtd None);
      out (`El_start (tag_attr "logical_query_plan" [("unique_names", "true")]));
      ignore (out_dag arg);
      out `El_end;
    in
      apply wrap (out, dag, IntSet.empty) ~finally:close_out oc
end

let test () =

    let et = ref (Dag.mk_emptytbl [((Iter 0), IntType); ((Pos 0), IntType); ((Item 0), IntType)]) in
    let r = ref (Dag.mk_tblref ("t1", [((Item 0), "foo", IntType)], [[Item 0]])) in
      let t =
	ref (Dag.mk_serializerel
	       (Iter 0, Pos 0, [Item 0])
	       (ref (Dag.mk_disjunion r et))
	       (ref (Dag.mk_distinct et)))
      in
      (*let t = ref (Dag.prune_empty !t) in *)
	Dag.export_plan "plan.xml" t;
      
