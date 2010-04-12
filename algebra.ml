open Utility

(*FIXME: should char constants be allowed *)
type base_type = [ `IntType | `StrType | `BoolType | `CharType | `FloatType | `NatType]
type column_type = [ base_type | `Surrogate ]

let column_type_of_constant = function
  | `Bool _ -> `BoolType
  | `Int _ -> `IntType
  | `String _ -> `StrType
  | `Float _ -> `FloatType
  | `Char _ -> `CharType

type constant = 
  | Float  of float
  | Int    of Num.num
  | String of string
  | Bool   of bool
  | Char   of char 
  | Nat of nativeint 

let escape_string s = (* SQL standard for escaping single quotes in a string *)
  Str.global_replace (Str.regexp "'") "''" s

let string_of_constant = function
  | Bool value -> string_of_bool value
  | Int value -> Num.string_of_num value
  | Char c -> "'"^ Char.escaped c ^"'" 
  | String s -> escape_string s
  | Float value   -> string_of_float value
  | Nat n -> Nativeint.to_string n

let const = function
  | `Bool b -> Bool b
  | `Int i -> Int i
  | `String s -> String s
  | `Float f -> Float f
  | `Char c -> Char c

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
type lit_tbl_info = constant list list * schema_infos
type attach_info = result_attr_name * constant
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
  | Eq -> "eq"
  | Gt -> "gt"
  | Ge -> "ge"
  | Lt -> "lt"
  | Le -> "le"
  | Ne -> "ne"

let string_of_column_type = function
  | `IntType -> "int"
  | `StrType -> "str"
  | `BoolType -> "bln"
  | `CharType -> "str"
  | `FloatType -> "dbl"
  | `NatType -> "nat"
  | `Surrogate -> "surr"

let typestring_of_constant = function
  | Float _ -> "dbl"
  | Int _ -> "int"
  | String _ -> "str"
  | Bool _ -> "bool"
  | Char _ -> "char"
  | Nat _ -> "nat"
    
let string_of_func = function
  | Add -> "add"
  | Subtract -> "subtract"
  | Multiply -> "multiply"
  | Divide -> "divide"
  | Modulo -> "modulo"
  | Contains -> "fn:contains"

let string_of_aggr = function
  | Max -> "max"
  | Min -> "min"
  | Avg -> "avg"
  | Sum -> "sum"

module Dag = struct
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
    ref (BinaryNode (op, (id ()), left_child, right_child))

  let mkunnode op child =
    ref (UnaryNode (op, (id ()), child))

  let mknullnode op =
    ref (NullaryNode (op, (id ())))

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
		     !ch2
		 | None, (Some (_schema2, _id2)) ->
		     !ch1
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
	
end
      
