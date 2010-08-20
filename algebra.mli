(** types supported by pathfinder *)
type pf_type = [ `IntType | `StrType | `BoolType | `CharType | `FloatType | `NatType ]

val show_pf_type : pf_type Show.show

(** Constructors for constant values in algebra expressions *)
type pf_constant = 
  | Float  of float
  | Int    of Num.num
  | String of string
  | Bool   of bool
  | Char   of char 
  | Nat of nativeint 

val pf_constant_of_constant : Constant.constant -> pf_constant

(** aggregate functions *)
type aggr = Avg | Max | Min | Sum | All

(** 1to1 functions *)
type func = Add | Subtract | Multiply | Divide | Modulo | Contains | SimilarTo | Concat

(** relation operators *)
type join_comparison = Eq | Gt | Ge | Lt | Le | Ne

(** categories of relational attributes *)
type attr_name =
  | Iter of int
  | Pos of int
  | Item of int

(* specific attribute types *)
type result_attr_name = attr_name
type partitioning_attr_name = attr_name
type selection_attr_name = attr_name
type sort_attr_name = attr_name
type new_attr_name = attr_name
type old_attr_name = attr_name
type left_attr_name = attr_name
type right_attr_name = attr_name

type sort_direction = Ascending | Descending
type tbl_name = string
type sort_infos = (sort_attr_name * sort_direction) list
type schema_infos = (attr_name * pf_type) list
type key_infos = attr_name list list
type tbl_attribute_infos = (attr_name * string * pf_type) list

(* semantic information on operator nodes *)
type rownum_info = result_attr_name * sort_infos * partitioning_attr_name option
type rowid_info = result_attr_name
type rank_info = result_attr_name * sort_infos
type project_info = (new_attr_name * old_attr_name) list
type select_info = selection_attr_name
type pos_select_info = int * sort_infos * partitioning_attr_name option
type eqjoin_info = left_attr_name * right_attr_name
type thetajoin_info = (join_comparison * (left_attr_name * right_attr_name)) list
type lit_tbl_info = pf_constant list list * schema_infos
type attach_info = result_attr_name * pf_constant
type cast_info = result_attr_name * attr_name * pf_type
type binop_info = result_attr_name * (left_attr_name * right_attr_name)
type unop_info = result_attr_name * attr_name
type fun_1to1_info = func * result_attr_name * (attr_name list)
type fun_aggr_info = aggr * unop_info * partitioning_attr_name option
type fun_aggr_count_info = result_attr_name * partitioning_attr_name option
type serialize_rel_info = attr_name * attr_name * (attr_name list)
type tbl_ref_info = tbl_name * tbl_attribute_infos * key_infos
type empty_tbl_info = schema_infos
type error_info = attr_name

(** algebra operators with two children *)
type binary_op =
  | EqJoin of eqjoin_info 
  | SemiJoin of eqjoin_info 
  | ThetaJoin of thetajoin_info 
  | DisjunctUnion
  | Difference 
  | SerializeRel of serialize_rel_info 
  | Cross 

(** algebra operators with one child *)
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

(** leaf algebra operators *)
type nullary_op =
  | LitTbl of lit_tbl_info
  | EmptyTbl of schema_infos
  | TblRef of tbl_ref_info
  | Nil
