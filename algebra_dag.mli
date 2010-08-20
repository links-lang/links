type node_id = int

(** type of DAG nodes *)
(* FIXME: remove this type from the interface *)
type dag =
  | BinaryNode of Algebra.binary_op * node_id * dag ref * dag ref
  | UnaryNode of Algebra.unary_op * node_id * dag ref
  | NullaryNode of Algebra.nullary_op * node_id

(** id of a DAG node *)
val id_of_node : dag -> node_id

(** type of a DAG *)
(* FIXME: make this type opaque *)
type t = dag ref

(** constructor functions for algebra DAG nodes with two children *)
val mk_eqjoin : Algebra.eqjoin_info -> t -> t -> t
val mk_semijoin : Algebra.eqjoin_info -> t -> t -> t
val mk_thetajoin : Algebra.thetajoin_info -> t -> t -> t
val mk_serializerel : Algebra.serialize_rel_info -> t -> t -> t
val mk_disjunion : t -> t -> t
val mk_difference : t -> t -> t
val mk_cross : t -> t -> t

(** constructor functions for algebra DAG nodes with one child *)
val mk_rownum : Algebra.rownum_info -> t -> t
val mk_rowid : Algebra.rowid_info -> t -> t
val mk_rowrank : Algebra.rank_info -> t -> t
val mk_rank : Algebra.rank_info -> t -> t
val mk_project : Algebra.project_info -> t -> t
val mk_select : Algebra.select_info -> t -> t
val mk_posselect : Algebra.pos_select_info -> t -> t
val mk_attach : Algebra.attach_info -> t -> t
val mk_cast : Algebra.cast_info -> t -> t
val mk_funnumeq : Algebra.binop_info -> t -> t
val mk_funnumgt : Algebra.binop_info -> t -> t
val mk_fun1to1 : Algebra.fun_1to1_info -> t -> t
val mk_funbooland : Algebra.binop_info -> t -> t
val mk_funboolor : Algebra.binop_info -> t -> t
val mk_funboolnot : Algebra.unop_info -> t -> t
val mk_funaggr : Algebra.fun_aggr_info -> t -> t
val mk_funaggrcount : Algebra.fun_aggr_count_info -> t -> t
val mk_distinct : t -> t

(** constructor functions for algebra DAG leafs *)
val mk_littbl : Algebra.lit_tbl_info -> t
val mk_tblref : Algebra.tbl_ref_info -> t
val mk_emptytbl : t
val mk_nil : t

(** remove all emptytbl nodes from the DAG *)
val prune_empty : dag -> dag
