module A = Algebra

type node_id = int

type dag =
  | BinaryNode of A.binary_op * node_id * dag ref * dag ref
  | UnaryNode of A.unary_op * node_id * dag ref
  | NullaryNode of A.nullary_op * node_id

type t = dag ref

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

let mk_eqjoin info = mkbinnode (A.EqJoin info)
let mk_semijoin info = mkbinnode (A.SemiJoin info)
let mk_thetajoin info = mkbinnode (A.ThetaJoin info)
let mk_disjunion = mkbinnode A.DisjunctUnion
let mk_difference = mkbinnode A.Difference
let mk_serializerel info = mkbinnode (A.SerializeRel info)
let mk_cross = mkbinnode (A.Cross)

let mk_rownum info = mkunnode (A.RowNum info)
let mk_rowid info = mkunnode (A.RowID info)
let mk_rowrank info = mkunnode (A.RowRank info)
let mk_rank info = mkunnode (A.Rank info)
let mk_project info = mkunnode (A.Project info)
let mk_select info = mkunnode (A.Select info)
let mk_posselect info = mkunnode (A.PosSelect info)
let mk_distinct = mkunnode (A.Distinct)
let mk_attach info = mkunnode (A.Attach info)
let mk_cast info = mkunnode (A.Cast info)
let mk_funnumeq info = mkunnode (A.FunNumEq info)
let mk_funnumgt info = mkunnode (A.FunNumGt info)
let mk_fun1to1 info = mkunnode (A.Fun1to1 info)
let mk_funbooland info = mkunnode (A.FunBoolAnd info)
let mk_funboolor info = mkunnode (A.FunBoolOr info)
let mk_funboolnot info = mkunnode (A.FunBoolNot info)
let mk_funaggr info = mkunnode (A.FunAggr info)
let mk_funaggrcount info = mkunnode (A.FunAggrCount info)

let mk_littbl info = mknullnode (A.LitTbl info)
let mk_emptytbl = mknullnode (A.EmptyTbl [(A.Iter 0, `NatType); (A.Pos 0, `NatType); (A.Item 1, `IntType)])
let mk_tblref info = mknullnode (A.TblRef info)
let mk_nil = mknullnode A.Nil

let id_of_node = function
  | NullaryNode (_, id) -> id
  | UnaryNode (_, id, _) -> id
  | BinaryNode (_, id, _, _) -> id
let is_emptytbl dag =
  match dag with
    | NullaryNode ((A.EmptyTbl _), _id) -> true
    | _ -> false

let emptytbl_schema dag =
  match dag with
    | NullaryNode ((A.EmptyTbl schema), id) -> 
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
      | A.EqJoin _
      | A.SemiJoin _
      | A.ThetaJoin _
      | A.Cross ->
	(match (emptytbl_schema !ch1), (emptytbl_schema !ch2) with
	  | (Some (schema1, id1)), (Some (_schema2, _id2)) ->
	    NullaryNode ((A.EmptyTbl schema1, id1))
	  | (Some (schema1, id1)), None ->
	    NullaryNode ((A.EmptyTbl schema1, id1))
	  | None, (Some (schema2, id2)) ->
	    NullaryNode ((A.EmptyTbl schema2, id2))
	  | None, None ->
	    n)
      | A.DisjunctUnion ->
	(match (emptytbl_schema !ch1), (emptytbl_schema !ch2) with
	  | (Some (schema1, id1)), (Some (_schema2, _id2)) ->
	    NullaryNode ((A.EmptyTbl schema1, id1))
	  | (Some (_schema1, _id1)), None ->
	    !ch2
	  | None, (Some (_schema2, _id2)) ->
	    !ch1
	  | None, None ->
	    n)
      | A.Difference ->
	(match (emptytbl_schema !ch1), (emptytbl_schema !ch2) with
	  | (Some (schema1, id1)), (Some (_schema2, _id2)) ->
	    NullaryNode ((A.EmptyTbl schema1, id1))
	  | (Some (schema1, id1)), None ->
	    NullaryNode ((A.EmptyTbl schema1, id1))
	  | None, (Some (_schema2, _id2)) ->
	    !ch1
	  | None, None ->
	    n)

      | A.SerializeRel _ -> n)
  | UnaryNode (_unop, _id, ch) as n ->
    if not (is_emptytbl !ch) then
      ch := prune_empty !ch;
    (match (emptytbl_schema !ch) with
      | Some (schema, id) ->
	NullaryNode ((A.EmptyTbl schema), id)
      | None ->
	n)
  | NullaryNode (_nullop, _id) as n ->
    n
