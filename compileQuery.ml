open Utility

let dummy = ()

module Cs = struct

  type offset = int
  type cs = csentry list
  and csentry =
    | Offset of offset
    | Mapping of string * cs

  let rec leafs cs =
    List.rev
      (List.fold_left
	(fun leaf_list cs_entry ->
	   match cs_entry with
	     | Offset o -> o :: leaf_list
	     | Mapping (_, cs) -> (List.rev (leafs cs)) @ leaf_list)
	[]
	cs)

  let cardinality = List.length

  let rec shift cs i =
    List.map
      (function
	 | Offset o -> Offset (o + i)
	 | Mapping (key, cs) -> Mapping (key, (shift cs i)))
      cs

  let (||) cs1 cs2 =
    cs1 @ (shift cs2 (cardinality cs1))
    
end
  
let value (_env, _aenv) loop e =
  match e with
    | `Constant c ->
	let cs = [Cs.Offset 1] in
	let q =
	  (Algebra.Dag.mk_attach
	     (Algebra.Item 1, c)
	     (ref (Algebra.Dag.mk_attach
		     (Algebra.Pos 1, `Int (Num.Int 1))
		     loop)))
	in
	  (q, cs, dummy, dummy)
    | _ ->
	failwith "CompileQuery.value: not implemented"

