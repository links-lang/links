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

  let append cs1 cs2 =
    cs1 @ (shift cs2 (cardinality cs1))

  let items_of_offsets = List.map (fun i -> Algebra.Item i)
    
end

let incr l i = List.map (fun j -> j + i) l

let proj_single col = (col, col)

let proj_list = List.map proj_single

let proj_list_map new_cols old_cols = 
  List.map2 (fun a b -> (a, b)) new_cols old_cols

let rec singleton_record (env, aenv) loop (name, e) =
  let (q, cs, _, _) = compile_value (env, aenv) loop e in
    (q, [Cs.Mapping (name, cs)], dummy, dummy)

and extend_record (env, aenv) loop ext_fields =
  match ext_fields with
    | (name, e) :: [] -> 
	singleton_record (env, aenv) loop (name, e)
    | (name, e) :: tl ->
	let (q1, cs1, _, _) = singleton_record (env, aenv) loop (name, e) in
	let (q2, cs2, _, _) = extend_record (env, aenv) loop tl in
	let leafs2 = Cs.leafs cs2 in
	let new_names_q2 = Cs.items_of_offsets (incr leafs2 (Cs.cardinality cs1)) in
	let old_names_q2 = Cs.items_of_offsets leafs2 in
	let names_q1 = Cs.items_of_offsets (Cs.leafs cs1) in
	let q =
	  Algebra.Dag.mk_project
	    (proj_list ([Algebra.Iter 0; Algebra.Pos 0] @ new_names_q2 @ names_q1))
	    (ref (Algebra.Dag.mk_eqjoin
		    (Algebra.Iter 0, Algebra.Iter 1)
		    (ref q1)
		    (ref (Algebra.Dag.mk_project
			    [(Algebra.Iter 1, Algebra.Iter 0)]
			    (ref (Algebra.Dag.mk_project
				    (proj_list_map new_names_q2 old_names_q2)
				    (ref q2)))))))
	in
	let cs = Cs.append cs1 cs2 in
	  (q, cs, dummy, dummy)
    | [] ->
	failwith "CompileQuery.extend_record: empty ext_fields"

and compile_value (env, aenv) loop e =
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
    | `Extend (ext_fields, record) ->
	(match record with
	  | None ->
	      extend_record (env, aenv) loop (StringMap.to_alist ext_fields)
	  | Some _ ->
	      failwith "CompileQuery.value: not implemented")
    | _ ->
	failwith "CompileQuery.value: not implemented"

