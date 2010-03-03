open Utility

module A = Algebra

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

  let items_of_offsets = List.map (fun i -> A.Item i)

  let fuse cs1 cs2 =
    if (List.length cs1) > (List.length cs2) then
      cs1
    else
      cs2
    
end

let incr l i = List.map (fun j -> j + i) l

let proj_single col = (col, col)

let proj_list = List.map proj_single

let proj_list_map new_cols old_cols = 
  List.map2 (fun a b -> (a, b)) new_cols old_cols

let lookup_var var env =
  match Value.lookup var env with
    | Some v -> Some v
    | None -> Some (Lib.primitive_stub (Lib.primitive_name var))

let constant_of_primitive_value (value : Value.primitive_value_basis) : Constant.constant =
  match value with
    | `NativeString s -> `String s
    | `XML _ -> failwith "no xml in query allowed"
    | `Database _ -> failwith "foo"
    | `Table _ -> failwith "foo"
    | `Float f -> `Float f
    | `Int i -> `Int i
    | `String s -> `String s
    | `Bool b -> `Bool b
    | `Char c -> `Char c

let nil = ref (A.Dag.mk_emptytbl [(A.Iter 0, A.IntType); (A.Pos 0, A.IntType)])

let rec singleton_record (env, aenv) loop (name, e) =
  let (q, cs, _, _) = compile_value_node (env, aenv) loop e in
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
	let iter = A.Iter 0 in
	let iter' = A.Iter 1 in
	let q =
	  A.Dag.mk_project
	    (proj_list ([A.Iter 0; A.Pos 0] @ new_names_q2 @ names_q1))
	    (ref (A.Dag.mk_eqjoin
		    (iter, iter')
		    (ref q1)
		    (ref ((A.Dag.mk_project
			     ((iter', iter) :: (proj_list_map new_names_q2 old_names_q2))
			     (ref q2))))))
	in
	let cs = Cs.append cs1 cs2 in
	  (q, cs, dummy, dummy)
    | [] ->
	failwith "CompileQuery.extend_record: empty ext_fields"
	  
and compile_cons (env, aenv) loop hd_e tl_e =
  let (q1, cs1, _, _) = compile_value_node (env, aenv) loop hd_e in
  let (q2, cs2, _, _) = compile_value_node (env, aenv) loop tl_e in
  let fused_cs = Cs.fuse cs1 cs2 in
  let ord = A.Pos 2 in
  let pos = A.Pos 0 in
  let pos' = A.Pos 1 in
  let iter = A.Iter 0 in
  let q =
    A.Dag.mk_project
      ((proj_single iter) :: ((pos, pos') :: proj_list (Cs.items_of_offsets (Cs.leafs (fused_cs)))))
      (ref (A.Dag.mk_rank
	      (pos', [(ord, A.Ascending); (pos, A.Ascending)])
	      (ref (A.Dag.mk_disjunion
		      (ref (A.Dag.mk_attach
			      (ord, `Int (Num.Int 1))
			      (ref q1)))
		      (ref (A.Dag.mk_attach
			      (ord, `Int (Num.Int 2))
			      (ref q2)))))))
  in
    (q, fused_cs, dummy, dummy)

and op_dispatch (env, aenv) loop op args =
  match op with
    | `PrimitiveFunction "Cons" ->
	assert ((List.length args) = 2);
	compile_cons (env, aenv) loop (List.nth args 0) (List.nth args 1)
    | _ ->
	failwith "CompileQuery.op_dispatch: not implemented"
(*
    | `PrimitiveFunction "Concat" ->
    | `PrimitiveFunction "take" ->
    | `PrimitiveFunction "drop" ->
    | `PrimitiveFunction "max" ->
    | `PrimitiveFunction "min" ->
    | `PrimitiveFunction "hd" ->
    | `PrimitiveFunction "tl" ->
*)

and compile_constant loop (const : Constant.constant) =
  let cs = [Cs.Offset 1] in
  let q =
    (A.Dag.mk_attach
       (A.Item 1, const)
       (ref (A.Dag.mk_attach
	       (A.Pos 0, `Int (Num.Int 1))
	       loop)))
  in
    (q, cs, dummy, dummy)

and compile_value (_env, _aenv) loop v =
  match v with
    | #Value.primitive_value_basis as p ->
	compile_constant loop (constant_of_primitive_value p)
    | `List [] ->
	(!nil, [], dummy, dummy)
    | `List _
    | `ClientFunction _
    | `Continuation _
    | `FunctionPtr _
    | `PrimitiveFunction _
    | `RecFunction _
    | `Record _
    | `Variant _ 
    | _ ->
	failwith "CompileQuery.compile_value: not implemented"

and compile_value_node (env, aenv) loop e =
  match e with
    | `Constant c->
	compile_constant loop c
    | `Extend (ext_fields, record) ->
	(match record with
	  | None ->
	      extend_record (env, aenv) loop (StringMap.to_alist ext_fields)
	  | Some _ ->
	      failwith "CompileQuery.compile_value_node: not implemented")
    | `ApplyPure (f, args) ->
	(match f with
	   | `TApp (`Variable var, _) ->
	       let value = 
		 match lookup_var var env with
		   | Some v -> v
		   | None -> failwith "CompileQuery.compile_value_node: var not found"
	       in
		 op_dispatch (env, aenv) loop value args
	   | _ ->
	       failwith "CompileQuery.compile_value_node: `ApplyPure no `TApp value")
    | `TApp (v, _) ->
	compile_value_node (env, aenv) loop v
    | `Variable var -> 
	let value = 
	  match lookup_var var env with
	    | Some v -> v
	    | None -> failwith "CompileQuery.compile_value_node: var not found"
	in
	  compile_value (env, aenv) loop value
    | _ ->
	failwith "CompileQuery.value_node: not implemented"

and compile_computation (env, aenv) loop (_binders, tailcomp) =
    match tailcomp with
      | `Return value ->
	  compile_value_node (env, aenv) loop value
      | _ ->
	  failwith "CompileQuery.compile_computation: not implemented"

let compile env e =
  let loop = 
    (ref (A.Dag.mk_littbl
	    ([[`Int (Num.Int 1)]], [(A.Iter 0, A.IntType)])))
  in
  let (q, cs, _, _) = compile_computation (env, ()) loop e in
  let q = ref q in
  let dag = 
    A.Dag.mk_serializerel 
      (A.Iter 0, A.Pos 0, Cs.items_of_offsets (Cs.leafs cs))
      (ref (A.Dag.mk_nil))
      q
  in
    A.Dag.export_plan "plan.xml" (ref dag)
