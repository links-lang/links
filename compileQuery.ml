open Utility


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



  let fuse cs1 cs2 =
    if (List.length cs1) > (List.length cs2) then
      cs1
    else
      cs2

  let is_operand cs =
    if List.length cs <> 1 then
      false
    else
      match (List.hd cs) with
	| Offset _ -> true
	| _ -> false

  let record_field cs field =
    let rec loop = function
      | (Offset _) :: tl ->
	   loop tl
      | (Mapping (key, cs)) :: tl ->
	  if key = field then
	    cs
	  else
	    loop tl
      | [] ->
	  failwith "Cs.get_mapping: unknown field name"
    in
      loop cs
end

module A = Algebra
module AEnv = Env.Int

type tblinfo_node = A.Dag.dag ref * Cs.cs * unit * unit
type aenv = tblinfo_node AEnv.t

let dummy = ()

let incr l i = List.map (fun j -> j + i) l
let items_of_offsets = List.map (fun i -> A.Item i)

let proj1 col = (col, col)

let proj_list = List.map proj1

let proj_list_map new_cols old_cols = 
  List.map2 (fun a b -> (a, b)) new_cols old_cols

let lookup_env var env =
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

let wrap_1to1 f res c c' algexpr =
  A.Dag.mk_fun1to1
    (f, res, [c; c'])
    (ref algexpr)

let wrap_eq res c c' algexpr =
  A.Dag.mk_funnumeq
    (res, (c, c'))
    (ref algexpr)

let incr_col = function
  | A.Iter i -> A.Iter (i + 1)
  | A.Pos i -> A.Pos (i + 1)
  | A.Item i -> A.Item (i + 1)

let wrap_ne res c c' algexpr =
  let res' = incr_col res in
    A.Dag.mk_funboolnot
      (res, res')
      (ref (A.Dag.mk_funnumeq
	      (res', (c, c'))
	      (ref algexpr)))

let wrap_gt res c c' algexpr =
  A.Dag.mk_funnumgt
    (res, (c, c'))
    (ref algexpr)

let wrap_not res op_attr algexpr =
  A.Dag.mk_funboolnot
    (res, op_attr)
    (ref algexpr)

(* the empty list *)
let nil = ref (A.Dag.mk_emptytbl [(A.Iter 0, A.IntType); (A.Pos 0, A.IntType)])

(* record values and extend *)
let rec singleton_record (env, aenv) loop (name, e) =
  let (q, cs, _, _) = compile_value_node (env, aenv) loop e in
    (q, [Cs.Mapping (name, cs)], dummy, dummy)

and extend_record (env, aenv) loop ext_fields r =
  match ext_fields with
    | (name, e) :: [] -> 
	(match r with 
	   | Some record ->
	       merge_records (singleton_record (env, aenv) loop (name, e)) record
	   | None ->
	       singleton_record (env, aenv) loop (name, e))
    | (name, e) :: tl ->
	let new_field = singleton_record (env, aenv) loop (name, e) in
	let record = extend_record (env, aenv) loop tl r in
	  merge_records new_field record
    | [] ->
	failwith "CompileQuery.extend_record: empty ext_fields"

and merge_records (r1_q, r1_cs, _, _) (r2_q, r2_cs, _, _) =
  let r2_leafs = Cs.leafs r2_cs in
  let new_names_r2 = items_of_offsets (incr r2_leafs (Cs.cardinality r1_cs)) in
  let old_names_r2 = items_of_offsets r2_leafs in
  let names_r1 = items_of_offsets (Cs.leafs r1_cs) in
  let iter = A.Iter 0 in
  let iter' = A.Iter 1 in
  let q =
    A.Dag.mk_project
      (proj_list ([A.Iter 0; A.Pos 0] @ names_r1 @ new_names_r2))
      (ref (A.Dag.mk_eqjoin
	      (iter, iter')
	      (ref r1_q)
	      (ref ((A.Dag.mk_project
		       ((iter', iter) :: (proj_list_map new_names_r2 old_names_r2))
		       (ref r2_q))))))
  in
  let cs = Cs.append r1_cs r2_cs in
    (q, cs, dummy, dummy)

and project_record (env, aenv) loop field r =
  let (q_r, cs_r, _, _) = compile_value_node (env, aenv) loop r in
  let field_cs' = Cs.record_field cs_r field in
  let c_old = Cs.leafs field_cs' in
  let offset = List.hd c_old in
  let c_new = incr c_old (-offset + 1) in
  let field_cs = Cs.shift field_cs' (-offset + 1) in
  let iter = A.Iter 0 in
  let pos = A.Pos 0 in
  let q =
    A.Dag.mk_project
      ([proj1 iter; proj1 pos] @ proj_list_map (items_of_offsets c_new) (items_of_offsets c_old))
      (ref q_r)
  in
    (q, field_cs, dummy, dummy)

and singleton_record_value (env, aenv) loop (name, v) =
  let (q, cs, _, _) = compile_value (env, aenv) loop v in
    (q, [Cs.Mapping (name, cs)], dummy, dummy)

and compile_record_value (env, aenv) loop r =
  let singleton_record_value (name, v) =
    let (q, cs, _, _) = compile_value (env, aenv) loop v in
      (q, [Cs.Mapping (name, cs)], dummy, dummy)
  in
    match r with
      | (name, value) :: [] ->
	  singleton_record_value (name, value)
      | (name, value) :: tl ->
	  let f = singleton_record_value (name, value) in
	    merge_records f (compile_record_value (env, aenv) loop tl)
      | [] ->
	  failwith "CompileQuery.compile_record_value: empty record"

(* list values and cons *)
and compile_cons (env, aenv) loop hd_e tl_e =
  let hd = compile_value_node (env, aenv) loop hd_e in
  let tl = compile_value_node (env, aenv) loop tl_e in
    compile_list hd tl

and compile_list_value (env, aenv) loop list =
  match list with
    | hd_value :: tl_value ->
	let hd = compile_value (env, aenv) loop hd_value in
	let tl = compile_list_value (env, aenv) loop tl_value in
	  compile_list hd tl
    | value :: [] ->
	compile_value (env, aenv) loop value
    | [] -> 
	(!nil, [], dummy, dummy)

and compile_list (hd_q, hd_cs, _, _) (tl_q, tl_cs, _, _) =
  let fused_cs = Cs.fuse hd_cs tl_cs in
  let ord = A.Pos 2 in
  let pos = A.Pos 0 in
  let pos' = A.Pos 1 in
  let iter = A.Iter 0 in
  let q =
    A.Dag.mk_project
      ((proj1 iter) :: ((pos, pos') :: proj_list (items_of_offsets (Cs.leafs (fused_cs)))))
      (ref (A.Dag.mk_rank
	      (pos', [(ord, A.Ascending); (pos, A.Ascending)])
	      (ref (A.Dag.mk_disjunion
		      (ref (A.Dag.mk_attach
			      (ord, `Int (Num.Int 1))
			      (ref hd_q)))
		      (ref (A.Dag.mk_attach
			      (ord, `Int (Num.Int 2))
			      (ref tl_q)))))))
  in
    (q, fused_cs, dummy, dummy)


and compile_binop (env, aenv) loop wrapper operands =
  assert ((List.length operands) = 2);
  let (op1_q, op1_cs, _, _) = compile_value_node (env, aenv) loop (List.hd operands) in
  let (op2_q, op2_cs, _, _) = compile_value_node (env, aenv) loop (List.nth operands 1) in
    assert (Cs.is_operand op1_cs);
    assert (Cs.is_operand op2_cs);
    let iter = A.Iter 0 in
    let iter' = A.Iter 1 in
    let pos = A.Pos 0 in
    let c = A.Item 1 in
    let c' = A.Item 2 in
    let res = A.Item 3 in
    let q = 
      A.Dag.mk_project
	[(proj1 iter); (proj1 pos); (c, res)]
	(ref (wrapper 
		res c c'
		(A.Dag.mk_eqjoin
		   (iter, iter')
		   (ref op1_q)
		   (ref (A.Dag.mk_project
			   [(iter', iter); (c', c)]
			   (ref op2_q))))))
    in
      (q, op1_cs, dummy, dummy)

and compile_unop (env, aenv) loop wrapper operands =
  assert ((List.length operands) = 1);
  let (op_q, op_cs, _, _) = compile_value_node (env, aenv) loop (List.hd operands) in
    assert (Cs.is_operand op_cs);
    let c = A.Item 1 in
    let res = A.Item 2 in
    let pos = A.Pos 0 in
    let iter = A.Iter 0 in
    let q = 
      A.Dag.mk_project
	[proj1 iter; proj1 pos; (c, res)]
	(ref (wrapper
		res c
		op_q))
    in
      (q, op_cs, dummy, dummy)

and function_dispatch (env, aenv) loop op args =
  match op with
    | `PrimitiveFunction "Cons" ->
	assert ((List.length args) = 2);
	compile_cons (env, aenv) loop (List.nth args 0) (List.nth args 1)
    | `PrimitiveFunction "+" 
    | `PrimitiveFunction "+." -> compile_binop (env, aenv) loop (wrap_1to1 A.Add) args
    | `PrimitiveFunction "-" 
    | `PrimitiveFunction "-." -> compile_binop (env, aenv) loop (wrap_1to1 A.Subtract) args
    | `PrimitiveFunction "*"
    | `PrimitiveFunction "*." -> compile_binop (env, aenv) loop (wrap_1to1 A.Multiply) args
    | `PrimitiveFunction "/" 
    | `PrimitiveFunction "/." -> compile_binop (env, aenv) loop (wrap_1to1 A.Divide) args
    | `PrimitiveFunction "==" -> compile_binop (env, aenv) loop wrap_eq args
    | `PrimitiveFunction ">" -> compile_binop (env, aenv) loop wrap_gt args
    | `PrimitiveFunction "<" -> compile_binop (env, aenv) loop wrap_gt (List.rev args)
    | `PrimitiveFunction "<>" -> compile_binop (env, aenv) loop wrap_ne args
    | `PrimitiveFunction "not" -> compile_unop (env, aenv) loop wrap_not args
    | `PrimitiveFunction ">="
    | `PrimitiveFunction "<="
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

(* values from the environment (value.ml) *)
and compile_value (env, aenv) loop v =
  match v with
    | #Value.primitive_value_basis as p ->
	compile_constant loop (constant_of_primitive_value p)
    | `List l ->
	compile_list_value (env, aenv) loop l
    | `Record r ->
	compile_record_value (env, aenv) loop r
    | `ClientFunction _
    | `Continuation _
    | `FunctionPtr _
    | `PrimitiveFunction _
    | `RecFunction _

    | `Variant _ 
    | _ ->
	failwith "CompileQuery.compile_value: not implemented"

(* value nodes from the IR (ir.ml) *)
and compile_value_node (env, aenv) loop e =
  match e with
    | `Constant c->
	compile_constant loop c
    | `Extend (ext_fields, record) ->
	(match record with
	   | None ->
	       extend_record (env, aenv) loop (StringMap.to_alist ext_fields) None
	   | Some value ->
	       let record_value = compile_value_node (env, aenv) loop value in
		 extend_record (env, aenv) loop (StringMap.to_alist ext_fields) (Some record_value))
    | `Project (field_name, r) ->
	project_record (env, aenv) loop field_name r
    | `ApplyPure (f, args) ->
	(match f with
	   | `TApp (`Variable var, _) ->
	       let value = 
		 match lookup_env var env with
		   | Some v -> v
		   | None -> failwith "CompileQuery.compile_value_node: var not found"
	       in
		 function_dispatch (env, aenv) loop value args
	   | _ ->
	       failwith "CompileQuery.compile_value_node: `ApplyPure no `TApp value")
    | `TApp (v, _) ->
	compile_value_node (env, aenv) loop v
    | `Variable var -> 
	(try
	  AEnv.lookup aenv var
	with NotFound _ ->
	  (match lookup_env var env with
	    | Some v -> compile_value (env, aenv) loop v
	    | None -> failwith "CompileQuery.compile_value_node: var not found"))
    | _ ->
	failwith "CompileQuery.value_node: not implemented"

and compile_tail_computation (env, aenv) loop tailcomp =
  match tailcomp with
    | `Return value ->
	compile_value_node (env, aenv) loop value
    | _ ->
	failwith "CompileQuery.compile_tail_computation: not implemented"

and compile_computation (env, aenv) loop (bindings, tailcomp) =
  let aenv = 
    List.fold_left
      (fun aenv' binding ->
	match binding with
	 | `Let (binder, (_tyvars, tailcomp)) ->
	     let tin = compile_tail_computation (env, aenv) loop tailcomp in
	       AEnv.bind aenv' ((Var.var_of_binder binder), tin)
	 | _ ->
	     failwith "CompileQuery.compile_computation: not implemented")
      aenv
      bindings
  in
    compile_tail_computation (env, aenv) loop tailcomp

let compile env e =
  let loop = 
    (ref (A.Dag.mk_littbl
	    ([[`Int (Num.Int 1)]], [(A.Iter 0, A.IntType)])))
  in
  let (q, cs, _, _) = compile_computation (env, AEnv.empty) loop e in
  let q = ref q in
  let dag = 
    A.Dag.mk_serializerel 
      (A.Iter 0, A.Pos 0, items_of_offsets (Cs.leafs cs))
      (ref (A.Dag.mk_nil))
      q
  in
    A.Dag.export_plan "plan.xml" (ref dag)
