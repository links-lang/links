open Utility

module A = Algebra

(* Table information node (q, cs, itbls, _) *)


module Cs = struct

  type offset = int
  type cs = csentry list
  and csentry =
    | Offset of offset
    | Mapping of string * cs

  (* return all columns *)	
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

  (* increase all column names by i *)
  let rec shift cs i =
    List.map
      (function
	 | Offset o -> Offset (o + i)
	 | Mapping (key, cs) -> Mapping (key, (shift cs i)))
      cs

  (* append to cs components *)
  let append cs1 cs2 =
    cs1 @ (shift cs2 (cardinality cs1))

  (* fuse two cs's by choosing the larger one *)
  let fuse cs1 cs2 =
    if (List.length cs1) > (List.length cs2) then
      cs1
    else
      cs2

  (* true iff cs has exactly one flat column *)
  let is_operand cs =
    if List.length cs <> 1 then
      false
    else
      match (List.hd cs) with
	| Offset _ -> true
	| _ -> false

  (* look up the column corresponding to a record field *)
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

type tblinfo = Ti of (A.Dag.dag ref * Cs.cs * ((int * tblinfo) list) * unit)

module Itbls = struct
  let empty = []
  let keys itbls = List.map fst itbls
  let incr_keys itbls i = List.map (fun (offset, ti) -> (offset + i, ti)) itbls
  let decr_keys itbls i =  List.map (fun (offset, ti) -> (offset - i, ti)) itbls

  (* remove all mappings whose offsets are not in keys *)
  let retain_by_keys itbls keys = 
    let p i j = i = j in
    let f l (offset, _ti) = List.exists (p offset) l in
      List.filter (f keys) itbls
end

module AEnv = Env.Int
type aenv = tblinfo AEnv.t

let dummy = ()

let incr l i = List.map (fun j -> j + i) l
let items_of_offsets = List.map (fun i -> A.Item i)

let proj1 col = (col, col)

let proj_list = List.map proj1

let proj_list_map new_cols old_cols = 
  List.map2 (fun a b -> (a, b)) new_cols old_cols

let wrap_1to1 f res c c' algexpr =
  A.Dag.mk_fun1to1
    (f, res, [c; c'])
    algexpr

let wrap_eq res c c' algexpr =
  A.Dag.mk_funnumeq
    (res, (c, c'))
    algexpr

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
	      algexpr))

let wrap_gt res c c' algexpr =
  A.Dag.mk_funnumgt
    (res, (c, c'))
    algexpr

let wrap_not res op_attr algexpr =
  A.Dag.mk_funboolnot
    (res, op_attr)
    algexpr

(* Naming conventions for Iter and Pos columns. Use only these names! *)
let iter = A.Iter 0 
let iter' = A.Iter 1
let inner = A.Iter 2
let outer = A.Iter 3 
let pos = A.Pos 0 
let pos' = A.Pos 1
let ord = A.Pos 2
let ord' = A.Pos 3
let item' = A.Pos 4
let item'' = A.Pos 5
let c' = A.Pos 6

(*
let rec suap q_paap (it1 : (int * tblinfo) list) (it2 : (int * tblinfo) list) =
  match (it1, it2) with
    | (c1, Ti (q_1, cs1, subs_1, _)) :: subs_hat, ((_, Ti (q_2, cs2, subs_2, _)) :: subs_tilde) ->
	let q =
	  ref (A.Dag.mk_rownum 
		 (item', [(iter, A.Ascending); (ord, A.Ascending); (pos, A.Ascending)], None)
		 (ref (A.Dag.mk_disjunion
			 (ref (A.Dag.mk_attach
				 (ord, A.Nat 1n)
				 q_1))
			 (ref (A.Dag.mk_attach
				 (ord, A.Nat 2n)
				 q_2)))))
	    in
	  let q'_projlist = [(iter, item''); proj1 pos] in
	  let q'_projlist = q'_projlist @ (proj_list (items_of_offsets (difference (Cs.leafs cs1) (Itbls.keys subs_1)))) in
	  let q'_projlist = q'_projlist @ [((List.hd (items_of_offsets (Itbls.keys subs_1))), item')] in
	  let q' =
	    ref (A.Dag.mk_project
		   q'_projlist
		   (ref (A.Dag.mk_thetajoin
			   [(A.Eq, (ord, ord')); (A.Eq, (iter, c'))]
			   q
			   (ref (A.Dag.mk_project
				   [(ord', ord); (item'', item'); (c', A.Item c1)]
				   q_paap)))))
	  in
	    [(c1, (q', (Cs.fuse cs1 cs2), [], dummy))] (* (suap q_paap subs_hat subs_tilde) *)
    | [], [] ->
	[]
    | _ -> assert false
*)

let wrap_agg loop q attachment =
  ref (A.Dag.mk_attach
	 (pos, A.Nat 1n)
	 (ref (A.Dag.mk_disjunion
		 q
		 (ref (A.Dag.mk_attach
			 (A.Item 1, attachment)
			 (ref (A.Dag.mk_difference
				 loop
				 (ref (A.Dag.mk_project
					 [proj1 iter]
					 q)))))))))

(* the empty list *)
let nil = ref (A.Dag.mk_emptytbl [(A.Iter 0, A.NatType); (A.Pos 0, A.NatType)])

(* loop-lift q by map *)
let lift map (Ti (q, cs, _, _)) =
  let q' =
    (ref (A.Dag.mk_project
	    ([(iter, inner); proj1 pos] @ (proj_list (items_of_offsets (Cs.leafs cs))))
	    (ref (A.Dag.mk_eqjoin
		    (iter, outer)
		    q
		    map))))
  in
    Ti (q', cs, Itbls.empty, dummy)

(* construct the ordering map of a for-loop *)
let rec omap map sort_criteria sort_cols =
  match (sort_criteria, sort_cols) with
    | (o :: os), (col :: cols) ->
	ref (A.Dag.mk_project
	       ([proj1 outer; proj1 inner; (col, A.Item 1)] @ (proj_list cols))
	       (ref (A.Dag.mk_eqjoin
		       (inner, iter)
		    	       (omap map os cols)
		       o)))
    | [], [] ->
	map
    | _ -> assert false

(* compute absolute positions for q *)
let abspos q cols =
  ref (A.Dag.mk_project
	 ([proj1 iter; proj1 pos] @ (proj_list cols))
	 (ref (A.Dag.mk_rownum
		 (pos, [(pos', A.Ascending)], Some iter)
		 (ref (A.Dag.mk_project
		    ([proj1 iter; (pos', pos)] @ (proj_list cols))
		    q)))))

let rec compile_box env loop e =
  let ti_e = compile_expression env loop e in
  let q_o = 
    ref (A.Dag.mk_attach
	   (pos, A.Nat 1n)
	   (ref (A.Dag.mk_project 
		   [(proj1 iter); (A.Item 1, iter)]
		   loop)))
      in
    Ti(q_o, [Cs.Offset 1], [(1, ti_e)], dummy)

and compile_unbox env loop e =
  let Ti(_, cs, itbls, _) = compile_expression env loop e in
    assert ((Cs.cardinality cs) = 1);
    assert ((List.length itbls) = 1);
    snd (List.hd itbls)

and compile_append env loop l =
  match l with
    | e :: [] ->
	compile_expression env loop e
    | hd_e :: tl_e ->
	let hd = compile_expression env loop hd_e in
	let tl = compile_append env loop tl_e in
	  compile_list hd tl
    | [] ->
	Ti (nil, [], Itbls.empty, dummy)

and compile_list (Ti (hd_q, hd_cs, _, _)) (Ti (tl_q, tl_cs, _, _)) =
  let fused_cs = Cs.fuse hd_cs tl_cs in
  let ord = A.Pos 2 in
  let pos = A.Pos 0 in
  let pos' = A.Pos 1 in
  let iter = A.Iter 0 in
  let q =
    ref (A.Dag.mk_project
	   ((proj1 iter) :: ((pos, pos') :: proj_list (items_of_offsets (Cs.leafs (fused_cs)))))
	   (ref (A.Dag.mk_rank
		   (pos', [(ord, A.Ascending); (pos, A.Ascending)])
		   (ref (A.Dag.mk_disjunion
			   (ref (A.Dag.mk_attach
				   (ord, A.Nat 1n)
				   hd_q))
			   (ref (A.Dag.mk_attach
				   (ord, A.Nat 2n)
				   tl_q)))))))
  in
    Ti (q, fused_cs, Itbls.empty, dummy)


and compile_length env loop args =
  assert ((List.length args) = 1);
  let e = List.hd args in
  let Ti (q_e, _, _, _) = compile_expression env loop e in
  let q = 
    ref (A.Dag.mk_funaggrcount
	   (A.Item 1, Some iter)
	   q_e)
  in
  let q' = wrap_agg loop q (A.Nat 1n) in
    Ti (q', [Cs.Offset 1], Itbls.empty, dummy)

and compile_aggr env loop aggr_fun args =
  assert ((List.length args) = 1);
  let c = A.Item 1 in
  let e = List.hd args in
  let Ti (q_e, cs_e, _, _) = compile_expression env loop e in
    assert (Cs.is_operand cs_e);
    let q = 
      (ref (A.Dag.mk_funaggr
	      (aggr_fun, (c, c), Some iter)
	      q_e))
    in
    let q' = wrap_agg loop q (A.String "error") in
      Ti (q', [Cs.Offset 1], Itbls.empty, dummy)

and compile_nth env loop operands =
  assert ((List.length operands) = 2);
  let Ti (q1, _, _, _) = compile_expression env loop (List.hd operands) in
  let Ti (q2, cs2, _, _) = compile_expression env loop (List.nth operands 1) in
  let q2' = abspos q2 (items_of_offsets (Cs.leafs cs2)) in
  let offset = List.length (Cs.leafs cs2) in
  let c' = A.Item (offset + 1) in
  let res = A.Item (offset + 2) in
  let q =
    (ref (A.Dag.mk_project
	    ([proj1 iter; proj1 pos] @ proj_list (items_of_offsets (Cs.leafs cs2)))
	    (ref (A.Dag.mk_select
		    res
		    (ref (A.Dag.mk_funnumeq
			    (res, (pos', c'))
			    (ref (A.Dag.mk_eqjoin
				    (iter, iter')
				    (ref (A.Dag.mk_cast
					    (pos', pos, A.IntType)
					    q2'))
				    (ref (A.Dag.mk_project
					    [(iter', iter); (c', A.Item 1)]
					    q1))))))))))
  in
    Ti (q, cs2, Itbls.empty, dummy)

and compile_binop env loop wrapper operands =
  assert ((List.length operands) = 2);
  let Ti (op1_q, op1_cs, _, _) = compile_expression env loop (List.hd operands) in
  let Ti (op2_q, op2_cs, _, _) = compile_expression env loop (List.nth operands 1) in
    assert (Cs.is_operand op1_cs);
    assert (Cs.is_operand op2_cs);
    let c = A.Item 1 in
    let c' = A.Item 2 in
    let res = A.Item 3 in
    let q = 
      ref (A.Dag.mk_project
	     [(proj1 iter); (proj1 pos); (c, res)]
	     (ref (wrapper 
		     res c c'
		     (ref (A.Dag.mk_eqjoin
			     (iter, iter')
			     op1_q
			     (ref (A.Dag.mk_project
				     [(iter', iter); (c', c)]
				     op2_q)))))))
    in
      Ti (q, op1_cs, Itbls.empty, dummy)

and compile_unop env loop wrapper operands =
  assert ((List.length operands) = 1);
  let Ti (op_q, op_cs, _, _) = compile_expression env loop (List.hd operands) in
    assert (Cs.is_operand op_cs);
    let c = A.Item 1 in
    let res = A.Item 2 in
    let q = 
      ref (A.Dag.mk_project
	     [proj1 iter; proj1 pos; (c, res)]
	     (ref (wrapper
		     res c
		     op_q)))
    in
      Ti (q, op_cs, Itbls.empty, dummy)

and compile_apply env loop f args =
  match f with
    | "+" 
    | "+." -> compile_binop env loop (wrap_1to1 A.Add) args
    | "-" 
    | "-." -> compile_binop env loop (wrap_1to1 A.Subtract) args
    | "*"
    | "*." -> compile_binop env loop (wrap_1to1 A.Multiply) args
    | "/" 
    | "/." -> compile_binop env loop (wrap_1to1 A.Divide) args
    | "==" -> compile_binop env loop wrap_eq args
    | ">" -> compile_binop env loop wrap_gt args
    | "<>" -> compile_binop env loop wrap_ne args
    | "not" -> compile_unop env loop wrap_not args
    | "nth" -> compile_nth env loop args
    | "length" -> compile_length env loop args
    | "maxf" -> compile_aggr env loop A.Max args
(*    | "min" -> compile_aggr env loop A.Min args
    | "avg" -> compile_aggr env loop A.Avg args
    | "sum" -> compile_aggr env loop A.Sum args *)
    | "<" | "<=" | ">=" ->
	failwith ("CompileQuery.compile_apply: </<=/>= should have been rewritten in query2")
    | s ->
	failwith ("CompileQuery.op_dispatch: " ^ s ^ " not implemented")
	  (*
	    | `PrimitiveFunction "Concat" ->
	    | `PrimitiveFunction "take" ->
	    | `PrimitiveFunction "drop" ->
	    | `PrimitiveFunction "max" ->
	    | `PrimitiveFunction "min" ->
	    | `PrimitiveFunction "hd" ->
	    | `PrimitiveFunction "tl" ->
	  *)

and compile_for env loop v e1 e2 order_criteria =
  let Ti (q1, cs1, _, _) = compile_expression env loop e1 in
  let q_v = 
    ref (A.Dag.mk_rownum
	   (inner, [(iter, A.Ascending); (pos, A.Ascending)], None)
	   q1)
  in
  let map =
    ref (A.Dag.mk_project
	   [(outer, iter); proj1 inner]
	   q_v)
  in
  let loop_v =
    ref (A.Dag.mk_project
	   [(iter, inner)]
	   q_v)
  in
  let q_v' =
    ref (A.Dag.mk_attach
	   (pos, A.Nat 1n)
	   (ref (A.Dag.mk_project
		   ([(iter, inner)] @ (proj_list (items_of_offsets (Cs.leafs cs1))))
		   q_v)))
  in
  let env = AEnv.map (lift map) env in
  let env_v = AEnv.bind env (v, Ti (q_v', cs1, Itbls.empty, dummy)) in
  let Ti (q2, cs2, _, _) = compile_expression env_v loop_v e2 in
  let (order_cols, map') =
    match order_criteria with
      | _ :: _ ->
	  (* compile orderby expressions *)
	  let q_os = List.map (compile_expression env_v loop_v) order_criteria in
	  let q_os = List.map (fun (Ti (q, _, _, _)) -> q) q_os in
	  let offset = (List.length (Cs.leafs cs2)) + 1 in
	  let cols = mapIndex (fun _ i -> A.Item (i + offset)) q_os in
	  let order_cols = List.map (fun c -> (c, A.Ascending)) (cols @ [pos]) in
	    (order_cols, omap map q_os cols)
	    
      | [] ->
	  ([(iter, A.Ascending); (pos, A.Ascending)], map)
  in
  let q =
    ref (A.Dag.mk_project
	   ([(iter, outer); (pos, pos')] @ (proj_list (items_of_offsets (Cs.leafs cs2))))
	   (ref (A.Dag.mk_rank
		   (pos', order_cols)
		   (ref (A.Dag.mk_eqjoin
			   (inner, iter)
			   map'
			   q2)))))
  in
    Ti (q, cs2, Itbls.empty, dummy)

and singleton_record env loop (name, e) =
  let Ti (q, cs, _, _) = compile_expression env loop e in
    Ti (q, [Cs.Mapping (name, cs)], Itbls.empty, dummy)

and extend_record env loop ext_fields r =
  assert (match ext_fields with [] -> false | _ -> true);
  match ext_fields with
    | (name, e) :: [] -> 
	(match r with 
	   | Some record ->
	       merge_records (singleton_record env loop (name, e)) record
	   | None ->
	       singleton_record env loop (name, e))
    | (name, e) :: tl ->
	let new_field = singleton_record env loop (name, e) in
	let record = extend_record env loop tl r in
	  merge_records new_field record
    | [] ->
	failwith "CompileQuery.extend_record: empty ext_fields"

and merge_records (Ti (r1_q, r1_cs, _, _)) (Ti (r2_q, r2_cs, _, _)) =
  let r2_leafs = Cs.leafs r2_cs in
  let new_names_r2 = items_of_offsets (incr r2_leafs (Cs.cardinality r1_cs)) in
  let old_names_r2 = items_of_offsets r2_leafs in
  let names_r1 = items_of_offsets (Cs.leafs r1_cs) in
  let q =
    ref (A.Dag.mk_project
	   (proj_list ([A.Iter 0; A.Pos 0] @ names_r1 @ new_names_r2))
	   (ref (A.Dag.mk_eqjoin
		   (iter, iter')
		   r1_q
		   (ref ((A.Dag.mk_project
			    ((iter', iter) :: (proj_list_map new_names_r2 old_names_r2))
			    r2_q))))))
  in
  let cs = Cs.append r1_cs r2_cs in
    Ti (q, cs, Itbls.empty, dummy)

and compile_project env loop field r =
  let Ti (q_r, cs_r, _, _) = compile_expression env loop r in
  let field_cs' = Cs.record_field cs_r field in
  let c_old = Cs.leafs field_cs' in
  let offset = List.hd c_old in
  let c_new = incr c_old (-offset + 1) in
  let field_cs = Cs.shift field_cs' (-offset + 1) in
  let q =
    ref (A.Dag.mk_project
	   ([proj1 iter; proj1 pos] @ proj_list_map (items_of_offsets c_new) (items_of_offsets c_old))
	   q_r)
  in
    Ti (q, field_cs, Itbls.empty, dummy)

and compile_record env loop r =
  match r with
    | (name, value) :: [] ->
	singleton_record env loop (name, value)
    | (name, value) :: tl ->
	let f = singleton_record env loop (name, value) in
	  merge_records f (compile_record env loop tl)
    | [] ->
	failwith "CompileQuery.compile_record_value: empty record"

(* HACK HACK HACK: rewrite this when Value.table stores key information *)
and compile_table loop ((_db, _params), tblname, _row) =
  Printf.printf "tblname = %s\n" tblname;
  flush stdout;
  assert (tblname = "test1");
  let columns = ["foo"; "bar"] in
  let col_pos = mapIndex (fun c i -> (c, (i + 1))) columns in
  let items = List.map (fun (c, i) -> (c, A.Item i)) col_pos in
  let cs = List.map (fun (c, i) -> Cs.Mapping (c, [Cs.Offset i])) col_pos in
  let pos = A.Pos 0 in
  let key_infos = [[A.Item 1]] in
  let attr_infos = List.map (fun (tname, name) -> (name, tname, A.IntType)) items in
  let q =
    ref (A.Dag.mk_cross
	   loop
	   (ref (A.Dag.mk_rank
		   (pos, (List.map (fun (_, name) -> (name, A.Ascending)) items))
		   (ref (A.Dag.mk_tblref
			   (tblname, attr_infos, key_infos))))))
  in
    Ti (q, cs, Itbls.empty, dummy)

and compile_constant loop (c : Constant.constant) =
  let cs = [Cs.Offset 1] in
  let q =
    (ref (A.Dag.mk_attach
	    (A.Item 1, A.const c)
	    (ref (A.Dag.mk_attach
		    (A.Pos 0, A.Nat 1n)
		    loop))))
  in
    Ti (q, cs, Itbls.empty, dummy)

and compile_if env loop e1 e2 e3 =
  let c = A.Item 1 in
  let res = A.Item 2 in
    
  let select loop (Ti (q, cs, _, _)) =
    let cols = items_of_offsets (Cs.leafs cs) in
      let q' =
	ref (A.Dag.mk_project
	   ([proj1 iter; proj1 pos] @ (proj_list cols))
	   (ref (A.Dag.mk_eqjoin
		   (iter, iter')
		   q
		   (ref (A.Dag.mk_project
			   [(iter', iter)]
			   loop)))))
      in
	Ti (q', cs, Itbls.empty, dummy)
  in
  (* condition *)
  let Ti (q_e1, cs_e1, _, _) = compile_expression env loop e1 in
    assert (Cs.is_operand cs_e1);
    let loop_then =
      ref (A.Dag.mk_project
	     [proj1 iter]
	     (ref (A.Dag.mk_select
		     c
		     q_e1)))
    in
      let loop_else =
	ref (A.Dag.mk_project
	       [proj1 iter]
	       (ref (A.Dag.mk_select
		       res
		       (ref (A.Dag.mk_funboolnot
			       (res, c)
			       q_e1)))))
      in
      let env_then = AEnv.map (select loop_then) env in
      let env_else = AEnv.map (select loop_else) env in
      let Ti (q_e2, cs_e2, _, _) = compile_expression env_then loop_then e2 in
      let Ti (q_e3, _cs_e3, _, _) = compile_expression env_else loop_else e3 in
      let q =
	ref (A.Dag.mk_disjunion
	       q_e2
	       q_e3)
      in
	Ti (q, cs_e2, Itbls.empty, dummy)

and compile_expression env loop e : tblinfo =
  match e with
    | `Constant c -> compile_constant loop c
    | `Apply (f, args) -> compile_apply env loop f args
    | `Var x -> AEnv.lookup env x
    | `Project (r, field) -> compile_project env loop field r
    | `Record r -> compile_record env loop (StringMap.to_alist r)
    | `Extend (r, ext_fields) ->
	let ext_fields = StringMap.to_alist ext_fields in
	  extend_record env loop ext_fields (opt_map (compile_expression env loop) r)
    | `Singleton e -> compile_expression env loop e
    | `Append l -> compile_append env loop l
    | `Table t -> compile_table loop t
    | `If (c, t, e) -> compile_if env loop c t e
    | `For ([x, l], os, body) -> compile_for env loop x l body os
    | `For _ -> failwith "compile_expression: multi-generator for-expression not implemented"
    | `Box e -> compile_box env loop e
    | `Unbox e -> compile_unbox env loop e
    | `Erase _
    | `Closure _
    | `Variant _
    | `XML _ -> failwith "compile_expression: not implemented"
    | `Primitive _ -> failwith "compile_expression: eval error"

let compile e =
  let loop = 
    (ref (A.Dag.mk_littbl
	    ([[A.Nat 1n]], [(A.Iter 0, A.NatType)])))
  in
  let Ti (q, cs, _, _) = compile_expression AEnv.empty loop e in
  let dag = 
    A.Dag.mk_serializerel 
      (A.Iter 0, A.Pos 0, items_of_offsets (Cs.leafs cs))
      (ref (A.Dag.mk_nil))
      q
  in
    A.Dag.export_plan "plan.xml" (ref dag)
