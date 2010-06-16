open Utility

module A = Algebra

type tblinfo = Ti of (A.Dag.dag ref * Cs.cs * ((int * tblinfo) list) * unit)
let q_of_tblinfo = function Ti (q, _, _, _) -> q

module Itbls = struct
  let empty = []
  let keys itbls = List.map fst itbls
  let incr_keys itbls i = List.map (fun (offset, ti) -> (offset + i, ti)) itbls
  let decr_keys itbls i =  List.map (fun (offset, ti) -> (offset - i, ti)) itbls
  let append = List.append
  let lookup = List.assoc
  let length = List.length

  (* remove all mappings whose offsets are not in keys *)
  let retain_by_keys itbls keys = 
    let p i j = i = j in
    let f l (offset, _ti) = List.exists (p offset) l in
      List.filter (f keys) itbls
end

let base_type_of_typ t = 
  let concrete_t = Types.concrete_type t in
  match concrete_t with
  | `Primitive `Bool -> `BoolType
  | `Primitive `Int -> `IntType
  | `Primitive `Char -> `CharType
  | `Primitive `Float -> `FloatType
  | `Primitive `NativeString -> `StrType
  | `Alias (("String", []), _) -> `StrType
  | `Application (l, [`Type (`Primitive `Char)]) when Types.Abstype.eq_t.Eq.eq l Types.list -> `StrType
  | _ -> failwith ("unsupported type " ^ (Types.string_of_datatype concrete_t))

let is_primitive_col = function
  | `Surrogate -> false
  | _ -> true

module AEnv = Env.Int
type aenv = tblinfo AEnv.t

let dummy = ()

let incr l i = List.map (fun j -> j + i) l
let decr l i = List.map (fun j -> j - i) l
let io = List.map (fun i -> A.Item i)

let prj col = (col, col)

let prjlist = List.map prj

let prjlist_map new_cols old_cols = 
  List.map2 (fun a b -> (a, b)) new_cols old_cols

let prjlist_single new_cols old_col =
  List.map (fun a -> (a, old_col)) new_cols

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
let grp_key = A.Pos 6
let c' = A.Pos 7
let res = A.Pos 8
let res' = A.Pos 9
let res'' = A.Pos 10
let pos'' = A.Pos 11


let incr_col = function
  | A.Iter i -> A.Iter (i + 1)
  | A.Pos i -> A.Pos (i + 1)
  | A.Item i -> A.Item (i + 1)

(* wrapper for binary functions/operators *)
let wrap_1to1 f res c c' algexpr =
  A.Dag.mk_fun1to1
    (f, res, [c; c'])
    algexpr

(* wrapper for logical and *)
let wrap_and res c c' algexpr =
  A.Dag.mk_funbooland
    (res, (c, c'))
    algexpr

(* wrapper for logical or *)
let wrap_or res c c' algexpr =
  A.Dag.mk_funboolor
    (res, (c, c'))
    algexpr

(* wrapper for equal *)
let wrap_eq res c c' algexpr =
  A.Dag.mk_funnumeq
    (res, (c, c'))
    algexpr

(* wrapper for not equal *)
let wrap_ne res c c' algexpr =
  let res' = incr_col res in
    A.Dag.mk_funboolnot
      (res, res')
      (A.Dag.mk_funnumeq
	 (res', (c, c'))
	 algexpr)

(* wrapper for greater than *)
let wrap_gt res c c' algexpr =
  A.Dag.mk_funnumgt
    (res, (c, c'))
    algexpr

(* wrapper for less than *)
let wrap_lt rescol c c' algexpr = wrap_gt rescol c' c algexpr

(* wrapper for not *)
let wrap_not res op_attr algexpr =
  A.Dag.mk_funboolnot
    (res, op_attr)
    algexpr

(* wrapper for aggregate operators *)
let wrap_agg loop q attachment =
  A.Dag.mk_attach
    (pos, A.Nat 1n)
    (A.Dag.mk_disjunion
       q
       (A.Dag.mk_attach
	  (A.Item 1, attachment)
	  (A.Dag.mk_difference
	     loop
	     (A.Dag.mk_project
		[prj iter]
		q))))

(* generate the algebra code for the application of a binary operator to two
   columns. the table must not represent lists, i.e. pos must be 1 for all iters *)
let do_primitive_binop wrapper op1 op2 =
  let c = A.Item 1 in
  let c' = A.Item 2 in
  let res = A.Item 3 in
    A.Dag.mk_project
      [(prj iter); (prj pos); (c, res)]
      (wrapper 
	 res c c'
	 (A.Dag.mk_eqjoin
	    (iter, iter')
	    op1
	    (A.Dag.mk_project
	       [(iter', iter); (c', c)]
	       op2)))

let do_primitive_binop_ti wrapper restype e1 e2 =
  let Ti (q_e1, cs_e1, _, _) = e1 in
  let Ti (q_e2, cs_e2, _, _) = e2 in
    assert (Cs.is_operand cs_e1);
    assert (Cs.is_operand cs_e2);
    let q = do_primitive_binop wrapper q_e1 q_e2 in
      Ti (q, [`Offset (1, restype)], Itbls.empty, dummy)

let smaller = do_primitive_binop_ti wrap_lt `BoolType
let greater = do_primitive_binop_ti wrap_gt `BoolType
let equal = do_primitive_binop_ti wrap_eq `BoolType
let or_op = do_primitive_binop_ti wrap_or `BoolType
let and_op = do_primitive_binop_ti wrap_and `BoolType

let do_unbox q_e surr_col inner_ti =
  Debug.print "do_unbox";
  let Ti(q_sub, cs_sub, itbls_sub, _) = inner_ti in
  let q_unbox =
    A.Dag.mk_project
      ([(iter, iter'); prj pos] @ (prjlist (io (Cs.columns cs_sub))))
      (A.Dag.mk_eqjoin
	 (c', iter)
	 (A.Dag.mk_project
	    [(iter', iter); (c', A.Item surr_col)]
	    q_e)
	 q_sub)
  in
    Ti(q_unbox, cs_sub, itbls_sub, dummy)

let do_project field record =
  let Ti (q_r, cs_r, itbls_r, _) = record in
  let field_cs' = Cs.lookup_record_field cs_r field in
  let old_cols = Cs.columns field_cs' in
  Debug.print (Cs.show cs_r);
  let offset = List.hd old_cols in
  let new_cols = incr old_cols (-offset + 1) in
  let field_cs = Cs.shift field_cs' (-offset + 1) in
  let field_itbls = Itbls.decr_keys (Itbls.retain_by_keys itbls_r old_cols) (offset - 1) in 
  let q =
    A.Dag.mk_project
      ([prj iter; prj pos] @ prjlist_map (io new_cols) (io old_cols))
      q_r
  in
    Ti (q, field_cs, field_itbls, dummy)

let do_length loop (Ti (q_e, _, _, _)) =
  let q = 
    A.Dag.mk_funaggrcount
      (A.Item 1, Some iter)
      q_e
  in
  let q' = wrap_agg loop q (A.Int (Num.Int 0)) in
    Ti (q', [`Offset (1, `IntType)], Itbls.empty, dummy)

(* q_e1 and q_e2 must have absolute positions *)
let do_zip e1 e2 =
  Debug.print "do_zip";
  let Ti (q_e1, cs_e1, itbls_e1, _) = e1 in
  let Ti (q_e2, cs_e2, itbls_e2, _) = e2 in
  let card_e1 = List.length (Cs.columns cs_e1) in
  let cs_e2' = Cs.shift cs_e2 card_e1 in
  let itbls_e2' = Itbls.incr_keys itbls_e2 card_e1 in
  let items = io ((Cs.columns cs_e1) @ (Cs.columns cs_e2')) in
  let q =
    A.Dag.mk_project
      ([prj iter; prj pos] @ (prjlist items))
      (A.Dag.mk_select
	 res
	 (A.Dag.mk_funnumeq
	    (res, (pos, pos'))
	    (A.Dag.mk_eqjoin
	       (iter, iter')
	       q_e1
	       (A.Dag.mk_project
		  ([(iter', iter); (pos', pos)] @ (prjlist_map (io (Cs.columns cs_e2')) (io (Cs.columns cs_e2))))
		  q_e2))))
  in
  let cs = [`Mapping ("1", cs_e1); `Mapping ("2", cs_e2')] in
    Ti (q, cs, (Itbls.append itbls_e1 itbls_e2'), dummy)

(* apply the all aggregate operator to the first column grouped by iter 
   (corresponds to the function "and" from the links prelude *)
let do_list_and loop (Ti (q, cs, _, _)) =
  assert (Cs.is_operand cs);
  let q' = 
    (A.Dag.mk_funaggr
       (A.All, (A.Item 1, A.Item 1), Some iter)
       q)
  in
  let q'' = wrap_agg loop q' (A.Bool true) in
    Ti (q'', [`Offset (1, `BoolType)], Itbls.empty, dummy)

(* apply the min aggregate operator to the first column grouped by iter 
   (corresponds to the function "or" from the links prelude *)
let do_list_or loop (Ti (q, cs, _, _)) =
  assert (Cs.is_operand cs);
  let q' =
    A.Dag.mk_funaggr
      (A.Max, (A.Item 1, A.Item 1), Some iter)
      q
  in
  let q'' = wrap_agg loop q' (A.Bool false) in
    Ti (q'', [`Offset (1, `BoolType)], Itbls.empty, dummy)

let rec suap q_paap it1 it2 : (int * tblinfo) list =
  match (it1, it2) with
    | (c1, Ti (q_1, cs1, subs_1, _)) :: subs_hat, ((_, Ti (q_2, cs2, subs_2, _)) :: subs_tilde) ->
	let q =
	  (A.Dag.mk_rownum 
	     (item', [(iter, A.Ascending); (ord, A.Ascending); (pos, A.Ascending)], None)
	     (A.Dag.mk_disjunion
		(A.Dag.mk_attach
		   (ord, A.Nat 1n)
		   q_1)
		(A.Dag.mk_attach
		   (ord, A.Nat 2n)
		   q_2)))
	in
	let q'_projlist = [(iter, item''); prj pos] in
	let q'_projlist = q'_projlist @ (prjlist (io (difference (Cs.columns cs1) (Itbls.keys subs_1)))) in
	let q'_projlist = q'_projlist @ (prjlist_single (io (Itbls.keys subs_1)) item') in
	let q' =
	  (A.Dag.mk_project
	     q'_projlist
	     (A.Dag.mk_thetajoin
		[(A.Eq, (ord, ord')); (A.Eq, (iter, c'))]
		q
		(A.Dag.mk_project
		   [(ord', ord); (item'', item'); (c', A.Item c1)]
		   q_paap)))
	in
	  [(c1, (Ti (q', (Cs.fuse cs1 cs2), (suap q subs_1 subs_2), dummy)))] @ (suap q_paap subs_hat subs_tilde)
    | [], [] -> []
	(* If one of the inner lists to be appended is empty, 
	   we still need to generate new surrogate keys so 
	   that they match with the ones computed in the outer table *)
    | [], (c, Ti(q_i, cs, subs, _)) :: _ -> 
	let q =
	  (A.Dag.mk_rownum 
	     (item', [(iter, A.Ascending); (ord, A.Ascending); (pos, A.Ascending)], None)
		(A.Dag.mk_attach
		   (ord, A.Nat 1n)
		   q_i))
	in
	let q'_projlist = [(iter, item''); prj pos] in
	let q'_projlist = q'_projlist @ (prjlist (io (difference (Cs.columns cs) (Itbls.keys subs)))) in
	let q'_projlist = q'_projlist @ (prjlist_single (io (Itbls.keys subs)) item') in
	let q' =
	  (A.Dag.mk_project
	     q'_projlist
	     (A.Dag.mk_thetajoin
		[(A.Eq, (ord, ord')); (A.Eq, (iter, c'))]
		q
		(A.Dag.mk_project
		   [(ord', ord); (item'', item'); (c', A.Item c)]
		   q_paap)))
	in
	  [c, (Ti (q', cs, (suap q subs[]), dummy))] 
    | (c, Ti(q_i, cs, subs, _)) :: _, [] -> 
	let q =
	  (A.Dag.mk_rownum 
	     (item', [(iter, A.Ascending); (ord, A.Ascending); (pos, A.Ascending)], None)
		(A.Dag.mk_attach
		   (ord, A.Nat 1n)
		   q_i))
	in
	let q'_projlist = [(iter, item''); prj pos] in
	let q'_projlist = q'_projlist @ (prjlist (io (difference (Cs.columns cs) (Itbls.keys subs)))) in
	let q'_projlist = q'_projlist @ (prjlist_single (io (Itbls.keys subs)) item') in
	let q' =
	  (A.Dag.mk_project
	     q'_projlist
	     (A.Dag.mk_thetajoin
		[(A.Eq, (ord, ord')); (A.Eq, (iter, c'))]
		q
		(A.Dag.mk_project
		   [(ord', ord); (item'', item'); (c', A.Item c)]
		   q_paap)))
	in
	  [c, (Ti (q', cs, (suap q subs []), dummy))] 

let rec suse q_pase subs : ((int * tblinfo) list) =
  if Settings.get_value Basicsettings.Ferry.slice_inner_tables then
    match subs with
      | (offset, (Ti(q, cs, itbls, _))) :: subs ->
	  let q' = 
	    A.Dag.mk_project
	      ([prj iter; prj pos] @ (prjlist (io (Cs.columns cs))))
	      (A.Dag.mk_eqjoin
		 (iter, iter')
		 q
		 (A.Dag.mk_project
		    [(iter', A.Item offset)]
		    q_pase))
	  in
	    [(offset, (Ti(q', cs, (suse q' itbls), dummy)))] @ (suse q_pase subs)
      | [] ->
	  []
  else
    subs

(* the empty list *)
let nil = A.Dag.mk_emptytbl [(A.Iter 0, `NatType); (A.Pos 0, `NatType); (A.Item 1, `IntType)]

(* loop-lift q by map *)
let lift map (Ti (q, cs, _, _)) =
  let q' =
    (A.Dag.mk_project
       ([(iter, inner); prj pos] @ (prjlist (io (Cs.columns cs))))
       (A.Dag.mk_eqjoin
	  (iter, outer)
	  q
	  map))
  in
    Ti (q', cs, Itbls.empty, dummy)

(* construct the ordering map of a for-loop *)
let rec omap map sort_criteria sort_cols =
  match (sort_criteria, sort_cols) with
    | (o :: os), (col :: cols) ->
	A.Dag.mk_project
	  ([prj outer; prj inner; (col, A.Item 1)] @ (prjlist cols))
	  (A.Dag.mk_eqjoin
	     (inner, iter)
	     (omap map os cols)
	     o)
    | [], [] ->
	map
    | _ -> assert false

(* compute absolute positions for q *)
let abspos q cols =
  A.Dag.mk_project
    ([prj iter; prj pos] @ (prjlist cols))
    (A.Dag.mk_rownum
       (pos, [(pos', A.Ascending)], Some iter)
       (A.Dag.mk_project
	  ([prj iter; (pos', pos)] @ (prjlist cols))
	  q))

(* compute absolute positions for a tblinfo *)
let abspos_ti (Ti (q, cs, itbls, _)) =
  Ti ((abspos q (io (Cs.columns cs))), cs, itbls, dummy)

let rec compile_box env loop e =
  let ti_e = compile_expression env loop e in
  let q_o = 
    A.Dag.mk_attach
      (pos, A.Nat 1n)
      (A.Dag.mk_project 
	 [(prj iter); (A.Item 1, iter)]
	 loop)
  in
    Ti(q_o, [`Offset (1, `Surrogate)], [(1, ti_e)], dummy)

and compile_unbox env loop e =
  let Ti (q_e, cs_e, itbls_e, _) = compile_expression env loop e in
    assert ((Cs.cardinality cs_e) = 1);
    assert ((List.length itbls_e) = 1);
    let (offset, inner_ti) = List.hd itbls_e in
      do_unbox q_e offset inner_ti

and compile_append env loop l =
  match l with
    | e :: [] ->
	compile_expression env loop e
    | hd_e :: tl_e ->
	let hd = compile_expression env loop hd_e in
	let tl = compile_append env loop tl_e in
	  compile_list hd tl
    | [] ->
	Ti (nil, [`Offset (1, `NatType)], Itbls.empty, dummy)

and compile_list (Ti (hd_q, hd_cs, hd_itbls, _)) (Ti (tl_q, tl_cs, tl_itbls, _)) =
  let fused_cs = Cs.fuse hd_cs tl_cs in
  let q =
    A.Dag.mk_rownum
      (item', [(iter, A.Ascending); (ord, A.Ascending); (pos, A.Ascending)], None)
      (A.Dag.mk_rank
	 (pos', [(ord, A.Ascending); (pos, A.Ascending)])
	 (A.Dag.mk_disjunion
	    (A.Dag.mk_attach
	       (ord, A.Nat 1n)
	       hd_q)
	    (A.Dag.mk_attach
	       (ord, A.Nat 2n)
	       tl_q)))
  in
  let q'_projlist = [prj iter; (pos, pos')] in
  let q'_projlist = q'_projlist @ (prjlist (io (difference (Cs.columns hd_cs) (Itbls.keys hd_itbls)))) in
  let q'_projlist = q'_projlist @ (prjlist_single (io (Itbls.keys hd_itbls)) item') in
  let q' = 
    A.Dag.mk_project
      q'_projlist
      q
  in
  let itbls' = suap q hd_itbls tl_itbls in
    Ti (q', fused_cs, itbls', dummy)

and compile_zip env loop args =
  assert ((List.length args) = 2);
  let Ti (q_e1, cs_e1, itbls_e1, _) = compile_expression env loop (List.hd args) in
  let Ti (q_e2, cs_e2, itbls_e2, _) = compile_expression env loop (List.nth args 1) in
  let q_e1' = abspos q_e1 (io (Cs.columns cs_e1)) in
  let q_e2' = abspos q_e2 (io (Cs.columns cs_e2)) in
    do_zip (Ti (q_e1', cs_e1, itbls_e1, dummy)) (Ti (q_e2', cs_e2, itbls_e2, dummy))

and compile_unzip env loop args =
  assert((List.length args) = 1);
  let Ti (q_e, cs_e, itbls_e, _) = compile_expression env loop (List.hd args) in
    Debug.print (Cs.show cs_e);
  let q = 
    A.Dag.mk_project
      ([prj iter; prj pos] @ (prjlist_single [A.Item 1; A.Item 2] iter))
      (A.Dag.mk_attach
	 (pos, A.Nat 1n)
	 loop)
  in
  let cs_1 = Cs.lookup_record_field cs_e "1" in
  let cs_2 = Cs.lookup_record_field cs_e "2" in
  let cols_1 = Cs.columns cs_1 in
  let card = List.length (Cs.columns cs_1) in
  let cols_2 = Cs.columns cs_2 in
  let cs_2' = Cs.shift cs_2 (-card) in
  let card = List.length cols_1 in
  let q_1 = 
    A.Dag.mk_project
      ([prj iter; prj pos] @ (prjlist (io cols_1)))
      q_e
  in
  let q_2 =
    A.Dag.mk_project
      (let proj = prjlist_map (io (decr cols_2 card)) (io cols_2) in
	 ([prj iter; prj pos] @ proj))
      q_e
  in
  let itbls_1 = Itbls.retain_by_keys itbls_e cols_1 in
  let itbls_2 = Itbls.decr_keys (Itbls.retain_by_keys itbls_e cols_2) card in
  let itbls = [(1, Ti(q_1, cs_1, itbls_1, dummy)); (2, Ti(q_2, cs_2', itbls_2, dummy))] in
  let cs = [`Mapping ("1", [`Offset (1, `Surrogate)]); `Mapping ("2", [`Offset (2, `Surrogate)])] in
    Ti (q, cs, itbls, dummy)

(* FIXME: unite at least compile_or/and/length *)
and compile_or env loop args =
  assert ((List.length args) = 1);
  let e = List.hd args in
  let ti_e = compile_expression env loop e in
    do_list_or loop ti_e

and compile_and env loop args =
  assert ((List.length args) = 1);
  let e = List.hd args in
  let ti_e = compile_expression env loop e in
    do_list_and loop ti_e

and compile_length env loop args =
  assert ((List.length args) = 1);
  let e = List.hd args in
  let ti_e = compile_expression env loop e in
    do_length loop ti_e

and compile_empty env loop args = 
  assert ((List.length args) = 1);
  let e = List.hd args in
  let ti_e = compile_expression env loop e in
  let Ti (q_length, cs_length, _, _) = do_length loop ti_e in
    assert (Cs.is_operand cs_length);
    let q =
      A.Dag.mk_project
	[prj iter; prj pos; (A.Item 1, res)]
	(A.Dag.mk_funnumeq
	   (res, (A.Item 1, A.Item 2))
	   (A.Dag.mk_attach
	      (A.Item 2, A.Int (Num.Int 0))
	      q_length))
    in
      Ti (q, [`Offset (1, `BoolType)], Itbls.empty, dummy)

(* FIXME: only sum works at the moment. max/min/avg can't be used.
   Issues:
   * infer the correct column type for the result (or give it as a parameter)
   * how to handle empty lists? *)
and compile_aggr env loop aggr_fun args =
  assert ((List.length args) = 1);
  let c = A.Item 1 in
  let e = List.hd args in
  let Ti (q_e, cs_e, _, _) = compile_expression env loop e in
    assert (Cs.is_operand cs_e);
    let q = 
      (A.Dag.mk_funaggr
	 (aggr_fun, (c, c), Some iter)
	 q_e)
    in
      (* HACK: special case for sum *)
    let q' = wrap_agg loop q (A.Int (Num.Int 0)) in
      (* FIXME: extract the correct column type from cs_e *)
      Ti (q', [`Offset (1, `IntType)], Itbls.empty, dummy)

and compile_nth env loop operands =
  assert ((List.length operands) = 2);
  let Ti (q1, _, _, _) = compile_expression env loop (List.hd operands) in
  let Ti (q2, cs2, itbls_2, _) = compile_expression env loop (List.nth operands 1) in
  let q2' = abspos q2 (io (Cs.columns cs2)) in
  let q =
    (A.Dag.mk_project
       ([prj iter; prj pos] @ prjlist (io (Cs.columns cs2)))
       (A.Dag.mk_select
	  res
	  (A.Dag.mk_funnumeq
	     (res, (pos', c'))
	     (A.Dag.mk_eqjoin
		(iter, iter')
		(A.Dag.mk_cast
		   (pos', pos, `IntType)
		   q2')
		(A.Dag.mk_project
		   [(iter', iter); (c', A.Item 1)]
		   q1)))))
  in
  let itbls' = suse q itbls_2 in
    Ti (q, cs2, itbls', dummy)

and compile_comparison env loop comparison_wrapper tablefun rowfun operands =
  assert ((List.length operands) = 2);
  let e1 = List.hd operands in
  let e2 = List.nth operands 1 in
  let e1_ti = compile_expression env loop e1 in
  let e2_ti = compile_expression env loop e2 in
  let is_boxed (Ti (_, cs, _, _)) =
    match cs with
      | [`Offset (1, `Surrogate)] -> true
      | _ -> false
  in
  let unbox (Ti (q, cs, itbls, _)) =
      assert (Cs.is_operand cs);
      assert ((Itbls.length itbls) = 1);
      let (offset, inner_ti) = List.hd itbls in
	do_unbox q offset inner_ti
  in
    match (Query2.Annotate.typeof_typed_t e1, Query2.Annotate.typeof_typed_t e2) with
	(* if arguments are boxed (i.e. they have list type), we need
	   to unbox them first *)
      | `Atom, `Atom when (is_boxed e1_ti) && (is_boxed e2_ti) ->
	  tablefun loop comparison_wrapper (unbox e1_ti) (unbox e2_ti)
      | `Atom, `List when is_boxed e1_ti ->
	  tablefun loop comparison_wrapper (unbox e1_ti) e2_ti
      | `List, `Atom when is_boxed e2_ti ->
	  tablefun loop comparison_wrapper e1_ti (unbox e2_ti)
      | `Atom, `Atom -> 
	  rowfun loop comparison_wrapper e1_ti e2_ti
      | `List, `List -> 
	  tablefun loop comparison_wrapper e1_ti e2_ti
      | _ -> assert false

(* ">"-operator on lists. we are using the definition of "<" (lexicographic ordering) to
   implement ">" so we need to switch the operands *)
and do_table_greater loop wrapper l1 l2 =
  
  (* switch the components of the zipped pairs, i.e. zip(a, b) -> zip(b, a) *)
  let switch_zipped ti =
    let Ti (q, cs, itbls, _) = ti in
    let cs1 = Cs.lookup_record_field cs "1" in
    let cs2 = Cs.lookup_record_field cs "2" in
    let cs' = [`Mapping ("1", cs2); `Mapping ("2", cs1)] in
      Ti (q, cs', itbls, dummy)
  in

  (* returns the minimal pos so that l1[pos] < l2[pos] *)
  let minpos zipped = 
    (* the comparison must be done loop-lifted so that inner tables can be unboxed and compared correctly *)

    (* lift zipped *)
    let Ti (q_s, cs_s, itbls_s, _) = zipped in
    let q_s' = 
      A.Dag.mk_rownum
	(inner, [(iter, A.Ascending); (pos, A.Ascending)], None)
	q_s
    in
    let q_s_mapped = 
      A.Dag.mk_attach
	(pos, A.Nat 1n)
	(A.Dag.mk_project
	   ((iter, inner) :: (prjlist (io (Cs.columns cs_s))))
	   q_s')
    in
    let map =
      A.Dag.mk_project
	[(outer, iter); prj inner; (pos', pos)]
	q_s'
    in
    let loop' =
      A.Dag.mk_project
	[prj iter]
	q_s_mapped
    in
    
    let zipped_mapped = Ti (q_s_mapped, cs_s, itbls_s, dummy) in

    (* we need "<" on rows but have only ">" -> switch arguments *)
    let compared = do_row_greater_real loop' wrapper (switch_zipped zipped_mapped) in

    (* unlift *)
    let compared_backmapped =
      A.Dag.mk_project
	[(iter, outer); (pos, pos'); prj (A.Item 1)]
	(A.Dag.mk_eqjoin
	   (iter, inner)
	   compared
	   map)
    in

    let selected = A.Dag.mk_select (A.Item 1) compared_backmapped in

    let q = 
      A.Dag.mk_disjunion
	(A.Dag.mk_attach
	   (pos, A.Nat 1n)
	   (A.Dag.mk_project
	      [prj iter; (A.Item 1, res)]
	      (A.Dag.mk_funaggr
		 (A.Min, (res, pos), Some iter)
		 selected)))
	(A.Dag.mk_attach
	   (pos, A.Nat 1n)
	   (A.Dag.mk_attach
	      (A.Item 1, A.Nat Nativeint.max_int)
	      (A.Dag.mk_difference
		 loop
		 (A.Dag.mk_project
		    [prj iter]
		    selected))))
    in
      Ti (q, [`Offset (1, `NatType)], Itbls.empty, dummy)
  in

  let abspos_ti (Ti (q, cs, itbls, _)) =
    Ti ((abspos q (io (Cs.columns cs))), cs, itbls, dummy)
  in

  (* l1 > l2 iff l2 < l1 -> swap arguments *)
  let (l1, l2) = (l2, l1) in
  let l1_abs = abspos_ti l1 in
  let l2_abs = abspos_ti l2 in
  let zipped = do_zip l1_abs l2_abs in
  let zipped_reverse = switch_zipped zipped in 
  let l1_len = do_length loop l1_abs in
  let l2_len = do_length loop l2_abs in
  let minp_l1_l2 = minpos zipped in
  let minp_l2_l1 = minpos zipped_reverse in

    or_op
      (and_op
	 (smaller l1_len l2_len)
	 (equal minp_l1_l2 minp_l2_l1))
      (smaller minp_l1_l2 minp_l2_l1)
    
and do_row_greater loop wrapper e1 e2 = 
  let q = do_row_greater_real loop wrapper (do_zip e1 e2) in
    Ti (q, [`Offset (1, `BoolType)], Itbls.empty, dummy)

and do_row_greater_real loop wrapper zipped =

  let column_greater ti_zipped ((col_l, type_l), (col_r, _type_r)) =
    let Ti (q_zipped, _cs_zipped, itbls_zipped, _) = ti_zipped in
    let q = 
      if is_primitive_col type_l then
	A.Dag.mk_project
	  [prj iter; prj pos; (A.Item 1, res)]
	  (* no need to join since the two arguments are already zipped *)
	  (wrap_gt res (A.Item col_l) (A.Item col_r) q_zipped)
      else
	(* inner tables need to be unboxed first *)
	let inner_table_l, inner_table_r =
	  try
	    Itbls.lookup col_l itbls_zipped, Itbls.lookup col_r itbls_zipped
	  with _ -> assert false
	in
	let ti_unboxed_l = do_unbox q_zipped col_l inner_table_l in
	let ti_unboxed_r = do_unbox q_zipped col_r inner_table_r in
	  q_of_tblinfo (do_table_greater loop wrapper ti_unboxed_l ti_unboxed_r)
	    
    in
      Ti (q, [`Offset (1, `BoolType)], Itbls.empty, dummy)
  in

  let column_equal ti_zipped ((col_l, type_l), (col_r, _type_r)) = 
    let Ti (q_zipped, _cs_zipped, itbls_zipped, _) = ti_zipped in
    let q = 
      if is_primitive_col type_l then
	A.Dag.mk_project
	  [prj iter; prj pos; (A.Item 1, res)]
	  (* no need to join since the two arguments are already zipped *)
	  (wrap_eq res (A.Item col_l) (A.Item col_r) q_zipped)
      else
	(* we compare nested lists represented by a inner table *)

	(* lookup the inner tables referred to by col1, col2 *)
	let inner_table_l, inner_table_r = 
	  try
	    Itbls.lookup col_l itbls_zipped, Itbls.lookup col_r itbls_zipped 
	  with _ -> assert false
	in
	  (* unbox the inner tables *)
	let ti_unboxed_l = do_unbox q_zipped col_l inner_table_l in
	let ti_unboxed_r = do_unbox q_zipped col_r inner_table_r in
	  (* compare the inner tables *)
	  q_of_tblinfo (do_table_equal loop wrapper ti_unboxed_l ti_unboxed_r) 
    in
      Ti (q, [`Offset (1, `BoolType)], Itbls.empty, dummy)
  in

  let Ti (_q_zipped, cs_zipped, _itbls_zipped, _) = zipped in
  let cs_l = Cs.lookup_record_field cs_zipped "1" in
  let cs_r = Cs.lookup_record_field cs_zipped "2" in

  (* special case: if we are comparing lists of records and one of the lists is the empty 
     list, the length of its cs component does not match the other cs's length.  in this case, 
     we need to "fake" a compatible cs for the empty list *)
  let cs_l, cs_r = Cs.longer_cs cs_l cs_r in
  
  (* sort record fields by field name so that the correct columns are
     compared *)
  let cols_l = Cs.leafs (Cs.sort_record_columns cs_l) in
  let cols_r = Cs.leafs (Cs.sort_record_columns cs_r) in

  let n = List.length cols_l in
    assert (n = List.length cols_r);
    
    let corresponding_columns = List.combine cols_l cols_r in

    (* l_1 = r_1 ... l_n = r_n *)
    let greater_terms = List.map (column_greater zipped) corresponding_columns in

    (* l_1 > r_1, ..., l_n-1 > r_n-1 *)
    let equal_terms = List.map (column_equal zipped) (take (n - 1) corresponding_columns) in

    (* l_1 = r_1, ..., l_1 = r_1 && ... && l_n-1 = r_n-1 *)
    let combined =
      List.fold_left
	(fun combined eq_k -> (and_op (List.hd combined) eq_k) :: combined)
	(take 1 equal_terms)
	(drop 1 equal_terms)
    in
    let combined = List.rev combined in

    (* l_1 = r_1 && l_2 > r_2, ..., l_1 = r_1 && l_2 = r_2 && l_2 > r_3, l_1 = r_1 && ... && l_n-1 = r_n-1 && l_n > r_n *)
    let and_terms = List.map2 and_op (drop 1 greater_terms) combined in

    (* l_1 > r_1 || (l_1 = r_1 && l_2 > r_2) || ... *)
    let Ti (q, _, _, _) = List.fold_left or_op (List.hd greater_terms) and_terms in
      q
    
and do_table_equal loop wrapper l1 l2 =
  let all = do_list_and loop in

  let map_equal source =
    let Ti (q_s, cs_s, itbls_s, _) = source in
     let q_s' = 
       A.Dag.mk_rownum
	 (inner, [(iter, A.Ascending); (pos, A.Ascending)], None)
	 q_s
     in
     let q_s_mapped = 
       A.Dag.mk_attach
	 (pos, A.Nat 1n)
	 (A.Dag.mk_project
	    ((iter, inner) :: (prjlist (io (Cs.columns cs_s))))
	    q_s')
     in
     let map =
       A.Dag.mk_project
	 [(outer, iter); prj inner; (pos', pos)]
	 q_s'
     in
     let loop =
       A.Dag.mk_project
	 [prj iter]
	 q_s_mapped
     in
     let ti_s = Ti (q_s_mapped, cs_s, itbls_s, dummy) in
     let Ti (q_equal, _, _, _) = (do_row_equal loop wrapper (do_project "1" ti_s) (do_project "2" ti_s)) in
     (* map the comparison result back into the outer iteration context *)
     let result_backmapped =
       A.Dag.mk_project
	 [(iter, outer); (pos, pos'); prj (A.Item 1)]
	 (A.Dag.mk_eqjoin
	    (iter, inner)
	    q_equal
	    map)
     in
       Ti (result_backmapped, [`Offset (1, `BoolType)], Itbls.empty, dummy)
  in

	
  let l1_abs = abspos_ti l1 in
  let l2_abs = abspos_ti l2 in
  let l1_len = do_length loop l1_abs in
  let l2_len = do_length loop l2_abs in
    and_op 
      (equal l1_len l2_len)
      (all (map_equal (do_zip l1_abs l2_abs)))

and do_row_equal loop wrapper r1 r2 =
  Debug.print "do_row_equal";
  let Ti (q_r1, cs_r1, itbls_r1, _) = r1 in
  let Ti (q_r2, cs_r2, itbls_r2, _) = r2 in

  (* special case: if we are comparing lists of records and one of the lists is the empty 
     list, the length of its cs component does not match the other cs's length.  in this case, 
     we need to "fake" a compatible cs for the empty list *)
  let cs_r1, cs_r2 = Cs.longer_cs cs_r1 cs_r2 in

  (* pair the item columns which belong to the respective record fields *)
  let items1 = Cs.leafs (Cs.sort_record_columns cs_r1) in
  let items2 = Cs.leafs (Cs.sort_record_columns cs_r2) in

  let items = List.combine items1 items2 in

  let c = A.Item 1 in
  let c' = A.Item 2 in
  let res = A.Item 3 in
  (* compare the columns for the first field. the result is then the conjuntion of
     this result and the result of the (recursive) comparison of the remaining fields *)
  let rec assemble_equals = function
    | [] -> 
	failwith "do_row_equal: empty records"
    | [((col1, coltype), (col2, _))] ->
	let q_r1', q_r2' =
	  (* no need to project if the row has only one item column *)
	  if (Cs.cardinality cs_r1) = 1 then
	    (q_r1, q_r2)
	  else
	    ((A.Dag.mk_project
		[prj iter; prj pos; (c, A.Item col1)]
		q_r1),
	     (A.Dag.mk_project
		[prj iter; prj pos; (c, A.Item col2)]
		q_r2))
	in
	  if is_primitive_col coltype then
	    (* normal comparison of atomic values *)
	    do_primitive_binop wrapper q_r1' q_r2'
	  else
	    (* we compare nested lists represented by a inner table *)

	    (* lookup the inner tables referred to by col1, col2 *)
	    let inner_table_r1, inner_table_r2 = 
	      try
		Itbls.lookup col1 itbls_r1, Itbls.lookup col2 itbls_r2 
	      with _ -> assert false
	    in
	    (* unbox the inner tables *)
	    let ti_unboxed_r1 = do_unbox q_r1' col1 inner_table_r1 in
	    let ti_unboxed_r2 = do_unbox q_r2' col2 inner_table_r2 in
	      (* compare the inner tables *)
	      q_of_tblinfo (do_table_equal loop wrapper ti_unboxed_r1 ti_unboxed_r2) 
    | ((col1, coltype), (col2, _)) :: items ->
	let col_equal_result = 
	  let q_r1', q_r2' =
	       (A.Dag.mk_project
		  [prj iter; prj pos; (c, A.Item col1)]
		  q_r1),
	       (A.Dag.mk_project
		  [prj iter; prj pos; (c, A.Item col2)]
		  q_r2)
	  in
	    if is_primitive_col coltype then
	      do_primitive_binop wrapper q_r1' q_r2'
	    else
	      (* we compare nested lists represented by a inner table *)

	      (* lookup the inner tables referred to by col1, col2 *)
	      let inner_table_r1, inner_table_r2 = 
		try
		  Itbls.lookup col1 itbls_r1, Itbls.lookup col2 itbls_r2 
		with _ -> assert false
	      in
		(* unbox the inner tables *)
	      let ti_unboxed_r1 = do_unbox q_r1' col1 inner_table_r1 in
	      let ti_unboxed_r2 = do_unbox q_r2' col2 inner_table_r2 in
		(* compare the inner tables *)
	      let result_ti = do_table_equal loop wrapper ti_unboxed_r1 ti_unboxed_r2 in
	      let Ti (q_result, _, _, _) = result_ti in
		q_result
	in
	  A.Dag.mk_project
	    [prj iter; prj pos; (c, res)]
	    (A.Dag.mk_funbooland
	       (res, (c, c'))
	       (A.Dag.mk_eqjoin
		  (iter', iter)
		  (A.Dag.mk_project
		     [(iter', iter); (c', c)]
		     col_equal_result)
		  (assemble_equals items)))
  in
  let q = assemble_equals items in
    Ti(q, [`Offset (1, `BoolType)], Itbls.empty, dummy)

and compile_binop env loop wrapper restype operands =
  assert ((List.length operands) = 2);
  let ti_1 = compile_expression env loop (List.hd operands) in
  let ti_2 = compile_expression env loop (List.nth operands 1) in
    do_primitive_binop_ti wrapper restype ti_1 ti_2

and compile_unop env loop wrapper operands =
  assert ((List.length operands) = 1);
  let Ti (op_q, op_cs, _, _) = compile_expression env loop (List.hd operands) in
    assert (Cs.is_operand op_cs);
    let c = A.Item 1 in
    let res = A.Item 2 in
    let q = 
      A.Dag.mk_project
	[prj iter; prj pos; (c, res)]
	(wrapper
	   res c
	   op_q)
    in
      Ti (q, op_cs, Itbls.empty, dummy)

and compile_concat env loop args =
  assert ((List.length args) = 1);
  let Ti (q_e, _, itbls_e, _) = compile_expression env loop (List.hd args) in
    assert((List.length itbls_e) = 1);
    let Ti(q_sub, cs_sub, itbls_sub, _) = Itbls.lookup 1 itbls_e in
    let c = A.Item 1 in
    let q =
      A.Dag.mk_project
	([(iter, iter'); (pos, pos'')] @ (prjlist (io (Cs.columns cs_sub))))
	(A.Dag.mk_rank
	   (pos'', [(pos', A.Ascending); (pos, A.Ascending)])
	   (A.Dag.mk_eqjoin
	      (c', iter)
	      (A.Dag.mk_project
		 [(iter', iter); (pos', pos); (c', c)]
		 q_e)
	      q_sub))
    in
      Ti(q, cs_sub, itbls_sub, dummy)

and compile_take env loop args =
  assert ((List.length args) = 2);
  let Ti(q1, _cs1, _, _) = compile_expression env loop (List.hd args) in
  let Ti(q2, cs2, itbls2, _) = compile_expression env loop (List.nth args 1) in
  let cols = (io (Cs.columns cs2)) in
  let q2' = abspos q2 cols in
  let c = A.Item 1 in
  let one = A.Item 2 in
  let q' = 
    A.Dag.mk_project
      ([prj iter; prj pos] @ (prjlist cols))
      (A.Dag.mk_select
	 res
	 (A.Dag.mk_funnumgt
	    (res, (c', pos'))
	    (A.Dag.mk_eqjoin
	       (iter, iter')
	       (A.Dag.mk_cast
		  (pos', pos, `IntType)
		  q2')
	       (A.Dag.mk_project
		  [(iter', iter); (c', res)]
		  (A.Dag.mk_fun1to1
		     (A.Add, res, [c; one])
		     (A.Dag.mk_attach
			(one, A.Int (Num.Int 1))
			q1))))))
       in
  let itbls' = suse q' itbls2 in
    Ti(q', cs2, itbls', dummy)

and compile_drop env loop args =
  assert ((List.length args) = 2);
  let Ti(q1, _cs1, _, _) = compile_expression env loop (List.hd args) in
  let Ti(q2, cs2, itbls2, _) = compile_expression env loop (List.nth args 1) in
  let cols = (io (Cs.columns cs2)) in
  let q2' = abspos q2 cols in
  let c = A.Item 1 in
  let q' =
    A.Dag.mk_project
      ([prj iter; prj pos] @ (prjlist cols))
      (A.Dag.mk_select
	 res
	 (A.Dag.mk_funnumgt
	    (res, (pos', c'))
	    (A.Dag.mk_eqjoin
	       (iter, iter')
	       (A.Dag.mk_cast
		  (pos', pos, `IntType)
		  q2')
	       (A.Dag.mk_project
		  [(iter', iter); (c', c)]
		  q1))))
  in
  let itbls' = suse q' itbls2 in
    Ti(q', cs2, itbls', dummy)

and compile_apply env loop f args =
  match f with
    | "+" -> compile_binop env loop (wrap_1to1 A.Add) `IntType args
    | "+." -> compile_binop env loop (wrap_1to1 A.Add) `FloatType args
    | "-" -> compile_binop env loop (wrap_1to1 A.Subtract) `IntType args
    | "-." -> compile_binop env loop (wrap_1to1 A.Subtract) `FloatType args
    | "*" -> compile_binop env loop (wrap_1to1 A.Multiply) `IntType args
    | "*." -> compile_binop env loop (wrap_1to1 A.Multiply) `FloatType args
    | "/" -> compile_binop env loop (wrap_1to1 A.Divide) `IntType args
    | "/." -> compile_binop env loop (wrap_1to1 A.Divide) `FloatType args
    | "==" -> compile_comparison env loop wrap_eq do_table_equal do_row_equal args
    | "<>" -> compile_comparison env loop wrap_ne do_table_equal do_row_equal args
    | ">" -> compile_comparison env loop wrap_gt do_table_greater do_row_greater args
    | "not" ->  compile_unop env loop wrap_not args
    | "nth" -> compile_nth env loop args
    | "length" -> compile_length env loop args
    | "sum" -> compile_aggr env loop A.Sum args
    | "take" -> compile_take env loop args
    | "drop" -> compile_drop env loop args
    | "zip" -> compile_zip env loop args
    | "unzip" -> compile_unzip env loop args
    | "concat" -> compile_concat env loop args
    | "and" -> compile_and env loop args
    | "or" -> compile_or env loop args
    | "empty" -> compile_empty env loop args
    | "<" | "<=" | ">=" ->
	failwith ("CompileQuery.compile_apply: </<=/>= should have been rewritten in query2")
    | s ->
	failwith ("CompileQuery.op_dispatch: " ^ s ^ " not implemented")

and compile_for env loop v e1 e2 order_criteria =
  let Ti (q1, cs1, itbls1, _) = compile_expression env loop e1 in
  let q1' = 
    A.Dag.mk_rownum
      (inner, [(iter, A.Ascending); (pos, A.Ascending)], None)
      q1
  in
  let q_v = 
    A.Dag.mk_attach
      (pos, A.Nat 1n)
      (A.Dag.mk_project
	 ((iter, inner) :: (prjlist (io (Cs.columns cs1))))
	 q1')
  in
  let map =
    A.Dag.mk_project
      [(outer, iter); prj inner]
      q1'
  in
  let loop_v =
    A.Dag.mk_project
      [prj iter]
      q_v
  in
  let env = AEnv.map (lift map) env in
  let env_v = AEnv.bind env (v, Ti (q_v, cs1, itbls1, dummy)) in
  let Ti (q2, cs2, itbls2, _) = compile_expression env_v loop_v e2 in
  let (order_cols, map') =
    match order_criteria with
      | _ :: _ ->
	  (* compile orderby expressions *)
	  let q_os = List.map (compile_expression env_v loop_v) order_criteria in
	  let q_os = List.map (fun (Ti (q, _, _, _)) -> q) q_os in
	  let offset = (List.length (Cs.columns cs2)) + 1 in
	  let cols = mapIndex (fun _ i -> A.Item (i + offset)) q_os in
	  let order_cols = List.map (fun c -> (c, A.Ascending)) (cols @ [pos]) in
	    (order_cols, omap map q_os cols)
      | [] ->
	  ([(iter, A.Ascending); (pos, A.Ascending)], map)
  in
  let q = A.Dag.mk_project
    ([(iter, outer); (pos, pos')] @ (prjlist (io (Cs.columns cs2))))
    (A.Dag.mk_rank
       (* (pos', [(iter, A.Ascending); (pos, A.Ascending)]) *)
       (pos', order_cols)
       (A.Dag.mk_eqjoin
	  (iter, inner)
	  q2
	  map'))
  in
    Ti(q, cs2, itbls2, dummy)

and singleton_record env loop (name, e) =
  let Ti (q, cs, itbls, _) = compile_expression env loop e in
    Ti (q, [`Mapping (name, cs)], itbls, dummy)

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

and merge_records (Ti (r1_q, r1_cs, r1_itbls, _)) (Ti (r2_q, r2_cs, r2_itbls, _)) =
  let r2_leafs = Cs.columns r2_cs in
  let new_names_r2 = io (incr r2_leafs (Cs.cardinality r1_cs)) in
  let old_names_r2 = io r2_leafs in
  let names_r1 = io (Cs.columns r1_cs) in
  let r2_itbls' = Itbls.incr_keys r2_itbls (Cs.cardinality r1_cs) in
  let q =
    A.Dag.mk_project
      (prjlist ([A.Iter 0; A.Pos 0] @ names_r1 @ new_names_r2))
      (A.Dag.mk_eqjoin
	 (iter, iter')
	 r1_q
	 ((A.Dag.mk_project
	     ((iter', iter) :: (prjlist_map new_names_r2 old_names_r2))
	     r2_q)))
  in
  let cs = Cs.append r1_cs r2_cs in
  let itbls = Itbls.append r1_itbls r2_itbls' in
    Ti (q, cs, itbls, dummy)

and compile_project env loop field record =
  let record_ti = compile_expression env loop record in
    do_project field record_ti

and compile_erase env loop erase_fields r =
  let Ti (q_r, cs_r, itbls_r, _) = compile_expression env loop r in
  let remaining_cs = Cs.filter_record_fields cs_r erase_fields in
  let remaining_cols = io (Cs.columns remaining_cs) in
  let remaining_itbls = Itbls.retain_by_keys itbls_r (Cs.columns remaining_cs) in
  let q =
    A.Dag.mk_project
      ([prj iter; prj pos] @ (prjlist remaining_cols))
      q_r
  in
    Ti(q, remaining_cs, remaining_itbls, dummy)

and compile_record env loop r =
  match r with
    | (name, value) :: [] ->
	singleton_record env loop (name, value)
    | (name, value) :: tl ->
	let f = singleton_record env loop (name, value) in
	  merge_records f (compile_record env loop tl)
    | [] ->
	failwith "CompileQuery.compile_record_value: empty record"

and compile_table loop ((_db, _params), tblname, keys, row) =
  List.iter (fun k -> Debug.print ("key " ^ (mapstrcat " " (fun x -> x) k))) keys;
  let (column_names, types) = 
    StringMap.fold
      (fun colname (_, typ) (cs, ts) -> (colname :: cs, (base_type_of_typ typ) :: ts))
      (fst (fst (Types.unwrap_row row)))
      ([], [])
  in
  let column_items = mapIndex (fun c i -> (c, A.Item (i + 1))) column_names in
  let key_items =
    List.map
      (fun key ->
	 List.map
	   (fun part_key ->
	      try 
		List.assoc part_key column_items 
	      with
		  NotFound _ -> failwith ("CompileQuery.compile_table: no column for key " ^ part_key))
	   key)
      keys
  in
  let attr_infos = List.map2 (fun (c, i) typ -> (i, c, typ)) column_items types in
  let offset = function
    | A.Item i -> i
    | _ -> assert false
  in
  let cs = 
    List.map 
      (fun (i, c, typ) -> `Mapping (c, [`Offset (offset i, typ)])) 
      attr_infos 
  in
  let q =
    A.Dag.mk_cross
      loop
      (A.Dag.mk_rank
	 (pos, (List.map (fun column -> (column, A.Ascending)) (List.hd key_items)))
	 (A.Dag.mk_tblref
	    (tblname, attr_infos, key_items)))
  in
    Ti (q, cs, Itbls.empty, dummy)

and compile_constant loop (c : Constant.constant) =
  let cs = [`Offset (1, A.column_type_of_constant c)] in
  let q =
    (A.Dag.mk_attach
       (A.Item 1, A.const c)
       (A.Dag.mk_attach
	  (A.Pos 0, A.Nat 1n)
	  loop))
  in
    Ti (q, cs, Itbls.empty, dummy)

(* if e1 then e2 else []:
   don't consider the else branch if it represents the empty list. *)
and compile_if2 env loop e1 e2 =
  let c = A.Item 1 in
  let select loop (Ti (q, cs, itbls, _)) =
    let cols = io (Cs.columns cs) in
    let q' =
      A.Dag.mk_project
	([prj iter; prj pos] @ (prjlist cols))
	(A.Dag.mk_eqjoin
	   (iter, iter')
	   q
	   (A.Dag.mk_project
	      [(iter', iter)]
	      loop))
    in
    let itbls' = suse q itbls in
      Ti (q', cs, itbls', dummy)
  in
  (* condition *)
  let Ti (q_e1, cs_e1, _, _) = compile_expression env loop e1 in
    assert (Cs.is_operand cs_e1);
    let loop_then =
      A.Dag.mk_project
	[prj iter]
	(A.Dag.mk_select
	   c
	   q_e1)
    in
    let env_then = AEnv.map (select loop_then) env in
    let Ti (q_e2, cs_e2, itbls_e2, _) = compile_expression env_then loop_then e2 in
      Ti (q_e2, cs_e2, itbls_e2, dummy)

and compile_if env loop e1 e2 e3 =
  let c = A.Item 1 in
  let res = A.Item 2 in
  let select loop (Ti (q, cs, itbls, _)) =
    let cols = io (Cs.columns cs) in
    let q' =
      A.Dag.mk_project
	([prj iter; prj pos] @ (prjlist cols))
	(A.Dag.mk_eqjoin
	   (iter, iter')
	   q
	   (A.Dag.mk_project
	      [(iter', iter)]
	      loop))
    in
    let itbls' = suse q itbls in
      Ti (q', cs, itbls', dummy)
  in
  (* condition *)
  let Ti (q_e1, cs_e1, _, _) = compile_expression env loop e1 in
    assert (Cs.is_operand cs_e1);
    let loop_then =
      A.Dag.mk_project
	[prj iter]
	(A.Dag.mk_select
	   c
	   q_e1)
    in
    let loop_else =
      A.Dag.mk_project
	[prj iter]
	(A.Dag.mk_select
	   res
	   (A.Dag.mk_funboolnot
	      (res, c)
	      q_e1))
    in
    let env_then = AEnv.map (select loop_then) env in
    let env_else = AEnv.map (select loop_else) env in
    let Ti (q_e2, cs_e2, itbls_e2, _) = compile_expression env_then loop_then e2 in
    let Ti (q_e3, _cs_e3, itbls_e3, _) = compile_expression env_else loop_else e3 in
    let q =
      A.Dag.mk_rownum
	(item', [(iter, A.Ascending); (ord, A.Ascending); (pos, A.Ascending)], None)
	(A.Dag.mk_disjunion
	   (A.Dag.mk_attach
	      (ord, A.Nat 1n)
	      q_e2)
	   (A.Dag.mk_attach
	      (ord, A.Nat 2n)
	      q_e3))
    in
    let cols = Cs.columns cs_e2 in
    let keys = Itbls.keys itbls_e2 in
    let proj = [prj iter; prj pos] in
    let proj = proj @ (prjlist (io (difference cols keys))) in
    let proj = proj @ (prjlist_single (io keys) item') in
    let q' = 
      A.Dag.mk_project
	proj
	q
    in
    let itbls' = suap q itbls_e2 itbls_e3 in
      Ti (q', cs_e2, itbls', dummy)

and compile_groupby env loop v g_e e =
  let Ti (q_e, cs_e, itbls_e, _) = compile_expression env loop e in
  let q_v =
    A.Dag.mk_rownum
      (inner, [(iter, A.Ascending); (pos, A.Ascending)], None)
      q_e
  in
  let loop_v =
    A.Dag.mk_project
      [(iter, inner)]
      q_v
  in
  let map_v = 
    A.Dag.mk_project
      [(outer, iter); (prj inner)]
      q_v
  in
  let q_v' = 
    A.Dag.mk_attach
      (pos, A.Nat 1n)
      (A.Dag.mk_project
	 ([(iter, inner)] @ (prjlist (io (Cs.columns cs_e))))
	 q_v)
  in
  let env_v = AEnv.map (lift map_v) env in
  let env_v = AEnv.bind env_v (v, Ti(q_v', cs_e, itbls_e, dummy)) in
  let Ti(q_eg, cs_eg, _, _) = compile_expression env_v loop_v g_e in
  let cs_eg' = Cs.shift cs_eg (Cs.cardinality cs_e) in
  let sortlist = List.map (fun c -> (A.Item c, A.Ascending)) (Cs.columns cs_eg') in
  let q_1 =
    A.Dag.mk_rowrank
      (grp_key, (iter, A.Ascending) :: sortlist)
      (A.Dag.mk_eqjoin
	 (inner, iter')
	 q_v
	 (A.Dag.mk_project
	    ((iter', iter) :: (prjlist_map (io (Cs.columns cs_eg')) (io (Cs.columns cs_eg))))
	    q_eg))
  in
  let grpkey_col = (Cs.cardinality cs_eg) + 1 in
  let q_2 =
    A.Dag.mk_distinct
      (A.Dag.mk_project
	 ([prj iter; (pos, grp_key); (A.Item grpkey_col, grp_key)] @ (prjlist_map (io (Cs.columns cs_eg)) (io (Cs.columns cs_eg'))))
	 q_1)
  in
  let q_3 =
    A.Dag.mk_project
      ([(iter, grp_key); (prj pos)] @ (prjlist (io (Cs.columns cs_e))))
      q_1
  in
  let cs = [`Mapping ("1", cs_eg); `Mapping ("2", [`Offset (grpkey_col, `Surrogate)])] in
  let itbls = [(grpkey_col, Ti(q_3, cs_e, itbls_e, dummy))] in
    Ti(q_2, cs, itbls, dummy)

and compile_expression env loop e : tblinfo =
  match e with
    | `Constant (c, _) -> compile_constant loop c
    | `Apply ((f, args), _) -> compile_apply env loop f args 
    | `Var (x, _) -> AEnv.lookup env x
    | `Project ((r, field), _) -> compile_project env loop field r
    | `Record (r, _) -> compile_record env loop (StringMap.to_alist r)
    | `Extend ((r, ext_fields), _) ->
	let ext_fields = StringMap.to_alist ext_fields in
	  extend_record env loop ext_fields (opt_map (compile_expression env loop) r)
    | `Erase ((r, erase_fields), _) -> compile_erase env loop erase_fields r
    | `Singleton (e, _) -> compile_expression env loop e
    | `Append (l, _) -> compile_append env loop l
    | `Table (t, _) -> compile_table loop t
    | `If ((c, t, Some e), _) -> compile_if env loop c t e
    | `If ((c, t, None), _) -> compile_if2 env loop c t
    | `For (([x, l], os, body), _) -> compile_for env loop x l body os
    | `For _ -> failwith "compile_expression: multi-generator for-expression not implemented"
    | `Box (e, _) -> compile_box env loop e
    | `Unbox (e, _) -> compile_unbox env loop e
    | `GroupBy (((x, group_exp), source), _) -> compile_groupby env loop x group_exp source 
    | `Variant _
    | `XML _ -> failwith "compile_expression: not implemented"
    | `Primitive _ -> failwith "compile_expression: eval error"

let wrap_serialize (Ti (q,cs,_,_)) =
  A.Dag.mk_serializerel 
    (A.Iter 0, A.Pos 0, io (Cs.columns cs))
    (A.Dag.mk_nil)
    q

let rec collect_itbls (plan_id, ref_id) itbls collected =
  match itbls with
    | (offset, (Ti(_, cs, [], _) as ti)) :: remaining_itbls ->
	let l = (plan_id, ((ref_id, offset), (wrap_serialize ti), cs)) :: collected in
	  collect_itbls (plan_id + 1, ref_id) remaining_itbls l
    | (offset, (Ti(_, cs, itbls, _) as ti)) :: remaining_itbls ->
	let (next_id, l) = collect_itbls (plan_id + 1, plan_id) itbls [] in
	let l = (plan_id, ((ref_id, offset), (wrap_serialize ti), cs)) :: (l @ collected) in
	  collect_itbls (next_id, plan_id) remaining_itbls l
    | [] ->
	(plan_id, collected)
      
let compile e =
  let loop = 
    (A.Dag.mk_littbl
       ([[A.Nat 1n]], [(A.Iter 0, `NatType)]))
  in
  let Ti (_, cs, itbls, _) as ti = compile_expression AEnv.empty loop e in
    (* Debug.print (Cs.print cs); *)
    (wrap_serialize ti), cs, snd (collect_itbls (1, 0) itbls [])
