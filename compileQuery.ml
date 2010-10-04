open Utility

module A = Algebra
module ADag = Algebra_dag

module rec Ts : sig

  (* type for the assoc list mapping surrogate columns to the corresponding 
     tables for inner lists *)
  type t = (int * ExpressionToAlgebra.tblinfo) list

  val empty : t
  val keys : t -> int list
  val incr_cols : t -> int -> t
  val decr_cols : t -> int -> t
  val append : t -> t -> t
  val lookup : int -> t -> ExpressionToAlgebra.tblinfo
  val length : t -> int
  val keep_cols : t -> int list -> t
end = 
struct
  type t = (int * ExpressionToAlgebra.tblinfo) list

  let empty = []
  let keys ts = List.map fst ts
  let incr_cols ts i = List.map (fun (offset, ti) -> (offset + i, ti)) ts
  let decr_cols ts i =  List.map (fun (offset, ti) -> (offset - i, ti)) ts
  let append = List.append
  let lookup = List.assoc
  let length = List.length
  (* remove all mappings whose refcols are not in keys *)
  let keep_cols = keep_keys
end

and Vs :
sig
  (* type for the assoc list mapping tags (variant types) and a surrogate 
     column (composite key) to the corresponding tables for tagged values *)
  type t = ((int * string) * (ExpressionToAlgebra.tblinfo * Cs.implementation_type)) list

  val empty : t
  val key_columns : t -> int list
  val incr_cols : t -> int -> t
  val decr_cols : t -> int -> t
  val append : t -> t -> t
  val lookup : (int * string) -> t -> ExpressionToAlgebra.tblinfo * Cs.implementation_type
  val lookup_col : int -> t -> t
  val length : t -> int
  val keep_cols : t -> int list -> t
end
=
struct
  type t = ((int * string) * (ExpressionToAlgebra.tblinfo * Cs.implementation_type)) list

  let empty = []
  let key_columns vs : int list = List.map (fst -<- fst) vs
  let incr_cols vs i = List.map (fun ((col, tag), ti) -> ((col + i, tag), ti)) vs
  let decr_cols vs i = List.map (fun ((col, tag), ti) -> ((col - i, tag), ti)) vs
  let append = List.append
  let lookup = List.assoc
  let lookup_col c vs = List.filter (fun ((col, _), _) -> col = c) vs
  let length = List.length
  let keep_cols vs cols = List.filter (fun ((col, _), _) -> List.mem col cols) vs
end

and ExpressionToAlgebra : 
sig
  type tblinfo = { q : ADag.t; cs : Cs.t; ts : Ts.t; vs : Vs.t }
  type error_plan = ADag.t option
      
  val compile : Query2.Annotate.typed_t -> tblinfo * error_plan
end
= 
struct

  module AEnv = Env.Int

  type tblinfo = { q : ADag.t; cs : Cs.t; ts : Ts.t; vs : Vs.t }
  type error_plan = ADag.t option

(* module-global reference that stores the global error plan of the generated
   plan bundle (if there is one) *)
  let errors = ref None

(* merge a new error plan with the previous ones with a disjoint union *)
  let merge_error_plans q_error =
    match !errors with
      | Some q_old ->
 	errors := Some (ADag.mk_disjunion q_old q_error)
      | None ->
	errors := Some q_error

  let pf_type_of_typ t = 
    let concrete_t = Types.concrete_type t in
    match concrete_t with
      | `Primitive `Bool -> `BoolType
      | `Primitive `Int -> `IntType
      | `Primitive `Char -> `CharType
      | `Primitive `Float -> `FloatType
      | `Primitive `String -> `StrType
      | _ -> failwith ("unsupported type " ^ (Types.string_of_datatype concrete_t))

(* the environment mapping variables to algebra plans *)

  let incr l i = List.map (fun j -> j + i) l
  let decr l i = List.map (fun j -> j - i) l

  let io = List.map (fun i -> A.Item i)

(* project a column onto itself (i.e. keep it) *)
  let prj col = (col, col)

(* project every column in the list onto itself *)
  let prjlist = List.map prj

(* project all columns in old_cols onto the corresponding columns in
   new_cols *)
  let prjlist_map new_cols old_cols = List.combine new_cols old_cols

(* project one single column old_col to all columns in new_col *)
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

(* wrapper for binary functions/operators *)
  let wrap_1to1 f res c c' algexpr =
    ADag.mk_fun1to1
      (f, res, [c; c'])
      algexpr

(* wrapper for logical and *)
  let wrap_and res c c' algexpr =
    ADag.mk_funbooland
      (res, (c, c'))
      algexpr

(* wrapper for logical or *)
  let wrap_or res c c' algexpr =
    ADag.mk_funboolor
      (res, (c, c'))
      algexpr

(* wrapper for equal *)
  let wrap_eq res c c' algexpr =
    ADag.mk_funnumeq
      (res, (c, c'))
      algexpr

(* wrapper for not equal *)
  let wrap_ne res c c' algexpr =
    ADag.mk_funboolnot
      (res, res')
      (ADag.mk_funnumeq
	 (res', (c, c'))
	 algexpr)

(* wrapper for greater than *)
  let wrap_gt res c c' algexpr =
    ADag.mk_funnumgt
      (res, (c, c'))
      algexpr

(* wrapper for less than *)
  let wrap_lt rescol c c' algexpr = 
    wrap_gt rescol c' c algexpr

(* wrapper for not *)
  let wrap_not res op_attr algexpr =
    ADag.mk_funboolnot
      (res, op_attr)
      algexpr

(* wrapper for aggregate operators *)
  let wrap_agg loop q attachment =
    ADag.mk_attach
      (pos, A.Nat 1n)
      (ADag.mk_disjunion
	 q
	 (ADag.mk_attach
	    (A.Item 1, attachment)
	    (ADag.mk_difference
	       loop
	       (ADag.mk_project
		  [prj iter]
		  q))))

(* generate the algebra code for the application of a binary operator to two
   columns. the columns represent primitive values *)
  let do_primitive_binop wrapper op1 op2 =
    let c = A.Item 1 in
    let c' = A.Item 2 in
    let res = A.Item 3 in
    ADag.mk_project
      [(prj iter); (prj pos); (c, res)]
      (wrapper 
	 res c c'
	 (ADag.mk_eqjoin
	    (iter, iter')
	    op1
	    (ADag.mk_project
	       [(iter', iter); (c', c)]
	       op2)))

  let do_primitive_binop_ti wrapper restype e1 e2 =
    assert (Cs.is_atomic e1.cs);
    assert (Cs.is_atomic e2.cs);
    { 
      q = do_primitive_binop wrapper e1.q e2.q;
      cs = Cs.Column (1, restype);
      ts = Ts.empty; 
      vs = Vs.empty 
    }

  let smaller = do_primitive_binop_ti wrap_lt `BoolType
  let greater = do_primitive_binop_ti wrap_gt `BoolType
  let equal = do_primitive_binop_ti wrap_eq `BoolType
  let or_op = do_primitive_binop_ti wrap_or `BoolType
  let and_op = do_primitive_binop_ti wrap_and `BoolType

(* unbox the inner tables represented by inner_ti, where
   q_e is the outer table and surr_col is the surrogate column
   in q_e which references entries in inner_ti *)
  let do_unbox q_e surr_col inner_ti =
    let q_unbox =
      ADag.mk_project
	([(iter, iter'); prj pos] @ (prjlist (io (Cs.offsets inner_ti.cs))))
	(ADag.mk_eqjoin
	   (c', iter)
	   (ADag.mk_project
	      [(iter', iter); (c', A.Item surr_col)]
	      q_e)
	   inner_ti.q)
    in
    {
      q = q_unbox;
      cs = inner_ti.cs;
      ts = inner_ti.ts;
      vs = inner_ti.vs 
    }

  (* *)
  let do_project field record =
    let field_cs' = Cs.lookup_record_field record.cs field in
    let old_cols = Cs.offsets field_cs' in
    let offset = List.hd old_cols in
    let new_cols = incr old_cols (-offset + 1) in
    let q =
      ADag.mk_project
	([prj iter; prj pos] @ prjlist_map (io new_cols) (io old_cols))
	record.q
    in
    { 
      q = q; 
      cs = Cs.shift (-offset + 1) field_cs';
      ts = Ts.decr_cols (Ts.keep_cols record.ts old_cols) (offset - 1); 
      vs = Vs.decr_cols (Vs.keep_cols record.vs old_cols) (offset - 1);
    }

  let do_length loop l =
    let q = 
      ADag.mk_funaggrcount
	(A.Item 1, Some iter)
	l.q
    in
    {
      q =  wrap_agg loop q (A.Int (Num.Int 0));
      cs = Cs.Column (1, `IntType); 
      ts = Ts.empty; 
      vs = Vs.empty 
    }

(* q_e1 and q_e2 must have absolute positions *)
  let do_zip e1 e2 =
    let card_e1 = List.length (Cs.offsets e1.cs) in
    let cs_e2' = Cs.shift card_e1 e2.cs in
    let ts_e2' = Ts.incr_cols e2.ts card_e1 in
    let vs_e2' = Vs.incr_cols e2.vs card_e1 in
    let items = io ((Cs.offsets e1.cs) @ (Cs.offsets cs_e2')) in
    let q =
      ADag.mk_project
	([prj iter; prj pos] @ (prjlist items))
	(ADag.mk_select
	   res
	   (ADag.mk_funnumeq
	      (res, (pos, pos'))
	      (ADag.mk_eqjoin
		 (iter, iter')
		 e1.q
		 (ADag.mk_project
		    ([(iter', iter); (pos', pos)] @ (prjlist_map (io (Cs.offsets cs_e2')) (io (Cs.offsets e2.cs))))
		    e2.q))))
    in
    {
      q = q;
      cs = Cs.Mapping [("1", e1.cs); ("2", cs_e2')];
      ts = Ts.append e1.ts ts_e2';
      vs = Vs.append e1.vs vs_e2';
    }

(* apply the all aggregate operator to the first column grouped by iter 
   (corresponds to the function "and" from the links prelude *)
  let do_list_and loop l =
    assert (Cs.is_atomic l.cs);
    let q' = 
      (ADag.mk_funaggr
	 (A.All, (A.Item 1, A.Item 1), Some iter)
	 l.q)
    in
    let q'' = wrap_agg loop q' (A.Bool true) in
    { q = q''; cs = Cs.Column (1, `BoolType); ts = Ts.empty; vs = Vs.empty }

(* apply the min aggregate operator to the first column grouped by iter 
   (corresponds to the function "or" from the links prelude *)
  let do_list_or loop l =
    assert (Cs.is_atomic l.cs);
    let q' =
      ADag.mk_funaggr
	(A.Max, (A.Item 1, A.Item 1), Some iter)
	l.q
    in
    let q'' = wrap_agg loop q' (A.Bool false) in
    { q = q''; cs = Cs.Column (1, `BoolType); ts = Ts.empty; vs = Vs.empty }

(* join two inner tables together and compute new surrogate keys *)
  let combine_inner_tables q_l q_r =
    ADag.mk_rownum 
      (item', [(iter, A.Ascending); (ord, A.Ascending); (pos, A.Ascending)], None)
      (ADag.mk_disjunion
	 (ADag.mk_attach
	    (ord, A.Nat 1n)
	    q_l)
	 (ADag.mk_attach
	    (ord, A.Nat 2n)
	    q_r))

(* use the new surrogate keys from q_outer in column surr_col in the 
   nested plan q_inner *)
  let renumber_inner_table q_outer q_inner surr_col = 
    ADag.mk_thetajoin
      [(A.Eq, (ord, ord')); (A.Eq, (iter, c'))]
      q_inner
      (ADag.mk_project
	 [(ord', ord); (item'', item'); (c', A.Item surr_col)]
	 q_outer)

(* project all columns which do _not_ contain reference values onto itself and use the fresh
   surrogate values in new_surr in all columns which _do_ contain reference values *)
  let refresh_surr_cols cs ts_l ts_r  vs_l vs_r new_surr =

    let int_union l r = IntSet.elements (IntSet.union (IntSet.from_list l) (IntSet.from_list r)) in
    let ts_cols = int_union (Ts.keys ts_l) (Ts.keys ts_r) in
    let vs_cols = int_union (Vs.key_columns vs_l) (Vs.key_columns vs_r) in

  (* all columns which are neither vs nor ts surrogate columns *)
    prjlist (io (difference (Cs.offsets cs) (vs_cols @ ts_cols)))

  (* use new keys in vs surrogate columns *)
    @ (prjlist_single (io vs_cols) new_surr) 

  (* use new keys in ts surrogate columns *)
    @ (prjlist_single (io ts_cols) new_surr)

(* append the corresponding vs entries from vs_l and vs_r *)
  let rec append_vs q_outer vs_l vs_r =
    let m = List.map (append_matching_vs q_outer) (same_keys vs_l vs_r) in
    let l = List.map (append_missing_vs q_outer (A.Nat 1n)) (missing_keys vs_l vs_r) in
    let r = List.map (append_missing_vs q_outer (A.Nat 2n)) (missing_keys vs_r vs_l) in
    List.sort compare (m @ l @ r)

  and append_matching_vs (q_outer : ADag.t) ((refcol, tag), ((ti_l, itype_l), (ti_r, _itype_r))) =
    let q_combined = combine_inner_tables ti_l.q ti_r.q in

    let projlist = [(iter, item''); prj pos] @ (refresh_surr_cols ti_l.cs ti_l.ts ti_r.ts ti_l.vs ti_r.vs item') in

    let q_renumbered = renumber_inner_table q_outer q_combined refcol in

    let q = ADag.mk_project projlist q_renumbered in

    let cs = Cs.choose_nonempty ti_l.cs ti_r.cs in

    let ts = append_ts q_combined ti_l.ts ti_r.ts in

    let vs = append_vs q_combined ti_l.vs ti_r.vs in
    (refcol, tag), ({ q = q; cs = cs; ts = ts; vs = vs }, itype_l)

  and append_missing_vs q_outer ord_val ((refcol, tag), (ti, itype)) =
    let q_combined = 
      ADag.mk_rownum
	(item', [(iter, A.Ascending); (ord, A.Ascending); (pos, A.Ascending)], None)
	(ADag.mk_attach
	   (ord, ord_val)
	   ti.q)
    in
    let projlist = [(iter, item''); prj pos] @ (refresh_surr_cols ti.cs ti.ts Ts.empty ti.vs Vs.empty item') in
    
    let q_renumbered = renumber_inner_table q_outer q_combined refcol in

    let q_refreshed = ADag.mk_project projlist q_renumbered in
    let ts = append_ts q_combined ti.ts [] in
    let vs = append_vs q_combined ti.vs [] in
    (refcol, tag), ({ q = q_refreshed; cs = ti.cs; ts = ts; vs = vs }, itype)

  and append_ts q_outer ts_l ts_r =
    let m = List.map (append_matching_ts q_outer) (same_keys ts_l ts_r) in
    let l = List.map (append_missing_ts q_outer (A.Nat 1n)) (missing_keys ts_l ts_r) in
    let r = List.map (append_missing_ts q_outer (A.Nat 2n)) (missing_keys ts_r ts_l) in
    List.sort compare (m @ l @ r)

  and append_matching_ts (q_outer : ADag.t) (refcol, (ti_l, ti_r)) =
    let q_combined = combine_inner_tables ti_l.q ti_r.q in

    let projlist = [(iter, item''); prj pos] @ (refresh_surr_cols ti_l.cs ti_l.ts ti_r.ts ti_l.vs ti_r.vs item') in

    let q_renumbered = renumber_inner_table q_outer q_combined refcol in

    let q = ADag.mk_project projlist q_renumbered in

    let cs = Cs.choose_nonempty ti_l.cs ti_r.cs in

    let ts = append_ts q_combined ti_l.ts ti_r.ts in

    let vs = append_vs q_combined ti_l.vs ti_r.vs in
    refcol, { q = q; cs = cs; ts = ts; vs = vs }

  and append_missing_ts q_outer ord_val (refcol, ti) =
    let q_combined = 
      ADag.mk_rownum
	(item', [(iter, A.Ascending); (ord, A.Ascending); (pos, A.Ascending)], None)
	(ADag.mk_attach
	   (ord, ord_val)
	   ti.q)
    in
    let projlist = [(iter, item''); prj pos] @ (refresh_surr_cols ti.cs ti.ts Ts.empty ti.vs Vs.empty item') in
    
    let q_renumbered = renumber_inner_table q_outer q_combined refcol in

    let q_refreshed = ADag.mk_project projlist q_renumbered in
    let ts = append_ts q_combined ti.ts [] in
    let vs = append_vs q_combined ti.vs [] in
    refcol, { q = q_refreshed; cs = ti.cs; ts = ts; vs = vs }

(* only keep those tuples in an nested table which are actually referenced from the
   outer table *)
  let rec slice_inner_tables (q_outer : ADag.t) (ts : Ts.t) : Ts.t =

    let slice (surr_col, inner) =
      let q_inner' = 
	ADag.mk_project
	  ([prj iter; prj pos] @ (prjlist (io (Cs.offsets inner.cs))))
	  (ADag.mk_eqjoin
	     (iter, iter')
	     inner.q
	     (ADag.mk_project
		[(iter', A.Item surr_col)]
		q_outer))
      in
      (surr_col, { q = q_inner'; cs = inner.cs; ts = (slice_inner_tables q_inner' inner.ts); vs = inner.vs })
    in

    if Settings.get_value Basicsettings.Ferry.slice_inner_tables then
      List.map slice ts
    else
      ts

  (* helper function for compile_if/compile_case:
     remove all iterations from environment entries which are not in loop *)
  let fragment_env loop env = 
    let select loop ti =
      let cols = io (Cs.offsets ti.cs) in
      let q' =
	ADag.mk_project
	  ([prj iter; prj pos] @ (prjlist cols))
	  (ADag.mk_eqjoin
	     (iter, iter')
	     ti.q
	     (ADag.mk_project
		[(iter', iter)]
		loop))
      in
      {
	q = q';
	cs = ti.cs;
	ts = slice_inner_tables ti.q ti.ts;
	vs = ti.vs;
      }
    in
    AEnv.map (select loop) env

(* derive an iteration context from the list represented by q and map
   q into this new iteration context *)
  let map_forward q cs =
    let q_renumbered = 
      ADag.mk_rownum
	(inner, [(iter, A.Ascending); (pos, A.Ascending)], None)
	q
    in

    let q_v = 
      ADag.mk_attach
	(pos, A.Nat 1n)
	(ADag.mk_project
	   ((iter, inner) :: (prjlist (io (Cs.offsets cs))))
	   q_renumbered)
    in
    let map =
      ADag.mk_project
	[(outer, iter); prj inner; (pos', pos)]
	q_renumbered
    in
    let loop =
      ADag.mk_project
	[prj iter]
	q_v
    in
    (q_renumbered, q_v, map, loop)

(* lift q into the new iteration context represented by map *)
  let lift map ti =
    let q' =
      (ADag.mk_project
	 ([(iter, inner); prj pos] @ (prjlist (io (Cs.offsets ti.cs))))
	 (ADag.mk_eqjoin
	    (iter, outer)
	    ti.q
	    map))
    in
    { ti with q = q' }

(* construct the ordering map of a for-loop *)
  let rec omap map sort_criteria sort_cols =
    match (sort_criteria, sort_cols) with
      | (o :: os), (col :: cols) ->
	ADag.mk_project
	  ([prj outer; prj inner; (col, A.Item 1)] @ (prjlist cols))
	  (ADag.mk_eqjoin
	     (inner, iter)
	     (omap map os cols)
	     o)
      | [], [] ->
	map
      | _ -> assert false

(* compute absolute positions for q *)
  let abspos q cs =
    let cols = io (Cs.offsets cs) in
    ADag.mk_project
      ([prj iter; prj pos] @ (prjlist cols))
      (ADag.mk_rownum
	 (pos, [(pos', A.Ascending)], Some iter)
	 (ADag.mk_project
	    ([prj iter; (pos', pos)] @ (prjlist cols))
	    q))

(* compute absolute positions for a tblinfo *)
  let abspos_ti ti = { ti with q = abspos ti.q ti.cs }

  let rec compile_box env loop e =
    let ti_e = compile_expression env loop e in
    let q_o = 
      ADag.mk_attach
	(pos, A.Nat 1n)
	(ADag.mk_project 
	   [(prj iter); (A.Item 1, iter)]
	   loop)
    in
    { 
      q = q_o;
      cs = Cs.Column (1, `Surrogate);
      ts = [(1, ti_e)];
      vs = Vs.empty 
    }

  and compile_unbox env loop e =
    let ti = compile_expression env loop e in
    assert ((Cs.cardinality ti.cs) = 1);
    assert ((List.length ti.ts) = 1);
    let (offset, inner_ti) = List.hd ti.ts in
    do_unbox ti.q offset inner_ti

  and compile_append env loop l =
    match l with
      | e :: [] ->
	compile_expression env loop e
      | hd_e :: tl_e ->
	let hd = compile_expression env loop hd_e in
	let tl = compile_append env loop tl_e in
	compile_list hd tl
      | [] ->
	  {
	    q = ADag.mk_emptytbl;
	    cs = Cs.Column (1, `EmptyListLit);
	    ts = Ts.empty;
	    vs = Vs.empty
	  }

  and compile_list hd tl = 
    let cs = Cs.choose_nonempty hd.cs tl.cs in
  (* combine the two lists and compute new surrogate values *)
    let q =
      ADag.mk_rownum
	(item', [(iter, A.Ascending); (ord, A.Ascending); (pos, A.Ascending)], None)
	(ADag.mk_rank
	   (pos', [(ord, A.Ascending); (pos, A.Ascending)])
	   (ADag.mk_disjunion
	      (ADag.mk_attach
		 (ord, A.Nat 1n)
		 hd.q)
	      (ADag.mk_attach
		 (ord, A.Nat 2n)
		 tl.q)))
    in
    let q'_projlist = [prj iter; (pos, pos')] @ (refresh_surr_cols cs hd.ts tl.ts hd.vs tl.vs item') in
    let q' = 
      ADag.mk_project
	q'_projlist
	q
    in
    {
      q = q';
      cs = cs;
      ts = append_ts q hd.ts tl.ts;
      vs = append_vs q hd.vs tl.vs;
    }

  and compile_zip env loop l1 l2 =
    let ti_l1 = compile_expression env loop l1 in
    let ti_l2 = compile_expression env loop l2 in
    let q_l1' = abspos ti_l1.q ti_l1.cs in
    let q_l2' = abspos ti_l2.q ti_l2.cs in
    do_zip { ti_l1 with q = q_l1' } { ti_l2 with q = q_l2' }

  and compile_unzip env loop zipped =
    let ti_zipped = compile_expression env loop zipped in
    let q = 
      ADag.mk_project
	([prj iter; prj pos] @ (prjlist_single [A.Item 1; A.Item 2] iter))
	(ADag.mk_attach
	   (pos, A.Nat 1n)
	   loop)
    in
    let cs_1 = Cs.lookup_record_field ti_zipped.cs "1" in
    let cs_2 = Cs.lookup_record_field ti_zipped.cs "2" in
    let cols_1 = Cs.offsets cs_1 in
    let card = List.length (Cs.offsets cs_1) in
    let cols_2 = Cs.offsets cs_2 in
    let cs_2' = Cs.shift (-card) cs_2 in
    let card = List.length cols_1 in
    let q_1 = 
      ADag.mk_project
	([prj iter; prj pos] @ (prjlist (io cols_1)))
	ti_zipped.q
    in
    let q_2 =
      ADag.mk_project
	(let proj = prjlist_map (io (decr cols_2 card)) (io cols_2) in
	 ([prj iter; prj pos] @ proj))
	ti_zipped.q
    in
    let ts_1 = Ts.keep_cols ti_zipped.ts cols_1 in
    let ts_2 = Ts.decr_cols (Ts.keep_cols ti_zipped.ts cols_2) card in
    let vs_1 = Vs.keep_cols ti_zipped.vs cols_1 in
    let vs_2 = Vs.decr_cols (Vs.keep_cols ti_zipped.vs cols_2) card in
    {
      q = q; 
      cs = Cs.Mapping [("1", Cs.Column (1, `Surrogate)); ("2", Cs.Column (2, `Surrogate))];
      ts = [(1, { q = q_1; cs = cs_1; ts = ts_1; vs = vs_1 });
	    (2, { q = q_2; cs = cs_2'; ts = ts_2; vs = vs_2 })];
      vs = Vs.empty 
    }

  (* FIXME: unite at least compile_or/and/length *)
  and compile_or env loop l =
    let ti_l = compile_expression env loop l in
    do_list_or loop ti_l

  and compile_and env loop l =
    let ti_l = compile_expression env loop l in
    do_list_and loop ti_l

  and compile_length env loop l =
    let ti_l = compile_expression env loop l in
    do_length loop ti_l

  and compile_empty env loop l = 
    let ti_l = compile_expression env loop l in
    let ti_length = do_length loop ti_l in
    assert (Cs.is_atomic ti_length.cs);
    let q =
      ADag.mk_project
	[prj iter; prj pos; (A.Item 1, res)]
	(ADag.mk_funnumeq
	   (res, (A.Item 1, A.Item 2))
	   (ADag.mk_attach
	      (A.Item 2, A.Int (Num.Int 0))
	      ti_length.q))
    in
    {
      q = q; 
      cs = Cs.Column (1, `BoolType);
      ts = Ts.empty; 
      vs = Vs.empty 
    }

(* application of sum to [] is defined as 0 *)
  and compile_sum env loop l =
    let c = A.Item 1 in
    let ti_l = compile_expression env loop l in
    assert (Cs.is_atomic ti_l.cs);
    let q = 
      (ADag.mk_funaggr
	 (A.Sum, (c, c), Some iter)
	 ti_l.q)
    in
    let q' = wrap_agg loop q (A.Int (Num.Int 0)) in
    {
      q = q';
      cs = Cs.Column (1, `IntType);
      ts = Ts.empty;
      vs = Vs.empty
    }

(* aggregate functions which are not defined on empty lists. the result
   is returned as a Maybe a, where sum(l) = Nothing iff l = [] *)
  and compile_aggr_error env loop aggr_fun restype l =
    let c = A.Item 1 in
    let ti_l = compile_expression env loop l in
    assert (Cs.is_atomic ti_l.cs);
    let q_inner_just = 
      ADag.mk_attach
	(pos, A.Nat 1n)
	(match aggr_fun with
	  | A.Avg -> 
	    ADag.mk_funaggr
	      (aggr_fun, (c, item'), Some iter)
	      (ADag.mk_cast
		 (item', c, `FloatType)
		 ti_l.q)
	  | _ -> 
	    ADag.mk_funaggr
	      (aggr_fun, (c, c), Some iter)
	      ti_l.q)
    in
    let empty_iterations =
      ADag.mk_difference
	loop
	(ADag.mk_project
	   [prj iter]
	   ti_l.q)
    in
    let ti_inner_nothing = compile_unit empty_iterations in
    let q_outer_just =
      ADag.mk_attach
	(pos, A.Nat 1n)
	(ADag.mk_attach
	   (A.Item 1, A.String "Just")
	   (ADag.mk_project
	      [prj iter; (A.Item 2, iter)]
	      q_inner_just))
    in
    let q_outer_nothing =
      ADag.mk_attach
	(pos, A.Nat 1n)
	(ADag.mk_attach
	   (A.Item 1, A.String "Nothing")
	   (ADag.mk_project
	      [prj iter; (A.Item 2, iter)]
	      empty_iterations))
    in
    let cs_inner_just = Cs.Column (1, restype) in
    let outer_cs = Cs.Tag ((1, `Tag), (2, `Surrogate)) in
    let ti_inner_just =
      {
	q = q_inner_just;
	cs = cs_inner_just;
	ts = Ts.empty;
	vs = Vs.empty
      }
    in	
    let vs = [((2, "Just"), (ti_inner_just, `Atom)); 
	      ((2, "Nothing"), (ti_inner_nothing, `Atom))] in
    let q_outer = ADag.mk_disjunion q_outer_just q_outer_nothing in
    {
      q = q_outer;
      cs = outer_cs;
      ts = Ts.empty;
      vs = vs
    }

  and compile_nth env loop i l =
    let ti_i = compile_expression env loop i in
    let ti_l = compile_expression env loop l in
    let q_l' = abspos ti_l.q ti_l.cs in
    let q_inner_just =
      (ADag.mk_project
	 ([prj iter; prj pos] @ prjlist (io (Cs.offsets ti_l.cs)))
	 (ADag.mk_select
	    res
	    (ADag.mk_funnumeq
	       (res, (pos', c'))
	       (ADag.mk_eqjoin
		  (iter, iter')
		  (ADag.mk_cast
		     (pos', pos, `IntType)
		     q_l')
		  (ADag.mk_project
		     [(iter', iter); (c', A.Item 1)]
		     ti_i.q)))))
    in
    let empty_iterations =
      ADag.mk_difference
	loop
	(ADag.mk_project
	   [prj iter]
	   q_inner_just)
    in
    let ti_inner_nothing = compile_unit empty_iterations in
    let q_outer_just =
      ADag.mk_attach
	(pos, A.Nat 1n)
	(ADag.mk_attach
	   (A.Item 1, A.String "Just")
	   (ADag.mk_project
	      [prj iter; (A.Item 2, iter)]
	      q_inner_just))
    in
    let q_outer_nothing = 
      ADag.mk_attach
	(pos, A.Nat 1n)
	(ADag.mk_attach
	   (A.Item 1, A.String "Nothing")
	   (ADag.mk_project
	      [prj iter; (A.Item 2, iter)]
	      empty_iterations))
    in
    let outer_cs = Cs.Tag ((1, `Tag), (2, `Surrogate)) in
    let ts_l' = slice_inner_tables q_inner_just ti_l.ts in
    let ti_inner_just =
      {
	q = q_inner_just;
	cs = ti_l.cs;
	ts = ts_l';
	vs = ti_l.vs
      }
    in	
    let vs = [((2, "Just"), (ti_inner_just, `Atom));
	      ((2, "Nothing"), (ti_inner_nothing, `Atom))] in
    let q_outer = ADag.mk_disjunion q_outer_just q_outer_nothing in
    {
      q = q_outer;
      cs = outer_cs;
      ts = Ts.empty;
      vs = vs
    }
      
  and compile_comparison env loop comparison_wrapper tablefun rowfun operand_1 operand_2 =
    let e1_ti = compile_expression env loop operand_1 in
    let e2_ti = compile_expression env loop operand_2 in
    let is_boxed_list ti = Cs.is_boxed_list ti.cs in
    let unbox ti =
      assert (Cs.is_atomic ti.cs);
      assert ((Ts.length ti.ts) = 1);
      let (offset, inner_ti) = List.hd ti.ts in
      do_unbox ti.q offset inner_ti
    in
    match (Query2.Annotate.typeof_typed_t operand_1, Query2.Annotate.typeof_typed_t operand_2) with
      (* if arguments are boxed (i.e. they have list type), we need
	 to unbox them first *)
      | `Atom, `Atom when (is_boxed_list e1_ti) && (is_boxed_list e2_ti) ->
	tablefun loop comparison_wrapper (unbox e1_ti) (unbox e2_ti)
      | `Atom, `List when is_boxed_list e1_ti ->
	tablefun loop comparison_wrapper (unbox e1_ti) e2_ti
      | `List, `Atom when is_boxed_list e2_ti ->
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
      let cs1 = Cs.lookup_record_field ti.cs "1" in
      let cs2 = Cs.lookup_record_field ti.cs "2" in
      let cs' = Cs.Mapping [("1", cs2); ("2", cs1)] in
      { ti with cs = cs' }
    in

  (* returns the minimal pos so that l1[pos] < l2[pos] *)
    let minpos zipped = 
    (* the comparison must be done loop-lifted so that inner tables can be unboxed and compared correctly *)

    (* lift zipped *)
      let _, q_mapped, map, loop' = map_forward zipped.q zipped.cs in
      let zipped_mapped = { zipped with q = q_mapped } in

    (* we need "<" on rows but have only ">" -> switch arguments *)
      let compared = do_row_greater_real loop' wrapper (switch_zipped zipped_mapped) in

    (* unlift *)
      let compared_backmapped =
	ADag.mk_project
	  [(iter, outer); (pos, pos'); prj (A.Item 1)]
	  (ADag.mk_eqjoin
	     (iter, inner)
	     compared
	     map)
      in

      let selected = ADag.mk_select (A.Item 1) compared_backmapped in

      let q = 
	ADag.mk_disjunion
	  (ADag.mk_attach
	     (pos, A.Nat 1n)
	     (ADag.mk_project
		[prj iter; (A.Item 1, res)]
		(ADag.mk_funaggr
		   (A.Min, (res, pos), Some iter)
		   selected)))
	  (ADag.mk_attach
	     (pos, A.Nat 1n)
	     (ADag.mk_attach
		(A.Item 1, A.Nat Nativeint.max_int)
		(ADag.mk_difference
		   loop
		   (ADag.mk_project
		      [prj iter]
		      selected))))
      in
      { 
	q = q;
	cs = Cs.Column (1, `NatType);
	ts = Ts.empty;
	vs = Vs.empty
      }
	  
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
    { 
      q = q;
      cs = Cs.Column (1, `BoolType);
      ts = Ts.empty;
      vs = Vs.empty
    }

  and do_row_greater_real loop wrapper zipped =

    let column_greater ti_zipped (cse_l, cse_r) =
      let q = 
	if Cs.is_atomic cse_l then
	  let col_l = List.hd (Cs.offsets cse_l) in
	  let col_r = List.hd (Cs.offsets cse_r) in
	  ADag.mk_project
	    [prj iter; prj pos; (A.Item 1, res)]
	    (* no need to join since the two arguments are already zipped *)
	    (wrap_gt res (A.Item col_l) (A.Item col_r) ti_zipped.q)
	else if Cs.is_variant cse_l then
	  failwith "comparison (<, >, <=, >=) of variants is not supported"
	else
	  let col_l = List.hd (Cs.offsets cse_l) in
	  let col_r = List.hd (Cs.offsets cse_r) in
	(* inner tables need to be unboxed first *)
	  let inner_table_l, inner_table_r =
	    try
	      Ts.lookup col_l ti_zipped.ts, Ts.lookup col_r ti_zipped.ts
	    with _ -> assert false
	  in
	  let ti_unboxed_l = do_unbox ti_zipped.q col_l inner_table_l in
	  let ti_unboxed_r = do_unbox ti_zipped.q col_r inner_table_r in
	  (do_table_greater loop wrapper ti_unboxed_l ti_unboxed_r).q
	    
      in
      {
	q = q;
	cs = Cs.Column (1, `BoolType);
	ts = Ts.empty;
	vs = Vs.empty
      }
    in

    let column_equal ti_zipped (cse_l, cse_r) = 
      let q = 
	if Cs.is_atomic cse_l then
	  let col_l = List.hd (Cs.offsets cse_l) in
	  let col_r = List.hd (Cs.offsets cse_r) in
	  ADag.mk_project
	    [prj iter; prj pos; (A.Item 1, res)]
	    (* no need to join since the two arguments are already zipped *)
	    (wrap_eq res (A.Item col_l) (A.Item col_r) ti_zipped.q)
	else if Cs.is_variant cse_l then
	  failwith "comparison (<, >, <=, >=) of variants is not supported"
	else
	  let col_l = List.hd (Cs.offsets cse_l) in
	  let col_r = List.hd (Cs.offsets cse_r) in
	(* we compare nested lists represented by a inner table *)

	(* lookup the inner tables referred to by col1, col2 *)
	  let inner_table_l, inner_table_r = 
	    try
	      Ts.lookup col_l ti_zipped.ts, Ts.lookup col_r ti_zipped.ts 
	    with _ -> assert false
	  in
	(* unbox the inner tables *)
	  let ti_unboxed_l = do_unbox ti_zipped.q col_l inner_table_l in
	  let ti_unboxed_r = do_unbox ti_zipped.q col_r inner_table_r in
	  (* compare the inner tables *)
	  (do_table_equal loop wrapper ti_unboxed_l ti_unboxed_r).q
      in
      {
	q = q;
	cs = Cs.Column (1, `BoolType);
	ts = Ts.empty;
	vs = Vs.empty
      }
    in

    let cs_l = Cs.lookup_record_field zipped.cs "1" in
    let cs_r = Cs.lookup_record_field zipped.cs "2" in

  (* special case: if we are comparing lists of records and one of the lists is the empty 
     list, the length of its cs component does not match the other cs's length.  in this case, 
     we need to "fake" a compatible cs for the empty list *)
    let cs_l, cs_r = 
      match Cs.is_empty_list_lit cs_l, Cs.is_empty_list_lit cs_r with
	| true, true
	| false, false -> cs_l, cs_r
	| true, false -> cs_r, cs_r
	| false, true -> cs_l, cs_l
    in
    
    let cols_l = Cs.leafs cs_l in
    let cols_r = Cs.leafs cs_r in

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
    (List.fold_left or_op (List.hd greater_terms) and_terms).q
      
  and do_table_equal loop wrapper l1 l2 =
    let all = do_list_and loop in

    let map_equal source =
      let _, q_s_mapped, map, loop = map_forward source.q source.cs in
      let ti_s = { source with q = q_s_mapped } in
      let q_equal = (do_row_equal loop wrapper (do_project "1" ti_s) (do_project "2" ti_s)).q in
      (* map the comparison result back into the outer iteration context *)
      let result_backmapped =
	ADag.mk_project
	  [(iter, outer); (pos, pos'); prj (A.Item 1)]
	  (ADag.mk_eqjoin
	     (iter, inner)
	     q_equal
	     map)
      in
      {
	q = result_backmapped;
	cs = Cs.Column (1, `BoolType);
	ts = Ts.empty;
	vs = Vs.empty
      }
    in

    let l1_abs = abspos_ti l1 in
    let l2_abs = abspos_ti l2 in
    let l1_len = do_length loop l1_abs in
    let l2_len = do_length loop l2_abs in
    and_op 
      (equal l1_len l2_len)
      (all (map_equal (do_zip l1_abs l2_abs)))

  and do_row_equal loop wrapper ti_l ti_r =

  (* special case: if we are comparing lists of records and one of the lists is the empty 
     list, the length of its cs component does not match the other cs's length.  in this case, 
     we need to "fake" a compatible cs for the empty list *)
    let cs = Cs.choose_nonempty ti_l.cs ti_r.cs in

    let fields = Cs.leafs (Cs.sort_record_columns cs) in

  (* compare the columns for the first field. the result is then the conjuntion of
     this result and the result of the (recursive) comparison of the remaining fields *)

    let compare_field field_cse =
      let project q col = 
	ADag.mk_project
	  [prj iter; prj pos; (A.Item 1, A.Item col)]
	  q
      in
      if Cs.is_atomic field_cse then
	(* normal comparison of atomic values *)
	let col = List.hd (Cs.offsets field_cse) in
	do_primitive_binop wrapper (project ti_l.q col) (project ti_r.q col)
      else if Cs.is_variant field_cse then

	let tagcol, refcol = 
	  match Cs.offsets field_cse with 
	    | [tagcol; refcol] -> tagcol, refcol
	    | _ -> assert false
	in
	(* compare tags *)
	let tags_compared =
	  ADag.mk_funnumeq
	    (res, (item', item''))
	    (ADag.mk_eqjoin
	       (iter, iter')
	       (ADag.mk_project
		  [(prj iter); (item', A.Item tagcol)]
		  ti_l.q)
	       (ADag.mk_project
		  [(iter', iter); (item'', A.Item tagcol)]
		  ti_r.q))
	in

	(* select those iterations in which the tags are not
	   equal. the result for these iterations is false regardless of
	   the tagged values *)
	let different_tags =
	  ADag.mk_attach
	    (A.Item 1, A.Bool false)
	    (ADag.mk_attach
	       (pos, A.Nat 1n)
	       (ADag.mk_project
		  [prj iter]
		  (ADag.mk_select
		     res'
		     (ADag.mk_funboolnot
			(res', res)
			tags_compared))))
	in

	(* iterations in which the tag is the same *)
	let same_tag =
	  ADag.mk_project
	    [prj iter; prj item']
	    (ADag.mk_select 
	       res 
	       tags_compared)
	in
	
	(* select those iterations from q in which tag1 = tag2 = tag *)
	let select_tag q tag = 
	  ADag.mk_eqjoin
	    (iter, iter')
	    q
	    (ADag.mk_project
	       [(iter', iter)]
	       (ADag.mk_select
		  res
		  (ADag.mk_funnumeq
		     (res, (item', item''))
		     (ADag.mk_attach
			(item'', A.String tag)
			same_tag))))
	in

	(* compare the tagged values in iterations having the same tag
	   _per tag_. for each tag which occurs in vs_left and
	   vs_right, select the iterations which both have this tag,
	   unbox the tagged values and compare them*)
	let matching_tis_compared =
	  List.map
	    (fun ((_refcol, tag), ((inner_ti_l, itype_l), (inner_ti_r, _))) -> 
	      (* select only the current tag *)
	      
	      let q_tag_l = select_tag ti_l.q tag in
	      let q_tag_r = select_tag ti_r.q tag in
	      let unboxed_l = do_unbox q_tag_l refcol inner_ti_l in
	      let unboxed_r = do_unbox q_tag_r refcol inner_ti_r in
	      let loop_tag = ADag.mk_project [prj iter] q_tag_l in
	      match itype_l with
		| `Atom ->
		  do_row_equal loop_tag wrapper unboxed_l unboxed_r
		| `List ->
		  do_table_equal loop_tag wrapper unboxed_l unboxed_r)
	    (same_keys (Vs.lookup_col refcol ti_l.vs) (Vs.lookup_col refcol ti_r.vs))
	in

	(* union of the results for all tags *)
	List.fold_left
	  (fun q_union ti -> ADag.mk_disjunion q_union ti.q)
	  different_tags
	  matching_tis_compared
	  
      else
	(* we compare nested lists represented by a inner table *)
	let col = List.hd (Cs.offsets field_cse) in

	(* lookup the inner tables referred to by col1, col2 *)
	let inner_table_l, inner_table_r = 
	  try
	    Ts.lookup col ti_l.ts, Ts.lookup col ti_r.ts
	  with _ -> assert false
	in
	(* unbox the inner tables *)
	let ti_unboxed_l = do_unbox (project ti_l.q col) col inner_table_l in
	let ti_unboxed_r = do_unbox (project ti_r.q col) col inner_table_r in
	  (* compare the inner tables *)
	(do_table_equal loop wrapper ti_unboxed_l ti_unboxed_r).q
    in

    let q = 
      List.fold_left
	(fun q field ->
	  ADag.mk_project
	    [prj iter; prj pos; (A.Item 1, res)]
	    (ADag.mk_funbooland
	       (res, (A.Item 1, A.Item 2))
	       (ADag.mk_eqjoin
		  (iter', iter)
		  (ADag.mk_project
		     [(iter', iter); (A.Item 2, A.Item 1)]
		     (compare_field field))
		  q)))
	(compare_field (List.hd fields))
	(drop 1 fields)
    in
    {
      q = q;
      cs = Cs.Column (1, `BoolType);
      ts = Ts.empty;
      vs = Vs.empty
    }

  and compile_binop env loop wrapper restype operand_1 operand_2 =
    let ti_1 = compile_expression env loop operand_1 in
    let ti_2 = compile_expression env loop operand_2 in
    do_primitive_binop_ti wrapper restype ti_1 ti_2

  and compile_unop env loop wrapper operand =
    let ti_operand = compile_expression env loop operand in
    assert (Cs.is_atomic ti_operand.cs);
    let c = A.Item 1 in
    let res = A.Item 2 in
    let q = 
      ADag.mk_project
	[prj iter; prj pos; (c, res)]
	(wrapper
	   res c
	   ti_operand.q)
    in
    {
      q = q;
      cs = ti_operand.cs;
      ts = Ts.empty;
      vs = Vs.empty
    }

  and compile_concat env loop l =
    let ti_l = compile_expression env loop l in
    assert((List.length ti_l.ts) = 1);
    let ti_sub = Ts.lookup 1 ti_l.ts in
    let c = A.Item 1 in
    let q =
      ADag.mk_project
	([(iter, iter'); (pos, pos'')] @ (prjlist (io (Cs.offsets ti_sub.cs))))
	(ADag.mk_rank
	   (pos'', [(pos', A.Ascending); (pos, A.Ascending)])
	   (ADag.mk_eqjoin
	      (c', iter)
	      (ADag.mk_project
		 [(iter', iter); (pos', pos); (c', c)]
		 ti_l.q)
	      ti_sub.q))
    in
    {
      q = q;
      cs = ti_sub.cs;
      ts = ti_sub.ts;
      vs = ti_sub.vs
    }

  and compile_take env loop n l =
    let ti_n = compile_expression env loop n in
    let ti_l = compile_expression env loop l in
    let cols = (io (Cs.offsets ti_l.cs)) in
    let q_l' = abspos ti_l.q ti_l.cs in
    let c = A.Item 1 in
    let one = A.Item 2 in
    let q' = 
      ADag.mk_project
	([prj iter; prj pos] @ (prjlist cols))
	(ADag.mk_select
	   res
	   (ADag.mk_funnumgt
	      (res, (c', pos'))
	      (ADag.mk_eqjoin
		 (iter, iter')
		 (ADag.mk_cast
		    (pos', pos, `IntType)
		    q_l')
		 (ADag.mk_project
		    [(iter', iter); (c', res)]
		    (ADag.mk_fun1to1
		       (A.Add, res, [c; one])
		       (ADag.mk_attach
			  (one, A.Int (Num.Int 1))
			  ti_n.q))))))
    in
    let ts' = slice_inner_tables q' ti_l.ts in
    {
      q = q';
      cs = ti_l.cs;
      ts = ts';
      vs = ti_l.vs
    }

  and compile_drop env loop n l =
    let ti_n = compile_expression env loop n in
    let ti_l = compile_expression env loop l in
    let cols = (io (Cs.offsets ti_l.cs)) in
    let q_l_abs = abspos ti_l.q ti_l.cs in
    let c = A.Item 1 in
    let q' =
      ADag.mk_project
	([prj iter; prj pos] @ (prjlist cols))
	(ADag.mk_select
	   res
	   (ADag.mk_funnumgt
	      (res, (pos', c'))
	      (ADag.mk_eqjoin
		 (iter, iter')
		 (ADag.mk_cast
		    (pos', pos, `IntType)
		    q_l_abs)
		 (ADag.mk_project
		    [(iter', iter); (c', c)]
		    ti_n.q))))
    in
    let ts' = slice_inner_tables q' ti_l.ts in
    {
      q = q';
      cs = ti_l.cs;
      ts = ts';
      vs = ti_l.vs
    }

  and compile_hd env loop l = 
    let ti_l = compile_expression env loop l in
    let q_l_abs = abspos ti_l.q ti_l.cs  in
    let q = 
      ADag.mk_project
	([prj iter; prj pos] @ (prjlist (io (Cs.offsets ti_l.cs))))
	(ADag.mk_select
	   res
	   (ADag.mk_funnumeq
	      (res, (pos, pos'))
	      (ADag.mk_attach
		 (pos', A.Nat 1n)
		 q_l_abs)))
    in
    let q_error =
      ADag.mk_project
	[prj (A.Item 1)]
	(ADag.mk_attach
	   (A.Item 1, A.String "hd() of empty list")
	   (ADag.mk_difference
	      loop
	      (ADag.mk_project
		 [prj iter]
		 q_l_abs)))
    in
    merge_error_plans q_error;
    (* FIXME: slice the inner tables *)
    {
      q = q;
      cs = ti_l.cs;
      ts = ti_l.ts;
      vs = ti_l.vs
    }

  and compile_tl env loop l = 
    let ti_l = compile_expression env loop l in
    let q_l_abs = abspos ti_l.q ti_l.cs in
    let q =
      ADag.mk_project
	([prj iter; prj pos] @ (prjlist (io (Cs.offsets ti_l.cs))))
	(ADag.mk_select
	   res
	   (ADag.mk_funnumgt
	      (res, (pos, item'))
	      (ADag.mk_attach
		 (item', A.Nat 1n)
		 q_l_abs)))
    in
    let q_error =
      ADag.mk_project
	[prj (A.Item 1)]
	(ADag.mk_attach
	   (A.Item 1, A.String "tl() of empty list")
	   (ADag.mk_difference
	      loop
	      (ADag.mk_project
		 [prj iter]
		 q_l_abs)))
    in
    merge_error_plans q_error;
    {
      q = q;
      cs = ti_l.cs;
      ts = ti_l.ts;
      vs = ti_l.vs
    }

  and compile_distinct env loop l =
    let ti_l = compile_expression env loop l in
      assert (not (Cs.is_variant ti_l.cs || Cs.is_boxed_list ti_l.cs));
      let items = io (Cs.offsets ti_l.cs) in
      let ranked = 
	ADag.mk_rowrank
	  (item', ((iter, A.Ascending) :: (List.map (fun i -> (i, A.Ascending)) items)))
	  ti_l.q
      in
      let q =
	ADag.mk_project
	  ([prj iter; prj pos] @ (prjlist items))
	  (ADag.mk_select
	     res
	     (ADag.mk_funnumeq
		(res, (pos, pos'))
		(ADag.mk_eqjoin
		   (iter, iter')
		   ti_l.q
		   (ADag.mk_eqjoin
		      (item', item'')
		      (ADag.mk_funaggr
			 (A.Min, (iter', iter), Some item')
			 ranked)
		      (ADag.mk_project
			 [prj pos'; (item'', item')]
			 (ADag.mk_funaggr
			    (A.Min, (pos', pos), Some item')
			    ranked))))))
      in
	{
	  q = q;
	  cs = ti_l.cs;
	  ts = Ts.empty;
	  vs = Vs.empty
	}

  and compile_quote env loop s =
  (* FIXME quoting at runtime is not implemented *)
    Debug.print "Warning: quoting at runtime is not implemented (compile_quite)";
    compile_expression env loop s

  and compile_apply env loop f args =
    match f, args with
      | "+", [op1; op2] -> compile_binop env loop (wrap_1to1 A.Add) `IntType op1 op2
      | "+.", [op1; op2] -> compile_binop env loop (wrap_1to1 A.Add) `FloatType op1 op2
      | "-", [op1; op2] -> compile_binop env loop (wrap_1to1 A.Subtract) `IntType op1 op2
      | "-.", [op1; op2] -> compile_binop env loop (wrap_1to1 A.Subtract) `FloatType op1 op2
      | "*", [op1; op2] -> compile_binop env loop (wrap_1to1 A.Multiply) `IntType op1 op2
      | "*.", [op1; op2] -> compile_binop env loop (wrap_1to1 A.Multiply) `FloatType op1 op2
      | "/", [op1; op2] -> compile_binop env loop (wrap_1to1 A.Divide) `IntType op1 op2
      | "/.", [op1; op2] -> compile_binop env loop (wrap_1to1 A.Divide) `FloatType op1 op2
      | "==", [op1; op2] -> compile_comparison env loop wrap_eq do_table_equal do_row_equal op1 op2
      | "<>", [op1; op2] -> compile_comparison env loop wrap_ne do_table_equal do_row_equal op1 op2
      | ">", [op1; op2] -> compile_comparison env loop wrap_gt do_table_greater do_row_greater op1 op2
      | "not", [op]->  compile_unop env loop wrap_not op
      | "nth", [i; l] -> compile_nth env loop i l
      | "length", [l] -> compile_length env loop l
      | "sum", [l] -> compile_sum env loop l
      | "max", [l] -> compile_aggr_error env loop A.Max `IntType l
      | "min", [l] -> compile_aggr_error env loop A.Min `IntType l
      | "avg", [l] -> compile_aggr_error env loop A.Avg `FloatType l
      | "take", [n; l] -> compile_take env loop n l
      | "drop", [n; l] -> compile_drop env loop n l
      | "zip", [l1; l2] -> compile_zip env loop l1 l2
      | "unzip", [p] -> compile_unzip env loop p
      | "concat", [l] -> compile_concat env loop l
      | "and", [l] -> compile_and env loop l
      | "or", [l] -> compile_or env loop l
      | "empty", [l] -> compile_empty env loop l
      | "hd", [l] -> compile_hd env loop l
      | "tl", [l] -> compile_tl env loop l
      | "tilde", [s; p] -> compile_binop env loop (wrap_1to1 A.SimilarTo) `BoolType s p
      | "quote", [s] -> compile_quote env loop s
      | "^^", [op1; op2] -> compile_binop env loop (wrap_1to1 A.Concat) `StrType op1 op2
      | "distinct", [l] -> compile_distinct env loop l
    (*    | "takeWhile" -> compile_takeWhile env loop args
	  | "dropWhile" -> compile_dropWhile env loop args *)
      | "<", _ | "<=", _ | ">=", _->
	failwith ("CompileQuery.compile_apply: </<=/>= should have been rewritten in query2")
      | s, _->
	failwith ("CompileQuery.compile_apply: " ^ s ^ " not implemented")

  and compile_for env loop v source body order_criteria =
    let  ti_source = compile_expression env loop source in
    let _, q_v, map, loop_v = map_forward ti_source.q ti_source.cs in
    let env = AEnv.map (lift map) env in
    let env_v = AEnv.bind env (v, { ti_source with q = q_v }) in
    let ti_body = compile_expression env_v loop_v body in
    let (sort_cols, sort_info, map') =
      match order_criteria with
	| _ :: _ ->
	  (* compile orderby expressions *)
	  let q_os = List.map (compile_expression env_v loop_v) order_criteria in
	  let q_os = List.map (fun ti -> ti.q) q_os in
	  let offset = (List.length (Cs.offsets ti_body.cs)) + 1 in
	  let cols = mapIndex (fun _ i -> A.Item (i + offset)) q_os in
	  let order_cols = List.map (fun c -> (c, A.Ascending)) (cols @ [pos]) in
	    cols, order_cols, omap map q_os cols
	| [] ->
	  ([], [(iter, A.Ascending); (pos, A.Ascending)], map)
    in
    let q = 
      ADag.mk_project
	([(iter, outer); (pos, pos')] @ (prjlist (io (Cs.offsets ti_body.cs))))
	(ADag.mk_rank
	   (pos', sort_info)
	   (ADag.mk_eqjoin
	      (iter, inner)
	      ti_body.q
	      (ADag.mk_project
		 ([prj outer; prj inner] @ (prjlist sort_cols))
		 map')))
    in
    {
      q = q;
      cs = ti_body.cs;
      ts = ti_body.ts;
      vs = ti_body.vs
    }

  and singleton_record env loop (name, e) =
    let ti = compile_expression env loop e in
    let cs' = Cs.Mapping [(name, ti.cs)] in
    { ti with cs = cs' }

  and extend_record env loop ext_fields r =
    assert (match ext_fields with [] -> false | _ -> true);
    let ti = 
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
    in
    (* guarantee invariant: cs fields are sorted in increasing order *)
    let cols_old = Cs.offsets ti.cs in
    let cs_sorted = Cs.sort_record_columns ti.cs in
    let cols_sorted = Cs.offsets cs_sorted in
    if cols_old = cols_sorted then
	(* columns were already in order *)
      ti
    else
      let cols_new = fromTo 1 (1 + (Cs.cardinality ti.cs)) in
      let cs_mapped = Cs.map_cols cols_new cs_sorted in
	(* change column order by projecting *)
      let q' =
	ADag.mk_project
	  ([prj iter; prj pos] @ (prjlist_map (io cols_new) (io cols_sorted)))
	  ti.q
      in
      (* change the offsets in the ts mappings accordingly *)
      let col_mapping = List.combine cols_old cols_new in
      let ts' =
	List.map
	  (fun (col, itbl) -> 
	    let new_col = 
	      try 
		List.assoc col col_mapping 
	      with _ -> assert false
	    in
	    (new_col, itbl))
	  ti.ts
      in
      let vs' =
	List.map
	  (fun ((col, tag), itbl) ->
	    let new_col =
	      try
		List.assoc col col_mapping
	      with _ -> assert false
	    in
	    ((new_col, tag), itbl))
	  ti.vs
      in
      {
	q = q';
	cs = cs_mapped;
	ts = ts';
	vs = vs'
      }

  and merge_records ti_r1 ti_r2 =
    let r2_cols = Cs.offsets ti_r2.cs in
    let new_names_r2 = io (incr r2_cols (Cs.cardinality ti_r1.cs)) in
    let old_names_r2 = io r2_cols in
    let names_r1 = io (Cs.offsets ti_r1.cs) in
    let card_r1 = Cs.cardinality ti_r1.cs in
    let r2_ts' = Ts.incr_cols ti_r2.ts card_r1 in
    let r2_vs' = Vs.incr_cols ti_r2.vs card_r1 in
    let q =
      ADag.mk_project
	(prjlist ([A.Iter 0; A.Pos 0] @ names_r1 @ new_names_r2))
	(ADag.mk_eqjoin
	   (iter, iter')
	   ti_r1.q
	   ((ADag.mk_project
	       ((iter', iter) :: (prjlist_map new_names_r2 old_names_r2))
	       ti_r2.q)))
    in
    let cs = Cs.append_mappings ti_r1.cs ti_r2.cs in
    let ts = Ts.append ti_r1.ts r2_ts' in
    let vs = Vs.append ti_r1.vs r2_vs' in
    {
      q = q;
      cs = cs;
      ts = ts;
      vs = vs
    }

  and compile_project env loop field record =
    let record_ti = compile_expression env loop record in
    do_project field record_ti

  and compile_erase env loop erase_fields r =
    let ti_r = compile_expression env loop r in
    let remaining_cs = Cs.filter_record_fields ti_r.cs erase_fields in
    let remaining_cols = Cs.offsets remaining_cs in
    let remaining_ts = Ts.keep_cols ti_r.ts remaining_cols in
    let remaining_vs = Vs.keep_cols ti_r.vs remaining_cols in
    let q =
      ADag.mk_project
	([prj iter; prj pos] @ (prjlist (io remaining_cols)))
	ti_r.q
    in
    {
      q = q;
      cs = remaining_cs;
      ts = remaining_ts;
      vs = remaining_vs
    }

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
  (* collect the column names of the table and their types from the row type *)
    let cs_ts = 
      StringMap.fold
	(fun colname (_, typ) cs_ts -> (colname, (pf_type_of_typ typ)) :: cs_ts)
	(fst (fst (Types.unwrap_row row)))
	[]
    in
  (* sort them by column name to get the canonical record layout 
     (invariant: cs is always sorted by field name) *)
    let cs_ts_sorted = List.sort (fun a b -> compare (fst a) (fst b)) cs_ts in
    let column_names, types = List.split cs_ts_sorted in
  (* column names to column numbers *)
    let column_items = mapIndex (fun c i -> (c, A.Item (i + 1))) column_names in
  (* lookup the column numbers corresponding to the key columns *)
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
      let fields = 
	List.map (fun (i, c, typ) -> (c, Cs.Column (offset i, typ))) attr_infos 
      in
      Cs.Mapping fields
    in
    let q =
      ADag.mk_cross
	loop
	(ADag.mk_rank
	   (pos, (List.map (fun column -> (column, A.Ascending)) (List.hd key_items)))
	   (ADag.mk_tblref
	      (tblname, attr_infos, key_items)))
    in
    {
      q = q;
      cs = cs;
      ts = Ts.empty;
      vs = Vs.empty
    }

  and compile_constant loop (c : Constant.constant) =
    let cs = Cs.Column (1, Cs.column_type_of_constant c) in
    let q =
      ADag.mk_attach
	(A.Item 1, A.pf_constant_of_constant c)
	(ADag.mk_attach
	   (A.Pos 0, A.Nat 1n)
	   loop)
    in
    {
      q = q;
      cs = cs;
      ts = Ts.empty;
      vs = Vs.empty
    }


(* if e1 then e2 else []:
   don't consider the else branch if it represents the empty list. *)
  and compile_if2 env loop c t =
  (* condition *)
    let ti_c = compile_expression env loop c in
    assert (Cs.is_atomic ti_c.cs);
    let loop_then =
      ADag.mk_project
	[prj iter]
	(ADag.mk_select
	   (A.Item 1)
	   ti_c.q)
    in
    let env_then = fragment_env loop_then env in
    compile_expression env_then loop_then t

  and compile_if env loop c t e =
  (* condition *)
    let ti_c = compile_expression env loop c in
    assert (Cs.is_atomic ti_c.cs);
    let loop_then =
      ADag.mk_project
	[prj iter]
	(ADag.mk_select
	   (A.Item 1)
	   ti_c.q)
    in
    let loop_else =
      ADag.mk_project
	[prj iter]
	(ADag.mk_select
	   res
	   (ADag.mk_funboolnot
	      (res, A.Item 1)
	      ti_c.q))
    in
    let env_then = fragment_env loop_then env in
    let env_else = fragment_env loop_else env in
    let ti_t = compile_expression env_then loop_then t in
    let ti_e= compile_expression env_else loop_else e in
    let q =
      ADag.mk_rownum
	(item', [(iter, A.Ascending); (ord, A.Ascending); (pos, A.Ascending)], None)
	(ADag.mk_disjunion
	   (ADag.mk_attach
	      (ord, A.Nat 1n)
	      ti_t.q)
	   (ADag.mk_attach
	      (ord, A.Nat 2n)
	      ti_e.q))
    in
    (* FIXME: need to check if the then branch is a empty list literal *)
    let cols = Cs.offsets ti_t.cs in
    (* FIXME: use refresh_surr_cols *)
    let keys = (Ts.keys ti_t.ts) @ (Vs.key_columns ti_t.vs) in
    let proj = [prj iter; prj pos] in
    let proj = proj @ (prjlist (io (difference cols keys))) in
    let proj = proj @ (prjlist_single (io keys) item') in
    let q' = 
      ADag.mk_project
	proj
	q
    in
    {
      q = q';
      cs = ti_t.cs;
      ts = append_ts q ti_t.ts ti_e.ts;
      vs = append_vs q ti_t.vs ti_e.vs
    }

  and compile_groupby env loop v ge e =
    let ti_e = compile_expression env loop e in
    let q_v, q_v', map_v, loop_v = map_forward ti_e.q ti_e.cs in
    let env_v = AEnv.map (lift map_v) env in
    let env_v = AEnv.bind env_v (v, { ti_e with q = q_v' }) in
    (* compile group expression *)
    let ti_ge = compile_expression env_v loop_v ge in
    let cs_ge' = Cs.shift (Cs.cardinality ti_e.cs) ti_ge.cs in
    let sortlist = List.map (fun c -> (A.Item c, A.Ascending)) (Cs.offsets cs_ge') in
    let q_1 =
      ADag.mk_rowrank
	(grp_key, (iter, A.Ascending) :: sortlist)
	(ADag.mk_eqjoin
	   (inner, iter')
	   q_v
	   (ADag.mk_project
	      ((iter', iter) :: (prjlist_map (io (Cs.offsets cs_ge')) (io (Cs.offsets ti_ge.cs))))
	      ti_ge.q))
    in
    let grpkey_col = (Cs.cardinality ti_ge.cs) + 1 in
    let q_2 =
      ADag.mk_distinct
	(ADag.mk_project
	   ([prj iter; (pos, grp_key); (A.Item grpkey_col, grp_key)] @ (prjlist_map (io (Cs.offsets ti_ge.cs)) (io (Cs.offsets cs_ge'))))
	   q_1)
    in
    let q_3 =
      ADag.mk_project
	([(iter, grp_key); (prj pos)] @ (prjlist (io (Cs.offsets ti_e.cs))))
	q_1
    in
    {
      q = q_2;
      cs = Cs.Mapping [("1", ti_ge.cs); ("2", Cs.Column (grpkey_col, `Surrogate))];
      ts = [(grpkey_col, { ti_e with q = q_3 })];
      vs = Vs.empty
    }

  and compile_unit (loop : ADag.t) : tblinfo =
    let q =
      ADag.mk_attach
	(A.Item 1, A.Nat 1n)
	(ADag.mk_attach
	   (pos, A.Nat 1n)
	   loop)
    in
    {
      q = q;
      cs = Cs.Column (1, `Unit);
      ts = Ts.empty;
      vs = Vs.empty
    }

  and compile_variant env loop tag value =
    Debug.f "compile_variant %s" tag;
    let ti_value = compile_expression env loop value in
    let itype = Query2.Annotate.typeof_typed_t value in
    let q = 
      ADag.mk_attach
	(pos, A.Nat 1n)
	(ADag.mk_attach
	   (A.Item 1, A.String tag)
	   (ADag.mk_project
	      [prj iter; (A.Item 2, iter)]
	      loop))
    in
    {
      q = q;
      cs = Cs.Tag ((1, `Tag), (2, `Surrogate));
      ts = Ts.empty;
      vs = [(2, tag), (ti_value, itype)]
    }

  and compile_case env loop value cases default =

    let select_tag q tag =
      let q_compared = 
	ADag.mk_funnumeq
	  (res, (A.Item 1, item'))
	  (ADag.mk_attach
	     (item', A.String tag)
	     q)
      in
    (* all iterations which have the tag *)
      let q_matching = 	
	ADag.mk_project
	  [prj iter; prj pos; prj (A.Item 2)]
	  (ADag.mk_select
	     res
	     q_compared)
      in

    (* all iterations which do not have this tag *)
      let q_other =
	ADag.mk_project
	  [prj iter; prj pos; prj (A.Item 1); prj (A.Item 2)]
	  (ADag.mk_select
	     res'
	     (ADag.mk_funboolnot
		(res', res)
		q_compared))
      in
      q_matching, q_other

    in
    
  (* compile value to be matched *)
    let ti_v = compile_expression env loop value in

    let _, q_v', map, _loop_v = map_forward ti_v.q ti_v.cs in

    let env' = AEnv.map (lift map) env in

    let case env vs_v tag (var, case_exp) (results, q_other) =
      let q_matching, q_other' = select_tag q_other tag in
      try 
	let itbl = fst (Vs.lookup (2, tag) vs_v) in
	let ti_unboxed = do_unbox q_matching 2 itbl in
	let env' = AEnv.bind env (var, ti_unboxed) in
	let loop' = ADag.mk_project [prj iter] q_matching in
	let env' = fragment_env loop' env' in
	let case_result = compile_expression env' loop' case_exp in
	(case_result :: results), q_other'
      with NotFound _ -> 
	(results, q_other')
    in

    let default_case env q_other (default_var, default_exp) =
      let loop' = ADag.mk_project [prj iter] q_other in
      let env' = AEnv.bind env (default_var, (compile_unit loop)) in
      let env' = fragment_env loop' env' in
      compile_expression env' loop default_exp
    in

    let explicit_case_results, q_other = StringMap.fold (case env' ti_v.vs) cases ([], q_v') in

    let all_results = 
      match default with
	| Some c -> (default_case env' q_other c) :: explicit_case_results
	| None -> explicit_case_results
    in
    
    let union ti_l ti_r =
      let q_union = 
	ADag.mk_rownum
	  (item', [(iter, A.Ascending); (pos, A.Ascending); (ord, A.Ascending)], None)
	  (ADag.mk_disjunion 
	     (ADag.mk_attach
		(ord, A.Nat 1n)
		ti_l.q) 
	     (ADag.mk_attach
		(ord, A.Nat 2n)
		ti_r.q))
      in
      let cs = Cs.choose_nonempty ti_l.cs ti_r.cs in
      let q_union' = 
	ADag.mk_project
	  ([prj iter; prj pos] @ (refresh_surr_cols cs ti_l.ts ti_r.ts ti_l.vs ti_r.vs item'))
	  q_union
      in
      {
	q = q_union';
	cs = cs;
	ts = append_ts q_union ti_l.ts ti_r.ts;
	vs = append_vs q_union ti_l.vs ti_r.vs;
      }
	
    in
    List.fold_left union (List.hd all_results) (drop 1 all_results) 

  and compile_wrong loop =
    let q_error = 
      ADag.mk_project
	[prj (A.Item 1)]
	(ADag.mk_attach
 	   (A.Item 1, A.String "something is wrong")
 	   loop)
    in
    merge_error_plans q_error;
    {
      q = ADag.mk_emptytbl;
      cs = Cs.Column (1, `IntType);
      ts = Ts.empty;
      vs = Vs.empty
    }

  and compile_expression env loop e : tblinfo =
    match e with
      | `Constant (c, _) -> compile_constant loop c
      | `Apply ((f, args), _) -> compile_apply env loop f args 
      | `Var (x, _) -> AEnv.lookup env x
      | `Project ((r, field), _) -> compile_project env loop field r
      | `Record (r, _) -> compile_record env loop (StringMap.to_alist r)
      | `Extend ((None, empty), _) when (StringMap.size empty) = 0-> compile_unit loop 
      | `Extend ((r, ext_fields), _) ->
	let ext_fields = StringMap.to_alist ext_fields in
	extend_record env loop ext_fields (opt_map (compile_expression env loop) r)
      | `Erase ((r, erase_fields), _) -> compile_erase env loop erase_fields r
      | `Singleton (e, _) -> compile_expression env loop e
      | `Append (l, _) -> compile_append env loop l
      | `Table (t, _) -> compile_table loop t
      | `If ((c, t, Some e), _) -> compile_if env loop c t e
      | `If ((c, t, None), _) -> compile_if2 env loop c t
      | `For (((x, l), os, body), _) -> compile_for env loop x l body os
      | `Box (e, _) -> compile_box env loop e
      | `Unbox (e, _) -> compile_unbox env loop e
      | `GroupBy (((x, group_exp), source), _) -> compile_groupby env loop x group_exp source 
      | `Variant ((tag, value), _) -> compile_variant env loop tag value
      | `Case ((v, cases, default), _) -> compile_case env loop v cases default
      | `Wrong _  -> compile_wrong loop 
      | `XML _ -> failwith "compile_expression: not implemented"
      | `Primitive _ -> failwith "compile_expression: eval error"

  let rec wrap_serialize ti = 
    let serialize q cs =
      ADag.mk_serializerel 
	(iter, pos, io (Cs.offsets cs))
	(ADag.mk_nil)
	q
    in
    {
      q = serialize ti.q ti.cs;
      cs = ti.cs;
      ts = alistmap wrap_serialize ti.ts;
      vs = alistmap (fun (ti, itype) ->  (wrap_serialize ti, itype)) ti.vs;
    }

  let wrap_serialize_errors q_error =
    let wrap q = 
      ADag.mk_serializerel 
	(iter, pos, [A.Item 1])
	ADag.mk_nil
	(ADag.mk_attach
	   (pos, A.Nat 1n)
	   (ADag.mk_rank
	      (iter, [(A.Item 1, A.Ascending)])
	      q))
    in
    opt_map wrap q_error

  let compile e =
    let loop = 
      (ADag.mk_littbl
	 ([[A.Nat 1n]], [(A.Iter 0, `NatType)]))
    in
    let ti = compile_expression AEnv.empty loop e in
    wrap_serialize ti, wrap_serialize_errors !errors

end    

let compile = ExpressionToAlgebra.compile

