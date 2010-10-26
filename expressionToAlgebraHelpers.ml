open Utility
open ExpressionToAlgebraDefinitions
(*open ExpressionToAlgebraDefinitions.Ti *)
open Components

module A = Algebra
module ADag = Algebra_dag

let pf_type_of_typ t = 
  let concrete_t = Types.concrete_type t in
    match concrete_t with
      | `Primitive `Bool -> `BoolType
      | `Primitive `Int -> `IntType
      | `Primitive `Char -> `CharType
      | `Primitive `Float -> `FloatType
      | `Primitive `String -> `StrType
      | _ -> failwith ("unsupported type " ^ (Types.string_of_datatype concrete_t))

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
    vs = Vs.empty;
    fs = Fs.empty
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
      vs = inner_ti.vs;
      fs = inner_ti.fs;
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
      fs = Fs.decr_cols (Fs.keep_cols record.fs old_cols) (offset - 1);
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
      vs = Vs.empty;
      fs = Fs.empty
    }

(* q_e1 and q_e2 must have absolute positions *)
let do_zip e1 e2 =
  let card_e1 = List.length (Cs.offsets e1.cs) in
  let cs_e2' = Cs.shift card_e1 e2.cs in
  let ts_e2' = Ts.incr_cols e2.ts card_e1 in
  let vs_e2' = Vs.incr_cols e2.vs card_e1 in
  let fs_e2' = Fs.incr_cols e2.fs card_e1 in
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
      fs = Fs.append e1.fs fs_e2';
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
    { 
      q = q'';
      cs = Cs.Column (1, `BoolType); 
      ts = Ts.empty; 
      vs = Vs.empty;
      fs = Fs.empty
    }

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
    { 
      q = q''; 
      cs = Cs.Column (1, `BoolType); 
      ts = Ts.empty; 
      vs = Vs.empty;
      fs = Fs.empty
    }

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
let refresh_surr_cols cs ti_l ti_r new_surr =

  let int_union l r = IntSet.elements (IntSet.union (IntSet.from_list l) (IntSet.from_list r)) in
  let ts_cols = int_union (Ts.columns ti_l.ts) (Ts.columns ti_r.ts) in
  let fs_cols = int_union (Fs.columns ti_l.fs) (Fs.columns ti_r.fs) in
  let vs_cols = int_union (Vs.columns ti_l.vs) (Vs.columns ti_r.vs) in

    (* all columns which are neither vs nor ts surrogate columns *)
    prjlist (io (difference (Cs.offsets cs) (vs_cols @ ts_cols @ fs_cols)))

    (* use new keys in vs surrogate columns *)
    @ (prjlist_single (io vs_cols) new_surr) 

    (* use new keys in ts surrogate columns *)
    @ (prjlist_single (io ts_cols) new_surr)

    (* use new keys in fs surrogate columns *)
    @ (prjlist_single (io fs_cols) new_surr)

(* FIXME: need comments on append_ crap *)

(* append the fundev-lists of two fs components *)
let append_fs q_outer fs_l fs_r =

  (* from the unioned results select those tupels with ord = o,
     i.e. the tupels from left or right respectively. we can't just
     use q_l or q_r from before the union because we need the new
     surrogate keys computed after the union *)
  let select_ord o =
    ADag.mk_select
      res
      (ADag.mk_funnumeq
	 (res, (ord, ord'))
	 (ADag.mk_attach
	    (ord', (A.Nat o))
	    q_outer))
  in

  let q_1 = select_ord 1n in
  let q_2 = select_ord 2n in

  (* refresh the outer column of all maps with the new keys *)
  let refresh_fs q_i fs_i =
    let refresh_map refcol (env, map, lambda) = 
      let map' =
	ADag.mk_project
	  [(outer, item'); prj inner]
	  (ADag.mk_eqjoin
	     (outer, A.Item refcol)
	     map
	     q_i)
      in
	(env, map', lambda)
    in
    let refresh_maps (refcol, fundevs) = 
      (refcol, List.map (refresh_map refcol) fundevs)
    in
      List.map refresh_maps fs_i

  in
  let fs_l' = refresh_fs q_1 fs_l in
  let fs_r' = refresh_fs q_2 fs_r in

  let combined = same_keys fs_l' fs_r' in
    List.map (fun (col, (funs_l, funs_r)) -> (col, funs_l @ funs_r)) combined
      

(* append the corresponding vs entries from vs_l and vs_r *)
let rec append_vs q_outer vs_l vs_r =
  let m = List.map (append_matching_vs q_outer) (same_keys vs_l vs_r) in
  let l = List.map (append_missing_vs q_outer (A.Nat 1n)) (missing_keys vs_l vs_r) in
  let r = List.map (append_missing_vs q_outer (A.Nat 2n)) (missing_keys vs_r vs_l) in
    List.sort compare (m @ l @ r)

and append_matching_vs (q_outer : ADag.t) ((refcol, tag), ((ti_l, itype_l), (ti_r, _itype_r))) =
  let q_combined = combine_inner_tables ti_l.q ti_r.q in

  let projlist = [(iter, item''); prj pos] @ (refresh_surr_cols ti_l.cs ti_l ti_r item') in

  let q_renumbered = renumber_inner_table q_outer q_combined refcol in

  let q = ADag.mk_project projlist q_renumbered in

  let cs = Cs.choose_nonempty ti_l.cs ti_r.cs in

  let ts = append_ts q_combined ti_l.ts ti_r.ts in

  let vs = append_vs q_combined ti_l.vs ti_r.vs in
  let fs = append_fs q_combined ti_l.fs ti_r.fs in
    (refcol, tag), ({ q = q; cs = cs; ts = ts; vs = vs; fs = fs }, itype_l)

and append_missing_vs q_outer ord_val ((refcol, tag), (ti, itype)) =
  let q_combined = 
    ADag.mk_rownum
      (item', [(iter, A.Ascending); (ord, A.Ascending); (pos, A.Ascending)], None)
      (ADag.mk_attach
	 (ord, ord_val)
	 ti.q)
  in
  let projlist = [(iter, item''); prj pos] @ (refresh_surr_cols ti.cs ti { ti with ts = Ts.empty; vs = Vs.empty } item') in
    
  let q_renumbered = renumber_inner_table q_outer q_combined refcol in

  let q_refreshed = ADag.mk_project projlist q_renumbered in
  let ts = append_ts q_combined ti.ts [] in
  let vs = append_vs q_combined ti.vs [] in
  let fs = append_fs q_combined ti.fs [] in
    (refcol, tag), ({ q = q_refreshed; cs = ti.cs; ts = ts; vs = vs; fs = fs }, itype)

and append_ts q_outer ts_l ts_r =
  let m = List.map (append_matching_ts q_outer) (same_keys ts_l ts_r) in
  let l = List.map (append_missing_ts q_outer (A.Nat 1n)) (missing_keys ts_l ts_r) in
  let r = List.map (append_missing_ts q_outer (A.Nat 2n)) (missing_keys ts_r ts_l) in
    List.sort compare (m @ l @ r)

and append_matching_ts (q_outer : ADag.t) (refcol, (ti_l, ti_r)) =
  let q_combined = combine_inner_tables ti_l.q ti_r.q in

  let projlist = [(iter, item''); prj pos] @ (refresh_surr_cols ti_l.cs ti_l ti_r item') in

  let q_renumbered = renumber_inner_table q_outer q_combined refcol in

  let q = ADag.mk_project projlist q_renumbered in

  let cs = Cs.choose_nonempty ti_l.cs ti_r.cs in

  let ts = append_ts q_combined ti_l.ts ti_r.ts in

  let vs = append_vs q_combined ti_l.vs ti_r.vs in
  let fs = append_fs q_combined ti_l.fs ti_r.fs in
    refcol, { q = q; cs = cs; ts = ts; vs = vs; fs = fs }

and append_missing_ts q_outer ord_val (refcol, ti) =
  let q_combined = 
    ADag.mk_rownum
      (item', [(iter, A.Ascending); (ord, A.Ascending); (pos, A.Ascending)], None)
      (ADag.mk_attach
	 (ord, ord_val)
	 ti.q)
  in
  let projlist = [(iter, item''); prj pos] @ (refresh_surr_cols ti.cs ti { ti with ts = Ts.empty; vs = Vs.empty } item') in
    
  let q_renumbered = renumber_inner_table q_outer q_combined refcol in

  let q_refreshed = ADag.mk_project projlist q_renumbered in
  let ts = append_ts q_combined ti.ts [] in
  let vs = append_vs q_combined ti.vs [] in
  let fs = append_fs q_combined ti.fs [] in
    refcol, { q = q_refreshed; cs = ti.cs; ts = ts; vs = vs; fs = fs }

(* generic pairwise sequence construction. union all q's with ord and
   append fs/ts/vs components.
   if ~newpos is true, new positions are computed with a rank over ord
   and pos *)
(* FIXME: union all cases at once, compute new keys only once:
   ord = 1, ..., nr_cases *)
let sequence_construction (tis : tblinfo list) ~newpos : tblinfo =
  assert ((List.length tis) > 0);
  let union ti_l ti_r =
    let q_union = 
      ADag.mk_rownum
	(item', [(iter, A.Ascending); (pos, A.Ascending); (ord, A.Ascending)], None)
	(ADag.mk_rank
	   (pos', [(ord, A.Ascending); (pos, A.Ascending)])
	   (ADag.mk_disjunion 
	      (ADag.mk_attach
		 (ord, A.Nat 1n)
		 ti_l.q) 
	      (ADag.mk_attach
		 (ord, A.Nat 2n)
		 ti_r.q)))
    in
    let cs = Cs.choose_nonempty ti_l.cs ti_r.cs in
    let q_union' = 
      if newpos then
	ADag.mk_project
	  ([prj iter; (pos, pos')] @ (refresh_surr_cols cs ti_l ti_r item'))
	  q_union
      else
	ADag.mk_project
	  ([prj iter; prj pos] @ (refresh_surr_cols cs ti_l ti_r item'))
	  q_union
    in
      {
	q = q_union';
	cs = cs;
	ts = append_ts q_union ti_l.ts ti_r.ts;
	vs = append_vs q_union ti_l.vs ti_r.vs;
	fs = append_fs q_union ti_l.fs ti_r.fs;
      }
	
  in
    List.fold_left union (List.hd tis) (drop 1 tis)

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
      (surr_col, { q = q_inner'; cs = inner.cs; ts = (slice_inner_tables q_inner' inner.ts); vs = inner.vs; fs = inner.fs })
  in

    if Settings.get_value Basicsettings.Ferry.slice_inner_tables then
      List.map slice ts
    else
      ts

(* FIXME: fragment_env seems unnecessary: if an env entry is used, it is 
   joined anyway, which has the same effect *)

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
	fs = ti.fs;
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

let do_take ti_n ti_l =
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
      vs = ti_l.vs;
      fs = ti_l.fs;
    }

and do_drop ti_n ti_l =
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
      vs = ti_l.vs;
      fs = ti_l.fs;
    }
