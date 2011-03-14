open Utility
(* open ExpressionToAlgebraDefinitions *)
module Defs = QrToAlgebraDefinitions
open Defs
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

let prjcs = prjlist -<- io -<- Cs.offsets

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
    { inner_ti with q = q_unbox }

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
let combine_inner_tables ti_ords =
  let attach_ord (ti, ord_val) =
    ADag.mk_attach (ord, A.Nat ord_val) ti.q
  in

  let union acc ti_ord = ADag.mk_disjunion acc (attach_ord ti_ord) in

  (* union over all q's with ord = 1, ..., n *)
  let q_union = List.fold_left union (attach_ord (List.hd ti_ords)) (drop 1 ti_ords) in

  (* new keys and position *)
    ADag.mk_rownum
      (item', [(iter, A.Ascending); (pos, A.Ascending); (ord, A.Ascending)], None)
      (* FIXME: compute new positions for inner tables?
	 (ADag.mk_rank
	 (pos', [(ord, A.Ascending), (pos, A.Ascending)])
      *)
      q_union

(* use the new surrogate keys from q_outer in column surr_col in the 
   nested plan q_inner *)
let renumber_inner_table q_outer q_inner surr_col = 
  ADag.mk_select
    res
    (ADag.mk_funnumeq
       (res, (ord', ord))
       (ADag.mk_eqjoin
	  (iter, c')
	  q_inner
	  (ADag.mk_project
	     [(ord', ord); (item'', item'); (c', A.Item surr_col)]
	     q_outer)))

(* project all columns which do _not_ contain reference values onto itself and use the fresh
   surrogate values in new_surr in all columns which _do_ contain reference values *)
let refresh_surr_cols cs tis new_surr =

  let union s ti =
    let key_cols = 
      IntSet.from_list
	((Ts.columns ti.ts) @ (Vs.columns ti.vs) @ (Fs.columns ti.fs))
    in
      IntSet.union s key_cols
  in
  let all_key_cols = IntSet.elements (List.fold_left union IntSet.empty tis) in

    (* all columns which are neither vs nor ts surrogate columns *)
    prjlist (io (difference (Cs.offsets cs) all_key_cols))

    (* use new keys in ts/vs/fs surrogate columns *)
    @ (prjlist_single (io all_key_cols) new_surr) 

(* FIXME: need comments on append_ crap *)

(* append the fundev-lists of two fs components *)
let append_fs q_outer ti_ords =

  let refresh (ti, ord_val) =
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
      refresh_fs (select_ord ord_val) ti.fs
  in

  let fs_refreshed = List.map refresh ti_ords in

  let insert map fs =
    List.fold_left
      (fun map (col, fds) ->
	 match IntMap.lookup col map with
	   | Some fds' -> IntMap.add col (fds @ fds') map
	   | None -> IntMap.add col fds map)
      map
      fs
  in

    IntMap.to_alist (List.fold_left insert IntMap.empty fs_refreshed)

module VsMap = Map.Make(struct 
			  type t = Vs.key
			  let compare = Pervasives.compare
			  let show_t = (Show.show_from_string_of (fun _ -> "foo"))
			end)

(* append the corresponding vs entries from vs_l and vs_r *)
let rec append_vs q_outer ti_ords =
  let insert map (ti, ord) =
    List.fold_left
      (fun map (key, (inner_ti, itype)) ->
	 match VsMap.lookup key map with
	   | Some value -> VsMap.add key ((((inner_ti, itype), ord) :: value)) map
	   | None -> VsMap.add key [((inner_ti, itype), ord)] map)
      map
      ti.vs
  in
    (* maps from a vs key column/tag to the list of all inner tis for this key column/tag
       together with the corresponding ords *)
  let m = List.fold_left insert VsMap.empty ti_ords in

  (* partition the map into one which contains all columns which appear as keys
     in more than one ts, and one which contains all columns which appear in exactly one
     ts *)
  let more_than_one, one = VsMap.partition (fun _key inner_tis -> List.length inner_tis > 1) m in

  let append_map map append_fun =
    VsMap.fold
      (fun key inner_tis_ord vs -> (append_fun q_outer key inner_tis_ord) :: vs)
      map
      []
      
  in
  let vs = (append_map more_than_one append_matching_vs) @ (append_map one append_missing_vs) in
    List.sort compare vs

and append_matching_vs q_outer (refcol, tag) innerti_ords = 
  let itype = (snd -<- fst) (List.hd innerti_ords) in
  let inner_tis = List.map (fst -<- fst) innerti_ords in
  let innerti_ords = List.map (fun ((ti, _), ord) -> (ti, ord)) innerti_ords in
  let q_combined = combine_inner_tables innerti_ords in

  let q_renumbered = renumber_inner_table q_outer q_combined refcol in

  (* choose a nonempty cs (if there is one) *)
  let cs = List.fold_left (fun cs_acc ti -> Cs.choose_nonempty cs_acc ti.cs) (List.hd inner_tis).cs (drop 1 inner_tis) in

  let projlist = [(iter, item''); prj pos] @ (refresh_surr_cols cs inner_tis item') in

  let q = ADag.mk_project projlist q_renumbered in

  let ts = append_ts q_combined innerti_ords in
  let vs = append_vs q_combined innerti_ords in
  let fs = append_fs q_combined innerti_ords in

    (refcol, tag), ({ q = q; cs = cs; ts = ts; vs = vs; fs = fs }, itype)

and append_missing_vs q_outer (refcol, tag) innerti_ords =
  assert (List.length innerti_ords = 1);
  let ((inner_ti, itype), ord_val) = List.hd innerti_ords in

  let q_ord = ADag.mk_attach (ord, A.Nat ord_val) inner_ti.q in 
  let q_renumbered = renumber_inner_table q_outer q_ord refcol in
  let q_prj =
    ADag.mk_project
      ([(iter, item''); prj pos] @ (prjlist (io (Cs.offsets inner_ti.cs))))
      q_renumbered
  in
    (refcol, tag), ({ inner_ti with q = q_prj }, itype)

and append_ts q_outer ti_ords =
  let insert map (ti, ord) =
    List.fold_left
      (fun map (col, inner_ti) ->
	 match IntMap.lookup col map with
	   | Some inner_ti_ords -> IntMap.add col ((inner_ti, ord) :: inner_ti_ords) map
	   | None -> IntMap.add col [inner_ti, ord] map)
      map
      ti.ts
  in
  (* maps from a ts key column to the list of all inner tis for this key column 
     together with the corresponding ords *)
  let m = List.fold_left insert IntMap.empty ti_ords in

  (* partition the map into one which contains all columns which appear as keys
     in more than one ts, and one which contains all columns which appear in exactly one
     ts *)
  let more_than_one, one = IntMap.partition (fun _col inner_tis -> List.length inner_tis > 1) m in

  let append_map map append_fun =
    IntMap.fold
      (fun col inner_tis_ord ts -> (append_fun q_outer col inner_tis_ord) :: ts)
      map
      []
  
  in
  let ts = (append_map more_than_one append_matching_ts) @ (append_map one append_missing_ts) in
    List.sort compare ts

and append_matching_ts q_outer refcol innerti_ords =
  let inner_tis = List.map fst innerti_ords in
  let q_combined = combine_inner_tables innerti_ords in

  let q_renumbered = renumber_inner_table q_outer q_combined refcol in

  (* choose a nonempty cs (if there is one) *)
  let cs = List.fold_left (fun cs_acc ti -> Cs.choose_nonempty cs_acc ti.cs) (List.hd inner_tis).cs (drop 1 inner_tis) in

  let projlist = [(iter, item''); prj pos] @ (refresh_surr_cols cs inner_tis item') in

  let q = ADag.mk_project projlist q_renumbered in

  let ts = append_ts q_combined innerti_ords in
  let vs = append_vs q_combined innerti_ords in
  let fs = append_fs q_combined innerti_ords in

    refcol, { q = q; cs = cs; ts = ts; vs = vs; fs = fs }

(* FIXME: is it sufficient to just align the iter values of the inner table
   with the new surrogate values from the outer table or do we have to compute
   new surrogate keys for the inner table itself?
   shouldn't be necessary, because the inner table is not appended to anything *)
(*
and append_missing_ts q_outer refcol (inner_tis, ord_val) =
  assert (List.length inner_tis = 1);
  let inner_ti = List.hd inner_tis in
  let q_combined = 
    ADag.mk_rownum
      (item', [(iter, A.Ascending); (ord, A.Ascending); (pos, A.Ascending)], None)
      (ADag.mk_attach
	 (ord, ord_val)
	 inner_ti.q)
  in
  let projlist = [(iter, item''); prj pos] @ (refresh_surr_cols inner_ti.cs inner_ti item') in
    
  let q_renumbered = renumber_inner_table q_outer q_combined refcol in

  let q_refreshed = ADag.mk_project projlist q_renumbered in
  let ts = append_ts q_combined ti.ts [] in
  let vs = append_vs q_combined ti.vs [] in
  let fs = append_fs q_combined ti.fs [] in
    refcol, { q = q_refreshed; cs = ti.cs; ts = ts; vs = vs; fs = fs }
*)

and append_missing_ts q_outer refcol innerti_ords =
  assert (List.length innerti_ords = 1);
  let (inner_ti, ord_val) = List.hd innerti_ords in
  let q_ord = ADag.mk_attach (ord, A.Nat ord_val) inner_ti.q in 
  let q_renumbered = renumber_inner_table q_outer q_ord refcol in
  let q_prj =
    ADag.mk_project
      ([(iter, item''); prj pos] @ (prjlist (io (Cs.offsets inner_ti.cs))))
      q_renumbered
  in
    refcol, { inner_ti with q = q_prj }

(* generic pairwise sequence construction. union all q's with ord and
   append fs/ts/vs components.
   if ~newpos is true, new positions are computed with a rank over ord
   and pos *)
let sequence_construction (tis : tblinfo list) ~newpos : tblinfo =
  assert ((List.length tis) > 0);
  let ords = List.map Nativeint.of_int (fromTo 1 ((List.length tis) + 1)) in
  let ti_ords = List.combine tis ords in
  let attach_ord (ti, ord_val) =
    ADag.mk_attach (ord, A.Nat ord_val) ti.q
  in

  let union acc ti_ord = ADag.mk_disjunion acc (attach_ord ti_ord) in

  (* union over all q's with ord = 1, ..., n *)
  let q_union = List.fold_left union (attach_ord (List.hd ti_ords)) (drop 1 ti_ords) in

  (* new keys and position *)
  let q_new_keys = 
    ADag.mk_rownum
      (item', [(iter, A.Ascending); (pos, A.Ascending); (ord, A.Ascending)], None)
      (ADag.mk_rank
	 (pos', [(ord, A.Ascending); (pos, A.Ascending)])
	 q_union)
  in

  (* choose a nonempty cs (if there is one) *)
  let cs = List.fold_left (fun cs_acc ti -> Cs.choose_nonempty cs_acc ti.cs) (List.hd tis).cs (drop 1 tis) in

  let q_new_keys' = 
    if newpos then
      ADag.mk_project
	([prj iter; (pos, pos')] @ (refresh_surr_cols cs tis item'))
	q_new_keys
    else
      ADag.mk_project
	([prj iter; prj pos] @ (refresh_surr_cols cs tis item'))
	q_new_keys
  in
    {
      q = q_new_keys';
      cs = cs;
      ts = append_ts q_new_keys ti_ords;
      vs = append_vs q_new_keys ti_ords;
      fs = append_fs q_new_keys ti_ords;
    }

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
let lift q cs =
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

(* lift q into the new iteration context represented by map 
   the column new_iter from the map is used as the new iter column,
   whereas the column old_iter from the map will be used in the join
   on iter *)
let lift_env map env new_iter old_iter =
  let lift map ti =
    let q' =
      (ADag.mk_project
	 ([(iter, new_iter); prj pos] @ (prjlist (io (Cs.offsets ti.cs))))
	 (ADag.mk_eqjoin
	    (iter, old_iter)
	    ti.q
	    map))
    in
      { ti with q = q' }
  in
    AEnv.map (lift map) env

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
