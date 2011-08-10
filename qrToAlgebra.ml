open Utility

module A = Algebra
module ADag = Algebra_dag

module AEnv = Env.Int

open QrToAlgebraDefinitions
module H = QrToAlgebraHelpers

(* module-global reference that stores the global error plan of the generated
   plan bundle (if there is one) *)
let errors = ref []

(* add new error plan to the front of the list of current error plans.
   since compilation is essentialy a bottom-up traversal, the reversed list
   !errors contains a reverse topological ordering of the error plans. If 
   executed in this order, the error condition of any expression can be 
   safely checked, because all error plans of any subexpressions have already
   been executed *)
let add_error_plan q_error = errors := q_error :: !errors

let next_key = ref 0
let tagkeys = ref StringMap.empty
let keytags = ref IntMap.empty

let tagkey tag = 
  match StringMap.lookup tag !tagkeys with
    | Some key -> Nativeint.of_int key
    | None ->
	let key = !next_key in
	  incr next_key;
	  tagkeys := StringMap.add tag key !tagkeys;
	  keytags := IntMap.add key tag !keytags;
	  Nativeint.of_int key

let concatmap_work _loop _ti_l ti_fr (_q_l', _q_v, map, _loop_v) =
  let q = 
    ADag.mk_project
      ([(iter, outer); (pos, pos')] @ (H.prjcs ti_fr.cs))
      (ADag.mk_rank
	 (pos', [(iter, A.Ascending); (pos, A.Ascending)])
	 (ADag.mk_eqjoin
	    (iter, inner)
	    ti_fr.q
	    (ADag.mk_project
	       [H.prj outer; H.prj inner]
	       map)))
  in
    { ti_fr with q = q }

let map_work _loop _ti_l ti_fr (_q_l', _q_v, map, _loop_v) =
  let q = 
    ADag.mk_project
      ([(iter, outer); (pos, pos')] @ (H.prjcs ti_fr.cs)) 
	 (ADag.mk_eqjoin
	    (iter, inner)
	    ti_fr.q
	    map)
  in
    { ti_fr with q = q }

let sortby_work _loop ti_l ti_fr (q_l', _q_v, _map, _loop_lifted) =
  let ordercol_offsets = Cs.offsets ti_fr.cs in
  let ordercols = H.io ordercol_offsets in
  let ordercols' = H.io (H.incr ordercol_offsets (Cs.cardinality ti_l.cs)) in
  let q = 
    ADag.mk_eqjoin
      (inner, iter')
      q_l'
      (ADag.mk_project
	 ((iter', iter) :: (H.prjlist_map ordercols' ordercols))
	 ti_fr.q)
  in
  let rankinfo = List.map (fun c -> (c, A.Ascending)) (ordercols' @ [pos]) in
  let q' =
    ADag.mk_project
      ([H.prj iter; (pos, pos')] @ (H.prjcs ti_l.cs))
      (ADag.mk_rank
	 (pos', rankinfo)
	 q)
  in
    { ti_l with q = q' }

let groupby_work _loop ti_l ti_fr (q_l', _q_v, _map, _loop_lifted) =

  let cs_fr' = Cs.shift (Cs.cardinality ti_l.cs) ti_fr.cs in
  let sortlist = List.map (fun c -> (A.Item c, A.Ascending)) (Cs.offsets cs_fr') in
  let q_1 =
    ADag.mk_rowrank
      (grp_key, (iter, A.Ascending) :: sortlist)
      (ADag.mk_project
	 ([H.prj iter; H.prj pos] @ (H.prjcs ti_l.cs) @ (H.prjcs cs_fr'))
	 (ADag.mk_eqjoin
	    (inner, iter')
	    q_l'
	    (ADag.mk_project
	       ((iter', iter) :: (H.prjlist_map (H.io (Cs.offsets cs_fr')) (H.io (Cs.offsets ti_fr.cs))))
	       ti_fr.q)))
       in
      let grpkey_col = (Cs.cardinality ti_fr.cs) + 1 in
      let q_2 =
	ADag.mk_distinct
	  (ADag.mk_project
	     ([H.prj iter; (pos, grp_key); (A.Item grpkey_col, grp_key)] @ (H.prjlist_map (H.io (Cs.offsets ti_fr.cs)) (H.io (Cs.offsets cs_fr'))))
	     q_1)
      in
      let q_3 =
	ADag.mk_project
	  ([(iter, grp_key); (H.prj pos)] @ (H.prjcs ti_l.cs))
	  q_1
      in
      let cs = Cs.Mapping [("1", ti_fr.cs); ("2", Cs.Column (grpkey_col, `Surrogate))] in 
	{
	  q = q_2;
	  cs = cs;
	  ts = [(grpkey_col, { ti_l with q = q_3 })];
	  vs = Vs.empty;
	  fs = Fs.empty
	}

let takewhile_work loop ti_l ti_p (q_l', _q_v, _map, _loop_lifted) = 
  let cs_l_prj = H.prjcs ti_l.cs in

  (* join predicate result with input list *)
  let q' = 
    ADag.mk_project
      ([H.prj iter; H.prj pos; H.prj res] @ cs_l_prj)
      (ADag.mk_eqjoin
	 (inner, iter')
	 q_l'
	 (ADag.mk_project
	    [(iter', iter); (res, A.Item 1)]
	    ti_p.q))
  in

  (* first position at which the predicate is false *)
  let q_m =
    ADag.mk_funaggr
      (A.Min, (pos'', pos), Some iter)
      (ADag.mk_select
	 pos'
	 (ADag.mk_funboolnot
	    (pos', res)
	    q'))
  in

  (* iterations in which the predicate is never false *)
  let q_e = 
    ADag.mk_project
      ([H.prj iter; H.prj pos] @ cs_l_prj)
      (ADag.mk_eqjoin
	 (iter, iter')
	 ti_l.q
	 (ADag.mk_project
	    [(iter', iter)]
	    (ADag.mk_difference
	       loop
	       (ADag.mk_project
		  [H.prj iter]
		  q_m))))
  in

  (* select all elements before the first false position *)
  let q'' =
   ADag.mk_disjunion
     q_e
     (ADag.mk_project
	([H.prj iter; H.prj pos] @ cs_l_prj)
	(ADag.mk_select
	   res'
	   (ADag.mk_funnumgt
	      (res', (pos'', pos))
	      (ADag.mk_eqjoin
		 (iter, iter')
		 q'
		 (ADag.mk_project
		    [(iter', iter); H.prj pos'']
		    q_m)))))
  in

    { ti_l with q = q'' }

let dropwhile_work _loop ti_l ti_p (q_l', _q_v, _map, _loop_lifted) = 
  let cs_l_prj = H.prjcs ti_l.cs in

  (* join predicate result with input list *)
  let q' = 
    ADag.mk_project
      ([H.prj iter; H.prj pos; H.prj res] @ cs_l_prj)
      (ADag.mk_eqjoin
	 (inner, iter')
	 q_l'
	 (ADag.mk_project
	    [(iter', iter); (res, A.Item 1)]
	    ti_p.q))
  in

  (* first position at which the predicate is false *)
  let q_m =
    ADag.mk_funaggr
      (A.Min, (pos'', pos), Some iter)
      (ADag.mk_select
	 pos'
	 (ADag.mk_funboolnot
	    (pos', res)
	    q'))
  in

  (* select all elements starting with the first false position *)
  let q'' =
    ADag.mk_project
      ([H.prj iter; H.prj pos] @ cs_l_prj)
      (ADag.mk_select
	 res
	 (ADag.mk_funboolor
	    (res, (res', res''))
	    (ADag.mk_funnumeq
	       (res'', (pos'', pos))
	       (ADag.mk_funnumgt
		  (res', (pos, pos''))
		  (ADag.mk_eqjoin
		     (iter, iter')
		     q'
		     (ADag.mk_project
			[(iter', iter); H.prj pos'']
			q_m))))))
  in
    { ti_l with q = q'' }

let rec compile_box env loop e =
  let ti_e = compile_expression env loop e in
  let q_o = 
    ADag.mk_attach
      (pos, A.Nat 1n)
      (ADag.mk_project 
	 [(H.prj iter); (A.Item 1, iter)]
	 loop)
  in
    { 
      q = q_o;
      cs = Cs.Column (1, `Surrogate);
      ts = [(1, ti_e)];
      vs = Vs.empty;
      fs = Fs.empty
    }

and compile_unbox env loop e =
  let ti = compile_expression env loop e in
    assert ((Cs.cardinality ti.cs) = 1);
    assert ((List.length ti.ts) = 1);
    let (offset, inner_ti) = List.hd ti.ts in
      H.do_unbox ti.q offset inner_ti

and compile_append env loop l =
  match l with
    | e :: [] ->
	compile_expression env loop e
    | _ :: _ ->
	  H.sequence_construction (List.map (compile_expression env loop) l) ~newpos:true
    | [] ->
	{
	  q = ADag.mk_emptytbl;
	  cs = Cs.Column (1, `EmptyListLit);
	  ts = Ts.empty;
	  vs = Vs.empty;
	  fs = Fs.empty
	}

and compile_zip env loop l1 l2 =
  let ti_l1 = compile_expression env loop l1 in
  let ti_l2 = compile_expression env loop l2 in
  let q_l1' = H.abspos ti_l1.q ti_l1.cs in
  let q_l2' = H.abspos ti_l2.q ti_l2.cs in
    H.do_zip { ti_l1 with q = q_l1' } { ti_l2 with q = q_l2' }

and compile_unzip env loop zipped =
  let ti_zipped = compile_expression env loop zipped in
  let q = 
    ADag.mk_project
      ([H.prj iter; H.prj pos] @ (H.prjlist_single [A.Item 1; A.Item 2] iter))
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
      ([H.prj iter; H.prj pos] @ (H.prjlist (H.io cols_1)))
      ti_zipped.q
  in
  let q_2 =
    ADag.mk_project
      (let proj = H.prjlist_map (H.io (H.decr cols_2 card)) (H.io cols_2) in
	 ([H.prj iter; H.prj pos] @ proj))
      ti_zipped.q
  in
  let ts_1 = Ts.keep_cols ti_zipped.ts cols_1 in
  let ts_2 = Ts.decr_cols (Ts.keep_cols ti_zipped.ts cols_2) card in
  let vs_1 = Vs.keep_cols ti_zipped.vs cols_1 in
  let vs_2 = Vs.decr_cols (Vs.keep_cols ti_zipped.vs cols_2) card in
  let fs_1 = Fs.keep_cols ti_zipped.fs cols_1 in
  let fs_2 = Fs.decr_cols (Fs.keep_cols ti_zipped.fs cols_2) card in
    {
      q = q; 
      cs = Cs.Mapping [("1", Cs.Column (1, `Surrogate)); ("2", Cs.Column (2, `Surrogate))];
      ts = [(1, { q = q_1; cs = cs_1; ts = ts_1; vs = vs_1; fs = fs_1 });
	    (2, { q = q_2; cs = cs_2'; ts = ts_2; vs = vs_2; fs = fs_2 })];
      vs = Vs.empty;
      fs = Fs.empty
    }

(* FIXME: unite at least compile_or/and/length *)
and compile_or env loop l =
  let ti_l = compile_expression env loop l in
    H.do_list_or loop ti_l

and compile_and env loop l =
  let ti_l = compile_expression env loop l in
    H.do_list_and loop ti_l

and compile_length env loop l =
  let ti_l = compile_expression env loop l in
    H.do_length loop ti_l

and compile_empty env loop l = 
  let ti_l = compile_expression env loop l in
  let ti_length = H.do_length loop ti_l in
    assert (Cs.is_atomic ti_length.cs);
    let q =
      ADag.mk_project
	[H.prj iter; H.prj pos; (A.Item 1, res)]
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
	vs = Vs.empty;
	fs = Fs.empty
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
    let q' = H.wrap_agg loop q (A.Int (Num.Int 0)) in
      {
	q = q';
	cs = Cs.Column (1, `IntType);
	ts = Ts.empty;
	vs = Vs.empty;
	fs = Fs.empty
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
	   [H.prj iter]
	   ti_l.q)
    in
    let ti_inner_nothing = compile_unit empty_iterations in
    let just_key = tagkey "Just" in
    let nothing_key = tagkey "Nothing" in
    let q_outer_just =
      ADag.mk_attach
	(pos, A.Nat 1n)
	(ADag.mk_attach
	   (A.Item 1, A.Nat just_key)
	   (ADag.mk_project
	      [H.prj iter; (A.Item 2, iter)]
	      q_inner_just))
    in
    let q_outer_nothing =
      ADag.mk_attach
	(pos, A.Nat 1n)
	(ADag.mk_attach
	   (A.Item 1, A.Nat nothing_key)
	   (ADag.mk_project
	      [H.prj iter; (A.Item 2, iter)]
	      empty_iterations))
    in
    let cs_inner_just = Cs.Column (1, restype) in
    let outer_cs = Cs.Tag ((1, `Tag), (2, `Surrogate)) in
    let ti_inner_just =
      {
	q = q_inner_just;
	cs = cs_inner_just;
	ts = Ts.empty;
	vs = Vs.empty;
	fs = Fs.empty
      }
    in	
    let vs = [((2, just_key), (ti_inner_just, `Atom)); 
	      ((2, nothing_key), (ti_inner_nothing, `Atom))] in
    let q_outer = ADag.mk_disjunion q_outer_just q_outer_nothing in
      {
	q = q_outer;
	cs = outer_cs;
	ts = Ts.empty;
	vs = vs;
	fs = Fs.empty
      }

and compile_select env loop l i =
  let ti_i = compile_expression env loop i in
  let ti_l = compile_expression env loop l in
  let q_l' = H.abspos ti_l.q ti_l.cs in
  let q_result =
    ADag.mk_project
      ([H.prj iter; H.prj pos] @ H.prjlist (H.io (Cs.offsets ti_l.cs)))
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
		  ti_i.q))))
  in
  let q_error =
    ADag.mk_project
      [H.prj (A.Item 1)]
      (ADag.mk_attach
	 (A.Item 1, A.String "select(): index too large")
	 (ADag.mk_difference
	    loop
	    (ADag.mk_project
	       [H.prj iter]
	       q_result)))
  in
    add_error_plan q_error;
    {
      q = q_result;
      cs = ti_l.cs;
      ts = ti_l.ts;
      vs = ti_l.vs;
      fs = ti_l.fs;
    }
      
and compile_comparison env loop comparison_wrapper tablefun rowfun operand_1 operand_2 =
  let e1_ti = compile_expression env loop operand_1 in
  let e2_ti = compile_expression env loop operand_2 in
  let is_boxed_list ti = Cs.is_boxed_list ti.cs in
  let unbox ti =
    assert (Cs.is_atomic ti.cs);
    assert ((Ts.length ti.ts) = 1);
    let (offset, inner_ti) = List.hd ti.ts in
      H.do_unbox ti.q offset inner_ti
  in
    match (Qr.ImpType.typeof_tqr operand_1, Qr.ImpType.typeof_tqr operand_2) with
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
    let _, q_mapped, map, loop' = H.lift zipped.q zipped.cs in
    let zipped_mapped = { zipped with q = q_mapped } in

    (* we need "<" on rows but have only ">" -> switch arguments *)
    let compared = do_row_greater_real loop' wrapper (switch_zipped zipped_mapped) in

    (* unlift *)
    let compared_backmapped =
      ADag.mk_project
	[(iter, outer); (pos, pos'); H.prj (A.Item 1)]
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
	      [H.prj iter; (A.Item 1, res)]
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
		    [H.prj iter]
		    selected))))
    in
      { 
	q = q;
	cs = Cs.Column (1, `NatType);
	ts = Ts.empty;
	vs = Vs.empty;
	fs = Fs.empty
      }
	
  in

  (* l1 > l2 iff l2 < l1 -> swap arguments *)
  let (l1, l2) = (l2, l1) in
  let l1_abs = H.abspos_ti l1 in
  let l2_abs = H.abspos_ti l2 in
  let zipped = H.do_zip l1_abs l2_abs in
  let zipped_reverse = switch_zipped zipped in 
  let l1_len = H.do_length loop l1_abs in
  let l2_len = H.do_length loop l2_abs in
  let minp_l1_l2 = minpos zipped in
  let minp_l2_l1 = minpos zipped_reverse in

    H.or_op
      (H.and_op
	 (H.smaller l1_len l2_len)
	 (H.equal minp_l1_l2 minp_l2_l1))
      (H.smaller minp_l1_l2 minp_l2_l1)
      
and do_row_greater loop wrapper e1 e2 = 
  let q = do_row_greater_real loop wrapper (H.do_zip e1 e2) in
    { 
      q = q;
      cs = Cs.Column (1, `BoolType);
      ts = Ts.empty;
      vs = Vs.empty;
      fs = Fs.empty;
    }

and do_row_greater_real loop wrapper zipped =

  let column_greater ti_zipped (cse_l, cse_r) =
    let q = 
      if Cs.is_atomic cse_l then
	let col_l = List.hd (Cs.offsets cse_l) in
	let col_r = List.hd (Cs.offsets cse_r) in
	  ADag.mk_project
	    [H.prj iter; H.prj pos; (A.Item 1, res)]
	    (* no need to join since the two arguments are already zipped *)
	    (H.wrap_gt res (A.Item col_l) (A.Item col_r) ti_zipped.q)
      else if Cs.is_variant cse_l then
	failwith "comparison (<, >, <=, >=) of variants is not supported"
      else
	let col_l = List.hd (Cs.offsets cse_l) in
	let col_r = List.hd (Cs.offsets cse_r) in
	  (* inner tables need to be unboxed first *)
	let inner_table_l, inner_table_r =
	  try
	    Ts.lookup ti_zipped.ts col_l, Ts.lookup ti_zipped.ts col_r 
	  with _ -> assert false
	in
	let ti_unboxed_l = H.do_unbox ti_zipped.q col_l inner_table_l in
	let ti_unboxed_r = H.do_unbox ti_zipped.q col_r inner_table_r in
	  (do_table_greater loop wrapper ti_unboxed_l ti_unboxed_r).q
    in
      {
	q = q;
	cs = Cs.Column (1, `BoolType);
	ts = Ts.empty;
	vs = Vs.empty;
	fs = Fs.empty
      }
  in

  let column_equal ti_zipped (cse_l, cse_r) = 
    let q = 
      if Cs.is_atomic cse_l then
	let col_l = List.hd (Cs.offsets cse_l) in
	let col_r = List.hd (Cs.offsets cse_r) in
	  ADag.mk_project
	    [H.prj iter; H.prj pos; (A.Item 1, res)]
	    (* no need to join since the two arguments are already zipped *)
	    (H.wrap_eq res (A.Item col_l) (A.Item col_r) ti_zipped.q)
      else if Cs.is_variant cse_l then
	failwith "comparison (<, >, <=, >=) of variants is not supported"
      else
	let col_l = List.hd (Cs.offsets cse_l) in
	let col_r = List.hd (Cs.offsets cse_r) in
	  (* we compare nested lists represented by a inner table *)

	(* lookup the inner tables referred to by col1, col2 *)
	let inner_table_l, inner_table_r = 
	  try
	    Ts.lookup ti_zipped.ts col_l, Ts.lookup ti_zipped.ts col_r 
	  with _ -> assert false
	in
	  (* unbox the inner tables *)
	let ti_unboxed_l = H.do_unbox ti_zipped.q col_l inner_table_l in
	let ti_unboxed_r = H.do_unbox ti_zipped.q col_r inner_table_r in
	  (* compare the inner tables *)
	  (do_table_equal loop wrapper ti_unboxed_l ti_unboxed_r).q
    in
      {
	q = q;
	cs = Cs.Column (1, `BoolType);
	ts = Ts.empty;
	vs = Vs.empty;
	fs = Fs.empty;
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
	(fun combined eq_k -> (H.and_op (List.hd combined) eq_k) :: combined)
	(take 1 equal_terms)
	(drop 1 equal_terms)
    in
    let combined = List.rev combined in

    (* l_1 = r_1 && l_2 > r_2, ..., l_1 = r_1 && l_2 = r_2 && l_2 > r_3, l_1 = r_1 && ... && l_n-1 = r_n-1 && l_n > r_n *)
    let and_terms = List.map2 H.and_op (drop 1 greater_terms) combined in

      (* l_1 > r_1 || (l_1 = r_1 && l_2 > r_2) || ... *)
      (List.fold_left H.or_op (List.hd greater_terms) and_terms).q
	
and do_table_equal loop wrapper l1 l2 =
  let all = H.do_list_and loop in

  let map_equal source =
    let _, q_s_mapped, map, loop = H.lift source.q source.cs in
    let ti_s = { source with q = q_s_mapped } in
    let q_equal = (do_row_equal loop wrapper (H.do_project "1" ti_s) (H.do_project "2" ti_s)).q in
      (* map the comparison result back into the outer iteration context *)
    let result_backmapped =
      ADag.mk_project
	[(iter, outer); (pos, pos'); H.prj (A.Item 1)]
	(ADag.mk_eqjoin
	   (iter, inner)
	   q_equal
	   map)
    in
      {
	q = result_backmapped;
	cs = Cs.Column (1, `BoolType);
	ts = Ts.empty;
	vs = Vs.empty;
	fs = Fs.empty
      }
  in

  let l1_abs = H.abspos_ti l1 in
  let l2_abs = H.abspos_ti l2 in
  let l1_len = H.do_length loop l1_abs in
  let l2_len = H.do_length loop l2_abs in
    H.and_op 
      (H.equal l1_len l2_len)
      (all (map_equal (H.do_zip l1_abs l2_abs)))

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
	[H.prj iter; H.prj pos; (A.Item 1, A.Item col)]
	q
    in
      if Cs.is_atomic field_cse then
	(* normal comparison of atomic values *)
	let col = List.hd (Cs.offsets field_cse) in
	  H.do_primitive_binop wrapper (project ti_l.q col) (project ti_r.q col)
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
		  [(H.prj iter); (item', A.Item tagcol)]
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
		  [H.prj iter]
		  (ADag.mk_select
		     res'
		     (ADag.mk_funboolnot
			(res', res)
			tags_compared))))
	in

	(* iterations in which the tag is the same *)
	let same_tag =
	  ADag.mk_project
	    [H.prj iter; H.prj item']
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
			(item'', A.Nat tag)
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
	       let unboxed_l = H.do_unbox q_tag_l refcol inner_ti_l in
	       let unboxed_r = H.do_unbox q_tag_r refcol inner_ti_r in
	       let loop_tag = ADag.mk_project [H.prj iter] q_tag_l in
		 match itype_l with
		   | `Atom ->
		       do_row_equal loop_tag wrapper unboxed_l unboxed_r
		   | `List ->
		       do_table_equal loop_tag wrapper unboxed_l unboxed_r)
	    (same_keys (Vs.lookup_col ti_l.vs refcol) (Vs.lookup_col ti_r.vs refcol))
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
	    Ts.lookup ti_l.ts col, Ts.lookup ti_r.ts col 
	  with _ -> assert false
	in
	  (* unbox the inner tables *)
	let ti_unboxed_l = H.do_unbox (project ti_l.q col) col inner_table_l in
	let ti_unboxed_r = H.do_unbox (project ti_r.q col) col inner_table_r in
	  (* compare the inner tables *)
	  (do_table_equal loop wrapper ti_unboxed_l ti_unboxed_r).q
  in

  let q = 
    List.fold_left
      (fun q field ->
	 ADag.mk_project
	   [H.prj iter; H.prj pos; (A.Item 1, res)]
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
      vs = Vs.empty;
      fs = Fs.empty
    }

and compile_binop env loop wrapper restype operand_1 operand_2 =
  let ti_1 = compile_expression env loop operand_1 in
  let ti_2 = compile_expression env loop operand_2 in
    H.do_primitive_binop_ti wrapper restype ti_1 ti_2

and compile_unop env loop wrapper operand =
  let ti_operand = compile_expression env loop operand in
    assert (Cs.is_atomic ti_operand.cs);
    let c = A.Item 1 in
    let res = A.Item 2 in
    let q = 
      ADag.mk_project
	[H.prj iter; H.prj pos; (c, res)]
	(wrapper
	   res c
	   ti_operand.q)
    in
      {
	q = q;
	cs = ti_operand.cs;
	ts = Ts.empty;
	vs = Vs.empty;
	fs = Fs.empty
      }

and compile_concat env loop l =
  let ti_l = compile_expression env loop l in
    assert((List.length ti_l.ts) = 1);
    let ti_sub = Ts.lookup ti_l.ts 1 in
    let c = A.Item 1 in
    let q =
      ADag.mk_project
	([(iter, iter'); (pos, pos'')] @ (H.prjlist (H.io (Cs.offsets ti_sub.cs))))
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
	vs = ti_sub.vs;
	fs = ti_sub.fs;
      }


and compile_take env loop n l =
  let ti_n = compile_expression env loop n in
  let ti_l = compile_expression env loop l in
    H.do_take ti_n ti_l

and compile_drop env loop n l =
  let ti_n = compile_expression env loop n in
  let ti_l = compile_expression env loop l in
    H.do_drop ti_n ti_l

and compile_limit env loop limit offset e =
  let ti_limit = compile_expression env loop limit in
  let ti_offset = compile_expression env loop offset in
  let ti_e = compile_expression env loop e in
    H.do_take ti_limit (H.do_drop ti_offset ti_e)

and compile_hd env loop l = 
  let ti_l = compile_expression env loop l in
  let q_l_abs = H.abspos ti_l.q ti_l.cs  in
  let q = 
    ADag.mk_project
      ([H.prj iter; H.prj pos] @ (H.prjlist (H.io (Cs.offsets ti_l.cs))))
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
      [H.prj (A.Item 1)]
      (ADag.mk_attach
	 (A.Item 1, A.String "hd() of empty list")
	 (ADag.mk_difference
	    loop
	    (ADag.mk_project
	       [H.prj iter]
	       q_l_abs)))
  in
    add_error_plan q_error;
    (* FIXME: slice the inner tables *)
    {
      q = q;
      cs = ti_l.cs;
      ts = ti_l.ts;
      vs = ti_l.vs;
      fs = ti_l.fs;
    }

and compile_tl env loop l = 
  let ti_l = compile_expression env loop l in
  let q_l_abs = H.abspos ti_l.q ti_l.cs in
  let q =
    ADag.mk_project
      ([H.prj iter; H.prj pos] @ (H.prjlist (H.io (Cs.offsets ti_l.cs))))
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
      [H.prj (A.Item 1)]
      (ADag.mk_attach
	 (A.Item 1, A.String "tl() of empty list")
	 (ADag.mk_difference
	    loop
	    (ADag.mk_project
	       [H.prj iter]
	       q_l_abs)))
  in
    add_error_plan q_error;
    {
      q = q;
      cs = ti_l.cs;
      ts = ti_l.ts;
      vs = ti_l.vs;
      fs = ti_l.fs;
    }

and compile_nubflat env loop l =
  let ti_l = compile_expression env loop l in
    assert (not (Cs.is_variant ti_l.cs || Cs.is_boxed_list ti_l.cs));
    let items = H.io (Cs.offsets ti_l.cs) in
    let q_r = 
      ADag.mk_rowrank
	(res, ((iter, A.Ascending) :: (List.map (fun i -> (i, A.Ascending)) items)))
	ti_l.q
    in
    let q_m = 
      ADag.mk_project
	[(res', res); H.prj pos']
	(ADag.mk_funaggr
	   (A.Min, (pos', pos), Some res)
	   q_r)
    in
    let q = 
      ADag.mk_project
	((H.prj iter) :: (H.prj pos) :: (H.prjlist items))
	(ADag.mk_thetajoin
	   [(A.Eq, (res, res')); (A.Eq, (pos, pos'))]
	   q_r
	   q_m)
    in
      {
	q = q;
	cs = ti_l.cs;
	ts = Ts.empty;
	vs = Vs.empty;
	fs = Fs.empty
      }

and compile_quote env loop s =
  (* FIXME quoting at runtime is not implemented *)
  Debug.print "Warning: quoting at runtime is not implemented (compile_quote)";
  compile_expression env loop s

and compile_ho_primitive env loop f source work =
  let ti_l = compile_expression env loop source in
  let q_l', q_v, map, loop_lifted = H.lift ti_l.q ti_l.cs in
  let env_lifted = H.lift_env map env inner outer in
  let ti_fr = apply_exp env_lifted loop_lifted f [{ ti_l with q = q_v }] in
    work loop ti_l ti_fr (q_l', q_v, map, loop_lifted)

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
	  ([H.prj iter; H.prj pos] @ (H.prjlist_map (H.io cols_new) (H.io cols_sorted)))
	  ti.q
      in
	(* change the offsets in the ts mappings accordingly *)
      let col_mapping = List.combine cols_old cols_new in
      let replace_key mapping alist =
	List.map
	  (fun (key, value) -> 
	     let new_key = 
	       try 
		 List.assoc key mapping 
	       with _ -> assert false
	     in
	       (new_key, value))
	  alist
      in
      let ts' = replace_key col_mapping ti.ts in
      let fs' = replace_key col_mapping ti.fs in
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
	  vs = vs';
	  fs = fs';
	}

and merge_records ti_r1 ti_r2 =
  let r2_cols = Cs.offsets ti_r2.cs in
  let new_names_r2 = H.io (H.incr r2_cols (Cs.cardinality ti_r1.cs)) in
  let old_names_r2 = H.io r2_cols in
  let names_r1 = H.io (Cs.offsets ti_r1.cs) in
  let card_r1 = Cs.cardinality ti_r1.cs in
  let r2_ts' = Ts.incr_cols ti_r2.ts card_r1 in
  let r2_vs' = Vs.incr_cols ti_r2.vs card_r1 in
  let r2_fs' = Fs.incr_cols ti_r2.fs card_r1 in
  let q =
    ADag.mk_project
      (H.prjlist ([A.Iter 0; A.Pos 0] @ names_r1 @ new_names_r2))
      (ADag.mk_eqjoin
	 (iter, iter')
	 ti_r1.q
	 ((ADag.mk_project
	     ((iter', iter) :: (H.prjlist_map new_names_r2 old_names_r2))
	     ti_r2.q)))
  in
  let cs = Cs.append_mappings ti_r1.cs ti_r2.cs in
  let ts = Ts.append ti_r1.ts r2_ts' in
  let vs = Vs.append ti_r1.vs r2_vs' in
  let fs = Fs.append ti_r1.fs r2_fs' in
    {
      q = q;
      cs = cs;
      ts = ts;
      vs = vs;
      fs = fs;
    }

and compile_project env loop field record =
  let record_ti = compile_expression env loop record in
    H.do_project field record_ti

and compile_erase env loop erase_fields r =
  let ti_r = compile_expression env loop r in
  let remaining_cs = Cs.filter_record_fields ti_r.cs erase_fields in
  let remaining_cols = Cs.offsets remaining_cs in
  let remaining_ts = Ts.keep_cols ti_r.ts remaining_cols in
  let remaining_vs = Vs.keep_cols ti_r.vs remaining_cols in
  let remaining_fs = Fs.keep_cols ti_r.fs remaining_cols in
  let q =
    ADag.mk_project
      ([H.prj iter; H.prj pos] @ (H.prjlist (H.io remaining_cols)))
      ti_r.q
  in
    {
      q = q;
      cs = remaining_cs;
      ts = remaining_ts;
      vs = remaining_vs;
      fs = remaining_fs;
    }

and compile_record env loop r =
  match r with
    | (name, value) :: [] ->
	singleton_record env loop (name, value)
    | (name, value) :: tl ->
	let f = singleton_record env loop (name, value) in
	  merge_records f (compile_record env loop tl)
    | [] ->
	let q =
	  ADag.mk_attach
	    (A.Item 1, A.Nat 1n)
	    (ADag.mk_attach
	       (pos, A.Nat 1n)
	       loop)
	in
	  {
	    q = q;
	    cs = Cs.Column (1, `EmptyRecord);
	    ts = Ts.empty;
	    vs = Vs.empty;
	    fs = Fs.empty
	  }

and compile_table loop ((_db, _params), tblname, keys, row) =
  (* collect the column names of the table and their types from the row type *)
  let cs_ts = 
    StringMap.fold
      (fun colname (_, typ) cs_ts -> (colname, (H.pf_type_of_typ typ)) :: cs_ts)
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
      vs = Vs.empty;
      fs = Fs.empty
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
      vs = Vs.empty;
      fs = Fs.empty
    }

(* if e1 then e2 else []:
   don't consider the else branch if it represents the empty list. *)
and compile_if2 env loop c t =
  (* condition *)
  let ti_c = compile_expression env loop c in
    assert (Cs.is_atomic ti_c.cs);
    let loop_then =
      ADag.mk_project
	[H.prj iter]
	(ADag.mk_select
	   (A.Item 1)
	   ti_c.q)
    in
    let env_then = H.fragment_env loop_then env in
      compile_expression env_then loop_then t

and compile_if env loop c t e =
  (* condition *)
  let ti_c = compile_expression env loop c in
    assert (Cs.is_atomic ti_c.cs);
    let loop_then =
      ADag.mk_project
	[H.prj iter]
	(ADag.mk_select
	   (A.Item 1)
	   ti_c.q)
    in
    let loop_else =
      ADag.mk_project
	[H.prj iter]
	(ADag.mk_select
	   res
	   (ADag.mk_funboolnot
	      (res, A.Item 1)
	      ti_c.q))
    in
    let env_then = H.fragment_env loop_then env in
    let env_else = H.fragment_env loop_else env in 
    let ti_t = compile_expression env_then loop_then t in
    let ti_e = compile_expression env_else loop_else e in
      H.sequence_construction [ti_t; ti_e] ~newpos:false


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
      vs = Vs.empty;
      fs = Fs.empty
    }

and compile_variant env loop tag value =
  let ti_value = compile_expression env loop value in
  let itype = Qr.ImpType.typeof_tqr value in
  let key = tagkey tag in
  let q = 
    ADag.mk_attach
      (pos, A.Nat 1n)
      (ADag.mk_attach
	 (A.Item 1, A.Nat key)
	 (ADag.mk_project
	    [H.prj iter; (A.Item 2, iter)]
	    loop))
  in
    {
      q = q;
      cs = Cs.Tag ((1, `Tag), (2, `Surrogate));
      ts = Ts.empty;
      vs = [(2, key), (ti_value, itype)];
      fs = Fs.empty
    }

and compile_case env loop value cases =

  let select_key q key =
    let q_compared = 
      ADag.mk_funnumeq
	(res, (A.Item 1, item'))
	(ADag.mk_attach
	   (item', A.Nat key)
	   q)
    in
      (* all iterations which have the tag *)
      ADag.mk_project
	[H.prj iter; H.prj pos; H.prj (A.Item 2)]
	(ADag.mk_select
	   res
	   q_compared)
  in
    
  (* compile value to be matched *)
  let ti_v = compile_expression env loop value in

  let case env tag (var, case_exp) results =
    let key = tagkey tag in
    let q_matching = select_key ti_v.q key in
      try 
	let itbl = fst (Vs.lookup ti_v.vs (2, key)) in
	let ti_unboxed = H.do_unbox q_matching 2 itbl in
	let env' = AEnv.bind env (var, ti_unboxed) in
	let loop' = ADag.mk_project [H.prj iter] q_matching in
	let env' = H.fragment_env loop' env' in
	let case_result = compile_expression env' loop' case_exp in
	  (case_result :: results)
      with NotFound _ -> 
	results
  in

  let results = StringMap.fold (case env) cases [] in

    H.sequence_construction results ~newpos:false

and compile_case_default env loop value cases default =

  let select_key q key =
    let q_compared = 
      ADag.mk_funnumeq
	(res, (A.Item 1, item'))
	(ADag.mk_attach
	   (item', A.Nat key)
	   q)
    in
      (* all iterations which have the tag *)
    let q_matching = 	
      ADag.mk_project
	[H.prj iter; H.prj pos; H.prj (A.Item 2)]
	(ADag.mk_select
	   res
	   q_compared)
    in

    (* all iterations which do not have this tag *)
    let q_other =
      ADag.mk_project
	[H.prj iter; H.prj pos; H.prj (A.Item 1); H.prj (A.Item 2)]
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

  let case tag (var, case_exp) (results, q_other) =
    let key = tagkey tag in
    let q_matching, q_other' = select_key q_other key in
      try 
	let itbl = fst (Vs.lookup ti_v.vs (2, key)) in
	let ti_unboxed = H.do_unbox q_matching 2 itbl in
	let env' = AEnv.bind env (var, ti_unboxed) in
	let loop' = ADag.mk_project [H.prj iter] q_matching in
	let env' = H.fragment_env loop' env' in
	let case_result = compile_expression env' loop' case_exp in
	  (case_result :: results), q_other'
      with NotFound _ -> 
	(results, q_other')
  in

  let default_case q_other (default_var, default_exp) =
    let loop' = ADag.mk_project [H.prj iter] q_other in
    let env' = AEnv.bind env (default_var, (compile_unit loop')) in
    let env' = H.fragment_env loop' env' in
      compile_expression env' loop' default_exp
  in

  let explicit_case_results, q_other = StringMap.fold case cases ([], ti_v.q) in

  let all_results = (default_case q_other default) :: explicit_case_results in
    H.sequence_construction all_results ~newpos:false

and compile_wrong loop =
  let q_error = 
    ADag.mk_project
      [H.prj (A.Item 1)]
      (ADag.mk_attach
 	 (A.Item 1, A.String "something is wrong")
 	 loop)
  in
    add_error_plan q_error;
    {
      q = ADag.mk_emptytbl;
      cs = Cs.Column (1, `IntType);
      ts = Ts.empty;
      vs = Vs.empty;
      fs = Fs.empty
    }




and compile_lambda env loop xs body =
  let q = 
    ADag.mk_attach
      (pos, A.Nat 1n)
      (ADag.mk_project
	 [H.prj iter; (A.Item 1, iter)]
	 loop)
  in
  let map = ADag.mk_project [(outer, iter); (inner, iter)] loop in
  let fs = [(1, [(env, map, (xs, body))])] in
    {
      q = q;
      cs = Cs.Column (1, `Surrogate);
      ts = Ts.empty;
      vs = Vs.empty;
      fs = fs;
    }

and compile_conversion_op env loop arg dest_type  =
  let ti_arg = compile_expression env loop arg in
    assert (Cs.is_atomic ti_arg.cs);
    let q_conv = 
      ADag.mk_project
	[H.prj iter; H.prj pos; (A.Item 1, A.Item 2)]
	(ADag.mk_cast
	   (A.Item 2, A.Item 1, dest_type)
	   ti_arg.q)
    in
      {
	q = q_conv;
	cs = Cs.Column (1, (Cs.column_type_of_pf_type dest_type));
	ts = Ts.empty;
	vs = Vs.empty;
	fs = Fs.empty;
      }

and compile_reverse env loop l =
  let ti_l = compile_expression env loop l in
  let q' =
    ADag.mk_project
      ([H.prj iter; (pos, pos')] @ H.prjlist (H.io (Cs.offsets ti_l.cs)))
      (ADag.mk_rank
	 (pos', [(pos, A.Descending)])
	 ti_l.q)
  in
    { ti_l with q = q' }

and apply_primitive env loop f args =
  match f, args with
    | "&&", [op1; op2] -> compile_binop env loop (H.wrap_and) `BoolType op1 op2
    | "||", [op1; op2] -> compile_binop env loop (H.wrap_or) `BoolType op1 op2
    | "+", [op1; op2] -> compile_binop env loop (H.wrap_1to1 A.Add) `IntType op1 op2
    | "+.", [op1; op2] -> compile_binop env loop (H.wrap_1to1 A.Add) `FloatType op1 op2
    | "-", [op1; op2] -> compile_binop env loop (H.wrap_1to1 A.Subtract) `IntType op1 op2
    | "-.", [op1; op2] -> compile_binop env loop (H.wrap_1to1 A.Subtract) `FloatType op1 op2
    | "*", [op1; op2] -> compile_binop env loop (H.wrap_1to1 A.Multiply) `IntType op1 op2
    | "*.", [op1; op2] -> compile_binop env loop (H.wrap_1to1 A.Multiply) `FloatType op1 op2
    | "/", [op1; op2] -> compile_binop env loop (H.wrap_1to1 A.Divide) `IntType op1 op2
    | "/.", [op1; op2] -> compile_binop env loop (H.wrap_1to1 A.Divide) `FloatType op1 op2
    | "==", [op1; op2] -> compile_comparison env loop H.wrap_eq do_table_equal do_row_equal op1 op2
    | "<>", [op1; op2] -> compile_comparison env loop H.wrap_ne do_table_equal do_row_equal op1 op2
    | ">", [op1; op2] -> compile_comparison env loop H.wrap_gt do_table_greater do_row_greater op1 op2
    | "not", [op]->  compile_unop env loop H.wrap_not op
    | "select", [i; l] -> compile_select env loop i l
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
    | "tilde", [s; p] -> compile_binop env loop (H.wrap_1to1 A.SimilarTo) `BoolType s p
    | "quote", [s] -> compile_quote env loop s
    | "^^", [op1; op2] -> compile_binop env loop (H.wrap_1to1 A.Concat) `StrType op1 op2
    | "nubFlat", [l] -> compile_nubflat env loop l
    | "groupByFlat", [f; l] -> compile_ho_primitive env loop f l groupby_work
    | "takeWhile", [p; l] -> compile_ho_primitive env loop p l takewhile_work
    | "dropWhile", [p; l] -> compile_ho_primitive env loop p l dropwhile_work
    | "limit", [limit; offset; e] -> compile_limit env loop limit offset e
    | "reverse", [l] -> compile_reverse env loop l
    | "floatToInt", [f] -> compile_conversion_op env loop f `IntType
    | "concatMap", [f; l] -> compile_ho_primitive env loop f l concatmap_work
    | "map", [f; l] -> compile_ho_primitive env loop f l map_work
    | "sortByFlat", [f; l] -> compile_ho_primitive env loop f l sortby_work
    | "<", _ | "<=", _ | ">=", _->
	failwith ("CompileQuery.compile_apply: </<=/>= should have been rewritten in query2")
    | s, _->
	failwith ("CompileQuery.compile_apply: primitive " ^ s ^ " not implemented")

and apply_exp env loop lambda_exp arg_tis =
  (* compile the function arguments *)
  (* compile the function expression *)
  match lambda_exp with
    | `Lambda ((xs, body), _) ->
	let env' = 
	  List.fold_left 
	    (fun env (x, a) -> Env.Int.bind env (x, a))
	    env
	    (List.combine xs arg_tis)
	in
	  compile_expression env' loop body
    | _ -> 
	let ti_f = compile_expression env loop lambda_exp in
	  do_apply_exp ti_f arg_tis

and do_apply_exp ti_f args_tis =
    (* extract the fundev list *)
  let fundevs = Fs.lookup ti_f.fs 1 in

  let fundev (function_env, map, (xs, body)) =
    (* lift and filter the fundev map (1) *)
    let map_lift = ADag.mk_eqjoin (outer, A.Item 1) map ti_f.q in

    (* new loop (2) *)
    let loop' = ADag.mk_project [H.prj iter] map_lift in

    (* reduce the map to (outer, inner) again *)
    let map_lift = ADag.mk_project [(outer, iter); H.prj inner] map_lift in

    let filter_arg arg = 
      let q_filtered = 
	ADag.mk_project
	  ([H.prj iter; H.prj pos] @ (H.prjlist (H.io (Cs.offsets arg.cs))))
	  (ADag.mk_eqjoin 
	     (iter, iter') 
	     arg.q 
	     (ADag.mk_project
		[(iter', iter)]
		loop'))
      in
	{ arg with q = q_filtered }
    in

    (* filter the function arguments for this function (3) *)
    let args_filtered = List.map filter_arg args_tis in
      
    let env_lifted = H.lift_env map_lift function_env outer inner in

    (* extend the environment with the function arguments (5) *)
    let env_args = List.fold_left AEnv.bind env_lifted (List.combine xs args_filtered) in

      (* compile the function body (6) *)
      compile_expression env_args loop' body

  in
    H.sequence_construction (List.map fundev fundevs) ~newpos:false

and binding loop env (name, t) =
  let ti = compile_expression env loop t in
    Env.Int.bind env (name, ti)

and compile_expression env loop q : tblinfo =
  match q with
    | `Constant (c, _) -> compile_constant loop c
    | `Apply ((`Primitive f, args), _) -> apply_primitive env loop f args 
    | `Apply ((lambda_exp, args), _) -> 
	let arg_tis = List.map (compile_expression env loop) args in
	apply_exp env loop lambda_exp arg_tis
    | `Variable (x, _) -> AEnv.lookup env x
    | `Project ((label, r), _) -> compile_project env loop label r
    | `Extend (empty, None, _) when (StringMap.size empty) = 0 -> compile_unit loop 
    | `Extend (ext_fields, r, _) ->
	let ext_fields = StringMap.to_alist ext_fields in
	  extend_record env loop ext_fields (opt_map (compile_expression env loop) r)
    | `Erase ((erase_fields, r), _) -> compile_erase env loop erase_fields r
    | `Singleton (e, _) -> compile_expression env loop e
    | `Concat (l, _) -> compile_append env loop l
    | `Table (t, _) -> compile_table loop t
    | `If ((c, t, Some e), _) -> compile_if env loop c t e
    | `If ((c, t, None), _) -> compile_if2 env loop c t
    | `Box (e, _) -> compile_box env loop e
    | `Unbox (e, _) -> compile_unbox env loop e
    | `Inject ((tag, value), _) -> compile_variant env loop tag value
    | `Case ((v, cases, Some default), _) -> compile_case_default env loop v cases default
    | `Case ((v, cases, None), _) -> compile_case env loop v cases
    | `Wrong _ -> compile_wrong loop 
    | `Lambda ((xs, body), _) -> compile_lambda env loop xs body
    | `Let (bs, tc, _) -> 
	let env = List.fold_left (binding loop) env bs in
	  compile_expression env loop tc
    | `Primitive _ -> failwith "compile_expression: eval error"

let rec wrap_serialize ti = 
  let serialize q cs =
    ADag.mk_serializerel 
      (iter, pos, H.io (Cs.offsets cs))
      (ADag.mk_nil)
      q
  in
    {
      q = serialize ti.q ti.cs;
      cs = ti.cs;
      ts = alistmap wrap_serialize ti.ts;
      vs = alistmap (fun (ti, itype) ->  (wrap_serialize ti, itype)) ti.vs;
      fs = Fs.empty
    }

let wrap_serialize_errors q_error =
  ADag.mk_serializerel 
    (iter, pos, [A.Item 1])
    ADag.mk_nil
    (ADag.mk_attach
       (pos, A.Nat 1n)
       (ADag.mk_rank
	  (iter, [(A.Item 1, A.Ascending)])
	  q_error))

let compile exp =
  let loop = 
    (ADag.mk_littbl
       ([[A.Nat 1n]], [(A.Iter 0, `NatType)]))
  in
  let ti = compile_expression AEnv.empty loop exp in
  let e = !errors in
  let kt = !keytags in
    errors := [];
    keytags := IntMap.empty;
    (* reverse error plan list so that they can be executed from the beginning
       in the proper bottom-up order *)
    wrap_serialize ti, List.map wrap_serialize_errors (List.rev e), kt
