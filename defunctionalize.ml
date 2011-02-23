(*pp deriving *)
open Utility
open Qr

(* TODO: instantiate every effect row var with an empty closed row (normalization of types) *)

(*
let normalize_types q normalized =
  let rec binding b =
    match b with
      | `Let ->
      | `Fun ->
      | `PFun ->

  and walk q =
    match q with
      | `Computation (bs, tc) ->
	  let bs, specmap = clone_bindings im bs in
	  let bs' = List.map (replace_spec_binding specmap) bs in
	  let tc' = replace_spec specmap tc in 
	    Debug.print (Show.show show_specmap specmap);
	  (* TODO: restrict im by names which have already been handled *)
(*	  let tc' = specialize (InstMap.restrict im cloned) tc' in *)
	  let tc' = specialize im tc' in
	    `Computation (bs', tc')
      | `Constant _ | `Database _ | `Table _ | `Wrong _ | `Variable _ -> q
      | `Project (label, v) -> `Project (label, specialize im v)
      | `Extend (extend_fields, r) -> `
	  Extend (StringMap.map (specialize im) extend_fields, opt_map (specialize im) r)
      | `Erase (labels, v) -> `Erase (labels, specialize im v)
      | `Inject (tag, v, t) -> `Inject (tag, specialize im v, t)
      | `TAbs (tyvars, v) -> `TAbs (tyvars, specialize im v)
      | `List xs -> `List (List.map (specialize im) xs)
      | `Apply (f, args) -> `Apply (specialize im f, List.map (specialize im) args)
      | `Case (v, cases, default) ->
	  let v = specialize im v in
	  let case (binder, body) = (binder, specialize im body) in
	  let cases = StringMap.map case cases in
	  let default = opt_map case default in
	    `Case (v, cases, default)
      | `If (c, t, e) -> `If (specialize im c, specialize im t, specialize im e)
      | `TApp (v, tyargs) -> `TApp (specialize im v, tyargs)
  in
*)    

module Census =
struct
  let merge maps = 
    let aux _ a b =
      match a, b with
	| Some a, Some b -> Some (a + b)
	| None, Some b -> Some b
	| Some a, None -> Some a
	| None, None -> assert false
    in
      List.fold_left (IntMap.merge aux) (List.hd maps) (drop 1 maps) 

  let rec bindings (bs : binding list)=
    let binding =
      function
	| `PFun _ -> IntMap.empty
	| `Fun (_, _, body, _) 
	| `Let (_, _, body) -> 
	    count body
    in
      List.map binding bs
    
    and count q =
    match q with
      | `Variable var -> IntMap.add var 1 IntMap.empty
      | `Constant _ | `Database _ | `Table _
      | `Wrong _ -> IntMap.empty
      | `Extend (extend_fields, r) ->
	  let cm = StringMap.fold (fun _ f cm -> merge [cm; count f]) extend_fields IntMap.empty in
	    opt_app (fun r -> merge [cm; count r]) cm r 
      | `Project (_, v) 
      | `Erase (_, v) 
      | `Inject (_, v, _) 
      | `TApp (v, _)
      | `TAbs (_, v) -> count v
      | `List xs -> merge (List.map count xs)
      | `Apply (f, args) ->
	  merge ((count f) :: (List.map count args))
      | `Case (v, cases, default) ->
	  let cm = count v in
	  let cm = 
	    StringMap.fold 
	      (fun _ (_, body) cm -> merge [cm; count body]) 
	      cases 
	      cm 
	  in
	    opt_app (fun (_, body) -> merge [cm; count body]) cm default
      | `If (c, t, e) -> merge [count c; count t; count e]
      | `Computation (bs, tc) ->
	  merge ((count tc) :: (bindings bs))
end

module ElimDeadDefs =
struct
  let bindings cm bs =
    let filter =
	function
	  | `PFun ((name, _), _)
	  | `Let ((name, _), _, _)
	  | `Fun ((name, _), _, _, _) ->
	      match IntMap.lookup name cm with
		| Some c when c > 0 -> true
		| _ -> false
    in
      List.filter filter bs

  let rec eliminate cm q = 
    match q with
      | `Constant _ | `Variable _ | `Database _ |`Table _
      | `Wrong _ -> q
      | `Project (label, v)  -> `Project (label, eliminate cm v)
      | `Erase (labels, v) -> `Erase (labels, eliminate cm v)
      | `Extend (extend_fields, r) ->
	  let extend_fields = StringMap.map (eliminate cm) extend_fields in
	  let r = opt_map (eliminate cm) r in
	    `Extend (extend_fields, r)
      | `Inject (tag, v, t) -> `Inject (tag, eliminate cm v, t)
      | `TApp (v, tyargs) -> `TApp (eliminate cm v, tyargs)
      | `TAbs (tyvars, v) -> `TAbs (tyvars, eliminate cm v)
      | `List xs -> `List (List.map (eliminate cm) xs)
      | `Apply (f, args) -> `Apply (eliminate cm f, List.map (eliminate cm) args)
      | `If (c, t, e) -> `If (eliminate cm c, eliminate cm t, eliminate cm e)
      | `Case (v, cases, default) ->
	  let case (binder, body) = (binder, eliminate cm body) in
	  let v = eliminate cm v in
	  let cases = StringMap.map case cases in
	  let default = opt_map case default in
	    `Case (v, cases, default)
      | `Computation (bs, tc) ->
	  let tc = eliminate cm tc in
	    match bindings cm bs with
	      | [] -> tc
	      | bs -> `Computation (bs, tc)
end

module FreeVars =
struct

  let rec name_map bound proj map =
    StringMap.fold
      (fun _ v free -> 
	 let free' = fv bound (proj v) in
	   IntSet.union free' free)
      map
      IntSet.empty

  and bindings bound bs =
    List.fold_left
      (fun (free, bound) b ->
	 match b with
	   | `Let ((var, _), _, tc) ->
	       let bound' = IntSet.add var bound in
	       let free = IntSet.union (fv bound tc) free in
		 (free, bound')
	   | `Fun ((var, _), arg_binders, body, _) ->
	       let bound' = List.fold_right IntSet.add (List.map fst arg_binders) bound in
	       let free = IntSet.union free (fv bound' body) in
	       let bound = IntSet.add var bound in
		 (free, bound)
	   | `PFun ((var, _), _) ->
	       let bound = IntSet.add var bound in
		 (free, bound))
      (IntSet.empty, bound)
      bs

  and fv bound = function
    | `Variable name -> 
	if not (IntSet.mem name bound) then
	  IntSet.add name IntSet.empty
	else
	  IntSet.empty
    | `Extend (extend_fields, base) ->
	let free = 
	  StringMap.fold
	    (fun _ v free -> IntSet.union (fv bound v) free)
	    extend_fields
	    IntSet.empty
	in
	  begin
	    match base with
	      | Some r -> IntSet.union (fv bound r) free
	      | None -> free
	  end
    | `Project (_, value) | `Erase (_, value) | `Inject (_, value, _) 
    | `TApp (value, _) | `TAbs (_, value) -> 
	fv bound value
    | `Computation (bs, tc) ->
	let free, bound = bindings bound bs in
	  IntSet.union (fv bound tc) free
    | `Fun (binders, _, body, _) ->
	let bound = List.fold_right IntSet.add (List.map fst binders) bound in
	  fv bound body
    | `Case (v, cases, default) ->
	let case _ ((var, _), body) free =
	  IntSet.union (fv (IntSet.add var bound) body) free
	in
	let v_free = fv bound v in
	let cases_free = StringMap.fold case cases IntSet.empty in
	let default_free = 
	  opt_app 
	    (fun ((var, _), body) -> fv (IntSet.add var bound) body) 
	    IntSet.empty
	    default 
	in
	  IntSet.union_all [v_free; cases_free; default_free]
    | `List xs -> IntSet.union_all (List.map (fv bound) xs)
    | `If (c, t, e) ->
	IntSet.union_all [fv bound c; fv bound t; fv bound e]
    | `Database _ | `Table _ | `PrimitiveFun _ | `Wrong _ | `Constant _ ->
	IntSet.empty
    | `Apply (f, args) ->
	IntSet.union_all ((fv bound f) :: (List.map (fv bound) args))

  let freevars = fv

  let boundvars =
    function
      | `Fun (binders, _, _, _) -> 
	  List.fold_right IntSet.add (List.map fst binders) IntSet.empty
      | `Let (bindings, _) ->
	  List.fold_right IntSet.add (List.map (fst -<- fst) bindings) IntSet.empty 
      | `Case (_, cases, default) ->
	  let case_bound = 
	    StringMap.fold 
	      (fun _ ((name, _), _) bound -> IntSet.add name bound)
	      cases
	      IntSet.empty
	  in
	  let default_bound = opt_app 
	    (fun ((var, _), _) -> IntSet.singleton var) 
	    IntSet.empty 
	    default 
	  in
	    IntSet.union case_bound default_bound
      | _ -> IntSet.empty
end

module Inliner =
struct

  type ctx = { venv : qr Env.Int.t; 
	       fenv : funct Env.Int.t;
	       tyenv : Types.datatype Env.Int.t; 
	       census : int IntMap.t }

  (* FIXME: differentiate between functions and other values based on type *)
  (* FIXME: differentiate between function inlining and value propagation *)
  let is_inlineable census name =
    match IntMap.lookup name census with
      | Some c when c < 2 -> true
      | _ -> false

  let rec inline ctx q =
    let inl = inline ctx in
      match q with
	| `Variable name ->
	    begin
	      match Env.Int.find ctx.venv name with
		| Some e -> e
		| None -> q
	    end
	| `Extend (extend_fields, r) ->
	    let extend_fields = StringMap.map inl extend_fields in
	    let r = opt_map inl r in
	      `Extend (extend_fields, r)
	| `Project (label, r) ->
	    `Project (label, inl r)
	| `Erase (names, r) ->
	    `Erase (names, inl r)
	| `Inject (tag, value, t) ->
	    `Inject (tag, inl value, t)
	| `TApp (v, tyargs) ->
	    (* `TApp (inl v, tyargs) *)
	    begin
	      match inl v with
		| `Variable var -> `TApp (`Variable var, tyargs)
		| e -> e
	    end
	| `TAbs (tyvars, v) ->
	    `TAbs (tyvars, inl v)
	| `Computation ([], tc) -> inl tc
	| `Computation (bs, tc) ->
	    let bs, venv, fenv = bindings ctx bs in
	    let tc = inline { ctx with venv = venv; fenv = fenv } tc in
	      `Computation (bs, tc)
	| `List xs -> 
	    `List (List.map inl xs)
	| `Apply (f, args) ->
	    apply ctx (inl f) (List.map inl args)
	| `Case (v, cases, default) ->
	    let case (binder, body) =
	      (binder, inl body)
	    in
	    let v = inl v in
	    let cases = StringMap.map case cases in
	    let default = opt_map case default in
	      begin
		match inl v with
		  | `Inject (tag, value, _t) ->
		      let (binder, body) =
			begin
			  match StringMap.lookup tag cases, default with
			    | Some case, _ -> case
			    | None, Some default -> default
			    | None, None -> failwith "Inline.inline: neither matching case nor default case"
			end
		      in
			beta ctx [value] [] [binder] body
		  | _ -> `Case (v, cases, default)
	      end
	      
	| `If (c, t, e) ->
	    let c = inl c in
	    let t = inl t in
	    let e = inl e in
	      begin
		match c with
		  | `Constant (`Bool true) -> t
		  | `Constant (`Bool false) -> e
		  | _ -> `If (inl c, inl t, inl e)
	      end
	| `Constant _
	| `Wrong _
	| `Database _
	| `Table _
	| `PrimitiveFun _ -> q

  and bindings ctx bs =
    let binding b (bs, venv, fenv) =
      let b, venv, fenv = 
	match b with
	  | `PFun _ -> b, venv, fenv
	  | `Let ((name, _) as binder, tyvars, tc) ->
	      let tc = inline { ctx with venv = venv; fenv = fenv } tc in
	      let b = `Let (binder, tyvars, tc) in
	      let venv = 
		if is_inlineable ctx.census name then
		  match tyvars with
		    | [] -> Env.Int.bind venv (name, tc)
		    | tyvars -> Env.Int.bind venv (name, `TAbs (tyvars, tc))
		else
		  venv
	      in
		b, venv, fenv
	  | `Fun ((name, _) as binder, arg_binders, body, tyvars) ->
	      let body = inline { ctx with venv = venv; fenv = fenv } body in
	      let b = `Fun (binder, arg_binders, body, tyvars) in
	      let fenv = 
		if is_inlineable ctx.census name then
		  Env.Int.bind fenv (name, (binder, arg_binders, body, tyvars))
		else
		  fenv
	      in
		b, venv, fenv
      in
	b :: bs, venv, fenv
    in
      List.fold_right binding bs ([], ctx.venv, ctx.fenv)

  and beta ctx args pre_bindings binders body =
    try
      let arg_bindings = List.map2 (fun binder arg -> `Let (binder, [], arg)) binders args in
	inline ctx (`Computation (pre_bindings @ arg_bindings, body))
    with Invalid_argument _ -> failwith "arity mismatch in function inlining"

  and apply ctx f args =
      match f with
	| `Variable fvar
	| `TApp (`Variable fvar, _) ->
	    begin
	      match Env.Int.find ctx.fenv fvar with
		| Some (_binder, arg_binders, body, _tyvars) -> beta ctx args [] arg_binders body
		| None -> `Apply (f, args)
	    end
(*
	| 
	(* FIXME: maintain typability: instantiate type, if vars remain, introduce tabs *)
	| `TApp (`Fun (binders, _tyvars, body, _t), _) -> beta ctx args [] binders body
	| `Let (bs, `Fun (binders, _tyvars, body, _t)) -> beta ctx args bs binders body
*)
	| _ -> `Apply (f, args)

end

(* a special kind of structural equality on types that doesn't look
   inside points *)
let rec eq_types_relaxed : (Types.datatype * Types.datatype) -> bool =
  fun (t1, t2) ->
    let rec unalias = function
      | `Alias (_, x) -> unalias x
      | x             -> x in
      match unalias t1 with 
	| `Not_typed -> 
            begin match unalias t2 with
		`Not_typed -> true
	      | _          -> false
            end
	| `Primitive x ->
            begin match unalias t2 with
		`Primitive y -> x = y
	      | _            -> false
            end
	| `MetaTypeVar lpoint ->
            begin match unalias t2 with
		`MetaTypeVar rpoint -> Unionfind.equivalent lpoint rpoint
	      | _                   -> false
            end
	| `Function (lfrom, _lm, lto) ->
            begin match unalias t2 with
		`Function (rfrom, _rm, rto) -> eq_types_relaxed (lfrom, rfrom)
                  && eq_types_relaxed (lto,   rto)
                  (* && eq_rows  (lm,    rm) *)
	      | _                          -> false
            end
	| `Record l ->
            begin match unalias t2 with
		`Record r -> eq_rows (l, r)
	      | _         -> false
            end
	| `Variant l ->
            begin match unalias t2 with
		`Variant r -> eq_rows (l, r)
	      | _          -> false
            end
	| `Application (s, ts) ->
            begin match unalias t2 with
		`Application (s', ts') -> s = s' && List.for_all2 (Utility.curry eq_type_args) ts ts'
	      | _ -> false
            end
	| `ForAll (qs, t) ->
            begin match unalias t2 with
	      | `ForAll (qs', t') ->
                  List.for_all2 (fun q q' -> eq_quantifier (q, q'))
                    (Types.unbox_quantifiers qs)
                    (Types.unbox_quantifiers qs') &&
                    eq_types_relaxed (t, t')
	      | _ -> false
            end

	| `Alias  _ -> assert false
	| `Table _  -> assert false
and eq_quantifier : (Types.quantifier * Types.quantifier) -> bool =
  function
    | `TypeVar ((lvar, _), _), `TypeVar ((rvar, _), _)
    | `RowVar ((lvar, _), _), `RowVar ((rvar, _), _)
    | `PresenceVar (lvar, _), `PresenceVar (rvar, _) -> lvar = rvar
and eq_rows : (Types.row * Types.row) -> bool =
  fun ((lfield_env, lrow_var), (rfield_env, rrow_var)) ->
    eq_field_envs (lfield_env, rfield_env) && eq_row_vars (lrow_var, rrow_var)
and eq_presence =
  function
    | `Absent, `Absent
    | `Present, `Present -> true
    | `Var lpoint, `Var rpoint -> Unionfind.equivalent lpoint rpoint
and eq_field_envs (lfield_env, rfield_env) =
  let eq_specs (lf, lt) (rf, rt) = eq_presence (lf, rf) && eq_types_relaxed (lt, rt) in
    StringMap.equal eq_specs lfield_env rfield_env
and eq_row_vars (lpoint, rpoint) =
  (* QUESTION:
     Do we need to deal with closed rows specially?
  *)
  match Unionfind.find lpoint, Unionfind.find rpoint with
    | `Closed, `Closed -> true
    | `Flexible (var, _), `Flexible (var', _)
    | `Rigid (var, _), `Rigid (var', _)
    | `Recursive (var, _), `Recursive (var', _) -> var=var'
    | _, _ -> Unionfind.equivalent lpoint rpoint
and eq_type_args =
  function
    | `Type lt, `Type rt -> eq_types_relaxed (lt, rt)
    | `Row lr, `Row rr -> eq_rows (lr, rr)
    | `Presence lf, `Presence rf -> eq_presence (lf, rf)
    | _, _ -> false

(* FIXME: this should be possible more elegantly *)
let eq_types_mod_effects origt instt =
  let strip_quantifiers = function
    | `ForAll (_, t) -> t
    | t -> t
  in
  let origt = TypeUtils.concrete_type origt in
  let instt = TypeUtils.concrete_type instt in
    match TypeUtils.quantifiers origt with
      | [`RowVar _] -> 
	  eq_types_relaxed ((strip_quantifiers origt), instt)
      | _ -> false

module Monomorphize =
struct

(*
  module MultiSubst =
  struct
    type tyarg_seq = tyarg list deriving (Show)
    type tyvar_seq = tyvar list deriving (Show)
    type t = tyvar_seq * tyarg_seq list deriving (Show)

    let lookup 

    let eq_tyarg_seq ta1 ta2 =
      List.for_all Unify.eq_type_args (List.combine ta1 ta2)

    let eq_tyvar_seq tv1 tv2 =
      List.for_all Unify.eq_quantifier (List.combine tv1 tv2)

    let apply_subst : tyvar_seq * tyarg_seq -> Types.datatype -> Types.datatype = fun tvs tas t ->
      failwith "apply_subst not implemented"

    let flatten_substs : t -> (tyvar_seq * tyarg_seq) list = fun (tyvar_seq, tyarg_seqs) ->
      List.map (fun tyarg_seq -> (tyvar_seq, tyarg_seq)) tyarg_seqs
      
    let apply : t -> Types.datatype -> Types.datatype list = fun msubst t =
      List.map apply_subst (flatten_substs msubst)
      
    let apply_seq : t list -> Types.datatype -> Types.datatype list list = fun msubst_seq ->
      failwith "not implemented"

    let sum : t -> t -> t = fun ms1 ms2 ->
      let domain1 = domain ms1 in
      let domain2 = domain ms2 in
	assert (eq_tyvar_seq domain1 domain2);
	List.unduplicate equal_tyarg_seq ((snd ms1) @ (snd ms2))

    let compose : t -> t -> t = fun ms1 ms2 -> 
      

  end

  module InstMap =
  struct
    type t = (var * MultiSubst.t) list

    let restrict : t -> var list -> t = remove_keys 

    let domain = List.map fst

    let sum : t -> t -> t = fun im1 im2 ->
      let d1 = domain im1 in
      let d2 = domain im2 in
      let d = List.unduplicate (=) (d1 @ d2) in
      let aux name =
	match lookup name im1, lookup name im2 with
	  | Some ms1, Some ms2 -> (name, MultiSubst.sum ms1 ms2)
	  | Some ms1, None -> (name, ms1)
	  | None, Some ms2 -> (name, ms2)
	  | None, None -> assert false
      in
	List.map aux d
      
    let compose_multisubst : t -> MultiSubst.t -> t = fun im ms ->
      assert ((List.length im) = 1);
      let (name, ms_name) = List.hd im in
	[(name, MultiSubst.compose ms ms_name)]

    let lookup : t -> var -> MultiSubst.t = fun im v ->
      List.assoc var im
  end

  let instmap : q -> InstMap.t = fun q ->
    failwith "instmap not implemented"
    
  let specialize : q -> InstMap.t -> q = fun q im ->
    failwith "specialize not implemented"
    
*)

  module InstMap =
  struct
    type t = (tyarg list list) IntMap.t
	deriving (Show)

    let eq_tyarg_seq ta1 ta2 =
      List.for_all Unify.eq_type_args (List.combine ta1 ta2)

    let empty = IntMap.empty

    let sum : t -> t -> t = fun m1 m2 ->
      let aux _k tas1 tas2 =
	match tas1, tas2 with
	  | Some tas1, Some tas2 -> Some (unduplicate eq_tyarg_seq (tas1 @ tas2))
	  | Some tas1, None -> Some tas1
	  | None, Some tas2 -> Some tas2
	  | None, None -> assert false
      in
	IntMap.merge aux m1 m2

    let sum_list : t list -> t = fun ims ->
      List.fold_left sum empty ims

    let make name tyargs =
      IntMap.add name tyargs IntMap.empty

    (* let restrict foo *)

    let fold = IntMap.fold

  end

  type specmap = ((var * tyarg list) * var) list
      deriving (Show)

  let specmap_lookup k l =
    let eq (var1, tyargs1) (var2, tyargs2) =
(*      Debug.f "%d %d" var1 var2;
      Debug.print (Show.show (Show.show_list Types.show_type_arg) tyargs1);
      Debug.print (Show.show (Show.show_list Types.show_type_arg) tyargs2); *)
      var1 = var2 && InstMap.eq_tyarg_seq tyargs1 tyargs2
    in
    let rec aux m =
      match m with
	| (k', v) :: es ->
	    if eq k' k then
	      Some v
	    else
	      aux es
	| [] -> None

    in
      aux l

  let rec instmap = function
    | `TApp (`Variable name, tyargs) -> 
	InstMap.make name [tyargs]
    | `Variable _ -> 
	InstMap.empty
    | `TApp (_v, _) -> 
	failwith "TApp applied to non-variable"
    | `Extend (extend_fields, r) ->
	let aux _label v acc = InstMap.sum (instmap v) acc in
	  InstMap.sum 
	    (StringMap.fold aux extend_fields InstMap.empty)
	    (opt_app instmap InstMap.empty r)
    | `Project (_, r) -> 
	instmap r
    | `Erase (_, r) -> 
	instmap r
    | `Inject (_, v, _t) -> 
	instmap v
    | `TAbs (_, v) -> 
	instmap v
    | `List xs -> 
	InstMap.sum_list (List.map instmap xs)
    | `Apply (f, args) -> 
	InstMap.sum_list ((instmap f) :: (List.map instmap args))
    | `Case (v, cases, default) -> 
	(* specialize binders? *)
	let im = instmap v in
	let case _tag (_binder, body) acc =
	  InstMap.sum (instmap body) acc
	in
	let im = StringMap.fold case cases im in
	  InstMap.sum im (opt_app (instmap -<- snd) InstMap.empty default)
    | `If (c, t, e) -> 
	InstMap.sum_list [instmap c; instmap t; instmap e]
    | `Computation (bs, tc) ->
	(* collect instmap from tc, 
	   clone bindings, 
	   specialize tc, 
	   pass remaining instmap upwards *)
	(* FIXME: do this correctly *)
	InstMap.sum (bindings bs) (instmap tc)
    | `Constant _ | `Database _ | `Table _ | `PrimitiveFun _ | `Wrong _ ->
	InstMap.empty

  and bindings bs =
    let binding =
      function
	| `PFun _ -> InstMap.empty
	| `Let (_binder, _tyvars, tc) -> instmap tc
	| `Fun (_binder, _binders, body, _tyvars) -> instmap body
    in
      InstMap.sum_list (List.map binding bs)

  (* clone a function binding at a concrete type *)
  (* basic assumption: type gets fully instantiated *)
  and clone b tyargs : binding list * specmap =
    (* generate a fresh binder with the instantiated type and a
       new variable *)
    let clone_binder (_f, (t, fs, _)) =
      let t' = Instantiate.apply_type t tyargs in
      let fs' = 
	if (String.length fs) = 0 then 
	  "" 
	else
	  fs ^ "___" ^ (Types.string_of_datatype t') 
      in
	Var.fresh_var (t', fs', `Local)
    in
      try
	match b with
	  | `PFun ((f, (t, _, _)) as binder, dispatch) ->
	      let (_, (t', _, _)) as binder', f' = clone_binder binder in
(*	      let tyenv = Env.Int.bind tyenv (f', t') in *)
	      let b' = `PFun (binder', dispatch) in
		(* don't clone if the original and cloned types differ
		   only by their effects *)
		if not (eq_types_mod_effects t t') then
		  [b; b'], [((f, tyargs), f')]
		else
		  begin
		    Debug.f "not cloning at type %s (orig %s)" (Types.string_of_datatype t') (Types.string_of_datatype t);
		    [b], []
		  end
		  
	  | `Fun ((f, (t, _, _)) as binder, arg_binders, body, _tyvars) ->
	      let (_, (t', _, _)) as binder', f' = clone_binder binder in
(*	      let tyenv = Env.Int.bind tyenv (f', t') in *)
		(* FIXME: adapt type annotations in body *)
	      let argts = TypeUtils.arg_types t' in
	      let arg_binders' = 
		List.map 
		  (fun ((n, (_argt, s, l)), argt') -> (n, (argt', s, l))) 
		  (List.combine arg_binders argts)
	      in
		(* don't clone if the original and cloned types differ
		   only by their effects *)
		if not (eq_types_mod_effects t t') then
		  [b; `Fun (binder', arg_binders', body, [])], [((f, tyargs), f')]
		else
		  begin
		    Debug.f "not cloning at type %s (orig %s)" (Types.string_of_datatype t') (Types.string_of_datatype t);
		    [b], []
		  end
	  | `Let _ -> failwith "Monomorphize.clone: attempto to clone non-function binding"
      with
	  Instantiate.ArityMismatch ->
            prerr_endline ("Arity mismatch in type application (Defunctionalize.Monomorphize.clone)");
            prerr_endline ("expression: "^Show.show show_binding b);
            prerr_endline ("tyargs: "^String.concat "," (List.map Types.string_of_type_arg tyargs));
	    failwith "fatal internal error"

  (* traverse binding list, clone bindings if there are entries
     in the instantiation map *)
  and clone_bindings im bs : binding list * specmap =
    let binding : binding -> binding list * specmap =
      fun b ->
	let ps = 
	match b with
	  | `Let _ -> [[b], []]
	  | `Fun _ 
	  | `PFun _ ->
	      begin
		match IntMap.lookup (var_of_binding b) im with
		  | Some tyarg_lists ->
		      List.map (clone b) tyarg_lists
		  | None -> [[b], []]
	      end
	in
	let bindings, mappings = List.split ps in
	  List.flatten bindings, List.flatten mappings
    in
    let bindings, varmapping = List.split (List.map binding bs) in
      List.flatten bindings, List.flatten varmapping
 
  (* replace variabled with their specialized equivalents *)
  let rec replace_spec_binding specmap b =
    match b with
      | `Let (binder, tyvars, tc) -> 
	  (* Debug.f "replace let %d" (Var.var_of_binder binder); *)
	  `Let (binder, tyvars, replace_spec specmap tc)
      | `Fun (binder, arg_binders, body, tyvars) -> 
	  `Fun (binder, arg_binders, replace_spec specmap body, tyvars)
      | `PFun _ -> b

  and replace_spec (specmap : specmap) (q : qr) : qr =
    let rec replace q =
      match q with
	| `Constant _ | `Database _ | `Table _ | `Wrong _ -> q
	| `Project (label, v) -> `Project (label, replace v)
	| `Extend (extend_fields, r) -> `
	    Extend (StringMap.map replace extend_fields, opt_map replace r)
	| `Erase (labels, v) -> `Erase (labels, replace v)
	| `Inject (tag, v, t) -> `Inject (tag, replace v, t)
	| `TAbs (tyvars, v) -> `TAbs (tyvars, replace v)
	| `List xs -> `List (List.map replace xs)
	| `Apply (f, args) -> `Apply (replace f, List.map replace args)
	| `Case (v, cases, default) ->
	    let v = replace v in
	    let case (binder, body) = (binder, replace body) in
	    let cases = StringMap.map case cases in
	    let default = opt_map case default in
	      `Case (v, cases, default)
	| `If (c, t, e) -> `If (replace c, replace t, replace e)
	| `Computation (bs, tc) -> 
	    `Computation (List.map (replace_spec_binding specmap) bs, replace tc)
	| `Variable var -> `Variable var
	| `TApp (`Variable var, tyargs) -> 
	    begin
	      match specmap_lookup (var, tyargs) specmap with
		| None -> q
		| Some new_var -> `Variable new_var
	    end
	| `TApp _ -> failwith ("Monomorphize.replace: `TApp on non-variable " ^ (Show.show show_qr q));
    in
      replace q

  let rec specialize (im : InstMap.t) q =
    match q with
      | `Computation (bs, tc) ->
	  let bs, specmap = clone_bindings im bs in
	  let bs' = List.map (replace_spec_binding specmap) bs in
	  let tc' = replace_spec specmap tc in 
	    (*Debug.print (Show.show show_specmap specmap); *)
	  (* TODO: restrict im by names which have already been handled *)
(*	  let tc' = specialize (InstMap.restrict im cloned) tc' in *)
	  let tc' = specialize im tc' in
	    `Computation (bs', tc')
      | `Constant _ | `Database _ | `Table _ | `Wrong _ | `Variable _ -> q
      | `Project (label, v) -> `Project (label, specialize im v)
      | `Extend (extend_fields, r) -> `
	  Extend (StringMap.map (specialize im) extend_fields, opt_map (specialize im) r)
      | `Erase (labels, v) -> `Erase (labels, specialize im v)
      | `Inject (tag, v, t) -> `Inject (tag, specialize im v, t)
      | `TAbs (tyvars, v) -> `TAbs (tyvars, specialize im v)
      | `List xs -> `List (List.map (specialize im) xs)
      | `Apply (f, args) -> `Apply (specialize im f, List.map (specialize im) args)
      | `Case (v, cases, default) ->
	  let v = specialize im v in
	  let case (binder, body) = (binder, specialize im body) in
	  let cases = StringMap.map case cases in
	  let default = opt_map case default in
	    `Case (v, cases, default)
      | `If (c, t, e) -> `If (specialize im c, specialize im t, specialize im e)
      | `TApp (v, tyargs) -> `TApp (specialize im v, tyargs)

  let rec monomorphize q =
    let im = instmap q in
    let q = specialize im q in
    let q = ElimDeadDefs.eliminate (Census.count q) q in
      Debug.print (Show.show show_qr q);
      q
      
end

module TypeMap =
struct
  type 'a t = (Types.datatype * 'a) list

  let rec assoc k l =
    match l with
      | [] -> None
      | (t, v) :: l -> 
	  if eq_types_mod_effects k t then
	    Some v
	  else
	    assoc k l

  let rec update k v l =
    match l with
      | [] -> [(k, v)]
      | (k', _) :: l when eq_types_mod_effects k k' -> (k, v) :: l
      | e :: l -> e :: (update k v l)

  let add k v l = (k, v) :: l

  let empty = []
end

module Defunctionalize =
struct

(*
  type defunctx = { 
    cloned_funs : Var.var IntMap.t;
    dispatchs : (Types.datatype * Var.var) list;
    dispatch_defs : (Var.var * funct) list;
    tagctrs : int TypeMap.t
    (* clostypes : (Types.datatype * Types.datatype) list *)
  }
*)

  let replace_var ms (q : qr) : qr =
    let rec replace_binding b =
      match b with
	| `Let (binder, tyvars, tc) -> 
	    (* Debug.f "replace let %d" (Var.var_of_binder binder); *)
	    `Let (binder, tyvars, replace tc)
	| `Fun (binder, arg_binders, body, tyvars) -> 
	    `Fun (binder, arg_binders, replace body, tyvars)
	| `PFun _ -> b
    and replace q =
      match q with
	| `Constant _ | `Database _ | `Table _ | `Wrong _ -> q
	| `Project (label, v) -> `Project (label, replace v)
	| `Extend (extend_fields, r) -> `
	    Extend (StringMap.map replace extend_fields, opt_map replace r)
	| `Erase (labels, v) -> `Erase (labels, replace v)
	| `Inject (tag, v, t) -> `Inject (tag, replace v, t)
	| `TAbs (tyvars, v) -> `TAbs (tyvars, replace v)
	| `List xs -> `List (List.map replace xs)
	| `Apply (f, args) -> `Apply (replace f, List.map replace args)
	| `Case (v, cases, default) ->
	    let v = replace v in
	    let case (binder, body) = (binder, replace body) in
	    let cases = StringMap.map case cases in
	    let default = opt_map case default in
	      `Case (v, cases, default)
	| `If (c, t, e) -> `If (replace c, replace t, replace e)
	| `Computation (bs, tc) -> 
	    `Computation (List.map replace_binding bs, replace tc)
	| `Variable var -> 
	    begin
	      match lookup var ms with
		| Some var' -> `Variable var'
		| None -> `Variable var
	    end
	| `TApp (v, tyargs) -> `TApp (replace v, tyargs)
    in
      replace q

  (* fold over binding list 
     2. replace applications/variables
     1. convert/clone current fun-binding -> new maps
  *)

  let cloned_funs = ref IntMap.empty
      
  (* map type to int *)
  let tagctrs = ref TypeMap.empty

  let dispatch_vars = ref TypeMap.empty

  let dispatch_funs = ref IntMap.empty

  let new_tag typ name =
    let i =
      match TypeMap.assoc typ !tagctrs with
	| Some i -> i
	| None -> 0
    in
      (* let ctx = { ctx with tagctrs = TypeMap.update typ (i + 1) tagctrs } in *)
      tagctrs := TypeMap.update typ (i + 1) !tagctrs;
      name ^ "+++" ^ (string_of_int i)

  let update_dispatch_fun (ty : Types.datatype) origfun tag freevars : unit =
    let (binder, arg_binders, body, _tyvars) = origfun in
    let (dispatch_binder, dispatch_arg_binders, dispatch_body, tyvars) =
      match TypeMap.assoc ty !dispatch_vars with
	| Some f -> val_of (IntMap.lookup f !dispatch_funs)
	| None ->
	    (* create empty initial dispatch function *)
	    let name = "dispatch_" ^ (Types.string_of_datatype ty) in
	    let binder = Var.fresh_binder (`Not_typed, name, `Local) in
	    let tag_binder, tag_var = Var.fresh_var (`Not_typed, "", `Local) in
	    let arg_binders = tag_binder :: arg_binders in
	    let body = `Case (`Variable tag_var, StringMap.empty, None) in
	      (binder, arg_binders, body, [])
    in
    let case_binder, tagged_var = Var.fresh_var (`Not_typed, "", `Local) in
    let varmappings = List.combine (List.map fst arg_binders) (List.map fst (drop 1 dispatch_arg_binders)) in
    let body = replace_var varmappings body in
    let freevar_binding (var, i) =
      let binder = (var, (`Not_typed, "", `Local)) in
      `Let (binder, [], `Project (string_of_int i, `Variable tagged_var))
    in
    let bindings = List.map freevar_binding freevars in
    let body = `Computation (bindings, body) in
    let dispatch_body =
      match dispatch_body with
	| `Case (v, cases, default) ->
	    `Case (v, StringMap.add tag (case_binder, body) cases, default)
	| _ -> failwith "non-case body in dispatch function"
    in
    let dispatch_fun = (binder, arg_binders, dispatch_body, tyvars) in
      (* { ctx with dispatchs = TypeMap.update ty dispatch_fun dispatchs } *)
      Debug.print (Show.show (Show.show_list Types.show_datatype) (List.map fst !dispatch_vars));
      dispatch_vars := TypeMap.update ty (Var.var_of_binder dispatch_binder) !dispatch_vars;
      dispatch_funs := IntMap.add (Var.var_of_binder dispatch_binder) dispatch_fun !dispatch_funs
    
  let update_dispatch_pfun _ctx (_ty : Types.datatype) (_binder, _dispatch) _tag : unit = ()

  let clone_primitive ((_f, (t, fname, loc)), dispatch) =
    (Var.fresh_binder (t, fname ^ "_orig", loc), dispatch)

  let clone (f : funct) : funct * Var.var =
    let (_f, (t, fname, loc)), arg_binders, body, tyargs = f in
    let binder, f' = Var.fresh_var (t, fname ^ "_orig", loc) in
      (binder, arg_binders, body, tyargs), f'

  let rec convert_binding : Types.datatype Env.Int.t -> binding -> binding list -> binding list = 
    fun tyenv b bs ->
      match b with
	| `Let (binder, tyvars, tc) ->
	    (`Let (binder, tyvars, convert tyenv tc)) :: bs
	    (* convert body with ctx *)
	    (* refresh type? *)
	| `Fun (((var, (t, name, _)), arg_binders, body, _tyvars) as funct) ->
	    let body = convert tyenv body in
	    let tag = new_tag t name in
	    let freevars = FreeVars.freevars (IntSet.from_list (List.map fst arg_binders)) body in
	    let freevars = List.sort compare (IntSet.elements freevars) in
	    let freevars_numbered = mapIndex (fun v i -> (v, i)) freevars in
	    let tag_fields = 
	      List.fold_left 
		(fun m (v, i) -> StringMap.add (string_of_int i) (`Variable v) m)
		StringMap.empty
		freevars_numbered
	    in
	    let tag_record = `Extend (tag_fields, None) in
	    let variant = `Inject (tag, tag_record, `Not_typed) in
	    let cloned_fun, cloned_var = clone funct in
	    let defun_binding = `Let ((var, (`Not_typed, "", `Local)), [], variant) in
	      cloned_funs := IntMap.add var cloned_var !cloned_funs;
	      update_dispatch_fun t funct tag freevars_numbered;
	      (`Fun cloned_fun) :: defun_binding :: bs
	      
	| `PFun ((var, _), _dispatch) ->
	    cloned_funs := IntMap.add var var !cloned_funs;
	    b :: bs
	    (*
	      FIXME
	      clone primitive
	      if primitive is higher-order, add dispatch-function
	      add arm to case expression of dispatch-function
	    *)

  and bindings tyenv bs =
      List.fold_right (convert_binding tyenv) bs []

  and convert_application tyenv (fexp : qr) (argexps : qr list) : qr =
    let t =
      match fexp with
	| `Variable f | `TApp (`Variable f, _) -> Env.Int.lookup tyenv f
	| _ -> type_qr tyenv fexp
    in
    let dispatch_fun = 
      begin
	Debug.print ("lookup dispatch_vars " ^ (Show.show Types.show_datatype t));
	match TypeMap.assoc t !dispatch_vars with
	  | Some dispatch -> dispatch
	  | None -> assert false
      end
    in
      `Apply (`Variable dispatch_fun, fexp :: argexps)

  and convert (tyenv : Types.datatype Env.Int.t) (q : qr) : qr = 
    match q with
      | `Computation (bs, tc) ->
	  let bs = bindings tyenv bs in
	  let tc = convert tyenv tc in
	    `Computation (bs, tc)
      | `Constant _ | `Database _ | `Table _ | `Wrong _ | `Variable _ -> q
      | `Project (label, v) -> `Project (label, convert tyenv v)
      | `Extend (extend_fields, r) -> `
	  Extend (StringMap.map (convert tyenv) extend_fields, opt_map (convert tyenv) r)
      | `Erase (labels, v) -> `Erase (labels, convert tyenv v)
      | `Inject (tag, v, t) -> `Inject (tag, convert tyenv v, t)
      | `TAbs (tyvars, v) -> `TAbs (tyvars, convert tyenv v)
      | `List xs -> `List (List.map (convert tyenv) xs)
      | `Apply (`Variable f, args)
      | `Apply (`TApp (`Variable f, _), args) ->
	  (* FIXME: convert f, args before converting the application itself *)
	  begin
	    match IntMap.lookup f !cloned_funs with
	      | Some f' -> `Apply (`Variable f', args)
	      | None -> convert_application tyenv (`Variable f) args
	  end
      | `Apply (f, args) ->
	  convert_application tyenv f args
      | `Case (v, cases, default) ->
	  let v = convert tyenv v in
	  let case (binder, body) = (binder, convert tyenv body) in
	  let cases = StringMap.map case cases in
	  let default = opt_map case default in
	    `Case (v, cases, default)
      | `If (c, t, e) -> `If (convert tyenv c, convert tyenv t, convert tyenv e)
      | `TApp (v, tyargs) -> `TApp (convert tyenv v, tyargs)

  let defunctionalize tyenv q : qr =
    let _, tyenv = complete_tyenv tyenv q in
      Debug.print (Show.show (Env.Int.show_t Types.show_datatype) tyenv);
    let q = convert tyenv q in
    let dispatch_bs = 
      IntMap.fold
	(fun _var funct bs -> (`Fun funct) :: bs)
	!dispatch_funs
	[]
    in
      match q with
	| `Computation (bs, tc) ->
	    `Computation (dispatch_bs @ bs, tc)
	| _ -> `Computation (dispatch_bs, q)
end

let optphase tyenv q =
  let census = Census.count q in
    Debug.print ">>>>> inliner";
    let ctx = { Inliner.venv = Env.Int.empty; 
		Inliner.fenv = Env.Int.empty;
		Inliner.tyenv = tyenv; 
		Inliner.census = census; } 
    in
    let q = Inliner.inline ctx q in
    let census = Census.count q in
      let q = ElimDeadDefs.eliminate census q in
	Debug.print ("inlined\n" ^ (Show.show show_qr q));
	q

let rec applyn f arg n =
  if n = 1 then
    f arg
  else if n > 1 then
    f (applyn f arg (n-1))
  else
    arg

let pipeline q tyenv =
  Debug.print ("before\n" ^ (Show.show show_qr q));
  let _optphase = optphase tyenv in
(*  let q = applyn optphase q 2 in *)
    (*ignore (Qr.type_qr tyenv q); *)
  let q = optphase tyenv q in
  let q = Monomorphize.monomorphize q in
    Defunctionalize.defunctionalize tyenv q
