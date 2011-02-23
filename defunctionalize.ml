open Utility
open Qr

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
	| `Fun (_, _, body, _) 
	| `Let (_, _, body) -> 
	    count body
    in
      List.map binding bs
    
    and count q =
    match q with
      | `Variable var -> IntMap.add var 1 IntMap.empty
      | `Constant _ | `Database _ | `Table _
      | `PrimitiveFun _ | `Wrong _ -> IntMap.empty
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
      | `PrimitiveFun _ | `Wrong _ -> q
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
      (fun (free, bound) ((var, _), tc) ->
	 let free = IntSet.union (fv bound tc) free in
	 let bound = IntSet.add var free in
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
	let proj (_label, value) = value in
	let free = name_map bound proj extend_fields in
	  begin
	    match base with
	      | Some r -> IntSet.union (fv bound r) free
	      | None -> free
	  end
    | `Project (_, value) | `Erase (_, value) | `Inject (_, value, _) 
    | `TApp (value, _) | `TAbs (_, value) -> 
	fv bound value
    | `Let (bs, tc) ->
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

module Monomorphize =
struct
end

module Defunctionalize =
struct
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
  let optphase = optphase tyenv in
  let q = applyn optphase q 1 in
    ignore (Qr.type_qr tyenv q);
    q
