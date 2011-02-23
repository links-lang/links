open Utility
open Qr

module Census =
struct
  let counter tyenv =
  object (_)
    inherit Transform.visitor(tyenv) as super

    val cm = IntMap.empty
    
    method get_cm = cm

    method! var var =
      let prev = 
	match IntMap.lookup var cm with
	  | Some c -> c
	  | None -> 0
      in
	var, {< cm = IntMap.add var (prev + 1) cm >}
  end

  let census tyenv q =
    let _, _, o = (counter tyenv)#qr q in
      o#get_cm
end

module ElimDeadDefs =
struct
  let eliminator tyenv census =
  object (o)
    inherit Transform.visitor(tyenv) as super

    val census = census

    method! bindings : bindings -> bindings * 'self_type = fun bs ->
      List.fold_right
	(fun (((x, (t, _, _)), _tyvars , _q) as b) (bs, o) ->
	   match IntMap.lookup x census with
	     | Some c when c > 0 ->
		 let env = Env.Int.bind tyenv (x, t) in
		   b :: bs, o#with_tyenv env
	     | Some _ | None ->
		 bs, o)
	bs
	([], o)

    method! qr q =
      let q, t, o = super#qr q in
	match q with
	  | `Let (bs, tc) -> 
	      let bs, o = o#bindings bs in
		begin
		  match bs with
		    | [] -> tc, t, o
		    | bs -> `Let (bs, tc), t, o
		end
	  | _ -> q, t, o

  end

  let eliminate tyenv census q =
    fst3 ((eliminator tyenv census)#qr q)
end

module Inliner =
struct

  type ctx = { env : qr Env.Int.t; 
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
	      match Env.Int.find ctx.env name with
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
	| `Let ([], tc) -> inl tc
	| `Let (bs, tc) ->
	    let bs, env = bindings ctx bs in
	    let tc = inline { ctx with env = env } tc in
	      `Let (bs, tc)
	| `List xs -> 
	    `List (List.map inl xs)
	| `Fun (binders, tyvars, body, t) ->
	    let body = inl body in
	      `Fun (binders, tyvars, body, t)
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
    let binding ((name, _) as binder, tyvars, tc) (bs, env) =
      let tc = inline { ctx with env = env } tc in
      let env = 
	if is_inlineable ctx.census name then
	  match tyvars with
	    | [] -> Env.Int.bind env (name, tc)
	    | tyvars -> Env.Int.bind env (name, `TAbs (tyvars, tc))
	else
	  env
      in
	(binder, tyvars, tc) :: bs, env
    in
      List.fold_right binding bs ([], ctx.env)

  and beta ctx args pre_bindings binders body =
    try
      let arg_bindings = List.map2 (fun binder arg -> (binder, [], arg)) binders args in
	inline ctx (`Let (pre_bindings @ arg_bindings, body))
    with Invalid_argument _ -> failwith "arity mismatch in function inlining"

  and apply ctx f args =
      match f with
	| `Fun (binders, _tyvars, body, _t)
	(* FIXME: maintain typability: instantiate type, if vars remain, introduce tabs *)
	| `TApp (`Fun (binders, _tyvars, body, _t), _) -> beta ctx args [] binders body
	| `Let (bs, `Fun (binders, _tyvars, body, _t)) -> beta ctx args bs binders body
	| _ -> `Apply (f, args)

end

module Monomorphize =
struct
  let monomorphizer tyenv =
  object (_o)
    inherit Transform.visitor(tyenv) as super

  end
end

module Defunctionalize =
struct
  let defunctionalizer tyenv =
  object (_o)
    inherit Transform.visitor(tyenv) as super
  end
end

let optphase tyenv q =
  let census = Census.census tyenv q in
    Debug.print ">>>>> inliner";
    let ctx = { Inliner.env = Env.Int.empty; Inliner.tyenv = tyenv; Inliner.census = census } in
    let q = Inliner.inline ctx q in
    let census = Census.census tyenv q in
      let q = ElimDeadDefs.eliminate tyenv census q in
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
    applyn optphase q 3
