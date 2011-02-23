open Utility
open Qr

module Census =
struct
  let counter tyenv =
  object (_)
    inherit Transform.visitor(tyenv) as super

    val cm = IntMap.empty
    
    method get_cm = cm

    method var var =
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

    method bindings : bindings -> bindings * 'self_type = fun bs ->
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

    method qr q =
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
  let inliner tyenv env census =
  object (o)
    inherit Transform.visitor(tyenv) as super

    val env = env

    val census = census

    method bind name value =
      {< env = Env.Int.bind env (name, value) >}

    (* FIXME: create type abstraction if type is quantified over *)
    method bind_if_inlineable name value tyvars =
      match IntMap.lookup name census with
	| Some c when c < 2 -> 
	    let value = 
	      begin
		match tyvars with
		  | [] -> value
		  | tyvars -> 
		      Debug.f "introducing tabs for %d: %s" name (Show.show (Show.show_list Types.show_quantifier) tyvars);
		      `TAbs (tyvars, value)
	      end
	    in
	      {< env = Env.Int.bind env (name, value) >}
	| None | Some _ -> o

    method bindings bs = 
      let bs, o = super#bindings bs in
      List.fold_right
	(fun ((name, _) as binder, tyvars, qr) (bs, o) ->
	   let qr, _, o = o#qr qr in
	   let o = o#bind_if_inlineable name qr tyvars in
	     (binder, tyvars, qr) :: bs, o)
	bs
	([], o)

    method apply : qr -> qr list -> qr * Types.datatype * 'self_type = fun f args ->
      match f with
	| `Fun (binders, _tyvars, body, _) ->
	    (* FIXME: handle typevars for argument bindings *)
	    Debug.print "inline function";
	    let arg_bindings = List.map (fun (b, a) -> (b, [], a)) (List.combine binders args) in
	      o#qr (`Let (arg_bindings, body))
	(* FIXME: handle case/if functional expressions *)
	| _ -> o#qr (`Apply (f, args))

    method qr q =
      let q, t, o = super#qr q in
	match q with
	  | `Apply (f, args) -> 
	      let args, _, o = o#list (fun o -> o#qr) args in
	      let f, _, o = o#qr f in
		o#apply f args
	  | `Constant _ -> q, t, o
	  | `Variable name ->
	      let _, t, o = super#qr q in
		begin
		  match Env.Int.find env name with
		    | Some e -> e, t, o
		    | None -> q, t, o
		end
	| `Extend (extend_fields, r) ->
	    let extend_fields, _, o = o#name_map (fun o -> o#qr) extend_fields in
	    let r, _, o = o#option (fun o -> o#qr) r in
	      `Extend (extend_fields, r), t, o
	| `Project (label, r) ->
	    let r, _, o = o#qr r in
	      `Project (label, r), t, o
	| `Erase (labels, r) ->
	    let r, _, o = o#qr r in
	      `Erase (labels, r), t, o
	| `Inject (tag, v, t) ->
	    let v, _, o = o#qr v in
	      `Inject (tag, v, t), t, o
	| `TApp (v, tyargs) ->
	    let v, _, o = o#qr v in
	      `TApp (v, tyargs), t, o
	| `TAbs (tyvars, v) ->
	    let v, _, o = o#qr v in
	    `TAbs (tyvars, v), t, o
	| `Database _ -> q, t, o
	| `Table _ -> q, t, o
	| `List xs ->
	    let xs, _, o = o#list (fun o -> o#qr) xs in
	      `List xs, t, o
	| `PrimitiveFun _ ->
	    q, t, o
	| `Fun (binders, tyvars, body, ft) ->
	    let body, _, o = o#qr body in
	      `Fun (binders, tyvars, body, ft), t, o
	| `Case (v, cases, default) ->
	    let v, _, o = o#qr v in
	    let cases =
	      StringMap.map
		(fun (binder, body) ->
		   let body, _, _ = o#qr body in
		     (binder, body))
		cases
	    in
	    let default = opt_map (fun (binder, body) -> (binder, fst3 (o#qr body))) default in
	      `Case (v, cases, default), t, o
	| `If (c, tb, eb) ->
	    let c, _, o = o#qr c in
	    let tb, _, o = o#qr tb in
	    let eb, _, o = o#qr eb in
	      `If (c, tb, eb), t, o
	| `Let (bs, tc) ->
	    let bs, o = o#bindings bs in
	    let tc, _, o = o#qr tc in
	      begin
		match tc with
		  | `Let (bsi, tci) ->
		      `Let (bs @ bsi, tci), t, o
		  | tc -> `Let (bs, tc), t, o
	      end
	| `Wrong _ -> q, t, o

  end

  let inline tyenv census q =
    fst3 ((inliner tyenv Env.Int.empty census)#qr q)
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
  let q = 
  ElimDeadDefs.eliminate 
    tyenv 
    (Census.census 
       tyenv 
       q) 
    (Inliner.inline 
       tyenv 
       (Census.census tyenv q)
       q)
  in
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
    applyn optphase q 1
    
