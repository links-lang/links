(*pp deriving *)

open Utility
open Ir

let complete_tyenv tyenv p =
  let _, _, o = (new Transform.visitor tyenv)#computation p in
    o#get_type_environment

type t =   
  | Value of Ir.value
  | Fun of (binder * (tyvar list * binder list * computation) * location)
      deriving (Show)

module OptimizeQuery =
struct

  let rec is_inlineable_value census v =
    Debug.print ("v " ^ (Show.show Ir.show_value v));
    match v with
      | `TApp (`Variable var, _)
      | `Variable var -> (IntMap.find var census) < 3 
      | v when is_atom v -> true
      | `ApplyPure _ -> true
      | `Project (_, v)
      | `Erase (_, v)
      | `Inject (_, v, _) -> is_inlineable_value census v
      | `Extend (fields, Some r) ->
	  let p _ v  = is_inlineable_value census v in
	    StringMap.for_all p fields && is_inlineable_value census r
      | `Extend (fields, None) ->
	  let p _ v  = is_inlineable_value census v in
	    StringMap.for_all p fields 
      | _ -> false

  let is_inlineable_fun v census = 
    match IntMap.lookup v census with
      | Some i -> i < 3
      | None -> false

  (* align arguments with formal parameters and create bindings *)
  let arg_lets xs args =
    let arg_let (x, arg) =
      let x_var, (_x_t, x_s, x_scope) = x in
      let arg_val, arg_t, _arg_o = arg in
      let binder = (x_var, (arg_t, x_s, x_scope)) in
	`Let (binder, ([], `Return arg_val))
    in 
      List.map arg_let (List.combine xs args)

  let has_value env var =
    match Env.Int.find env var with
      | Some (Value _) -> true
      | _ -> false

  let remove_fields fset m =
    let p f _ = not (StringSet.mem f fset) in
      StringMap.filter p m

  let inliner tyenv env census =
  object (o)
    inherit Transform.visitor(tyenv) as super

    val env = env

    method with_env env =
      {< env = env >}

    method get_env = env

    method value =
      fun v ->
	Debug.print ("value " ^ (Show.show Ir.show_value v));
	Debug.print ("env " ^ (Show.show (Env.Int.show_t show_t) o#get_env));
	let v, t, o = super#value v in
	  match v with
	    | `TApp (`Variable var, _)
            | `Variable var when Env.Int.has env var -> 
		(* inline values *)
		begin
		  match Env.Int.lookup env var with
		    | Value v -> 
			Debug.f "inline value %d" var;
			v, o#lookup_type var, o
		    | Fun _ -> `Variable var, o#lookup_type var, o
		end
	    | `Project (label, r) ->
		(* project from known record values *)
		begin
		  let r, _rt, o = o#value r in
		    match r with
		      | `Extend (fields, _) ->
			  let v = from_option v (StringMap.lookup label fields)  in
			    v, t, o
		      | `Erase (labels, r) ->
			  begin
			    let r, _rt, o = o#value r in
			      match r with
				| `Extend (fields, _) ->
				    let rem_fields = (remove_fields labels fields) in
				    let v = from_option v (StringMap.lookup label rem_fields) in
				      v, t, o
				| _ -> v, t, o
			  end
		      | _ -> v, t, o
		end
	    | `Extend (outer_fields, outer_r) ->
		(* extend known record values *)
		begin
		  match outer_r with
		    | Some outer_r -> 
			begin
			  let outer_r, _rt, o = o#value outer_r in
			    match outer_r with
			      | `Extend (inner_fields, inner_r) ->
				  let fields = StringMap.union_disjoint outer_fields inner_fields in
				    `Extend (fields, inner_r), t, o
			      | _ -> v, t, o
			end
		    | None -> v, t, o
		end
	    | `Erase (labels, r) ->
		(* erase from known record values *)
		let r, _rt, o = o#value r in
		  begin
		    match r with
		      | `Extend (fields, Some inner_r) ->
			  let rdomain = StringSet.from_list (StringMap.domain fields) in
			  let remaining_labels = StringSet.diff labels rdomain in
			  let remaining_fields = remove_fields labels fields in
			    if (StringSet.cardinal remaining_labels) > 0 then
			      `Extend (remaining_fields, Some (`Erase (remaining_labels, inner_r))), t, o
			    else
			      `Extend (remaining_fields, Some inner_r), t, o
		      | `Extend (fields, None) ->
			  `Extend (remove_fields labels fields, None), t, o
		      | _ -> v, t, o
		  end
	    | _ -> v, t, o

    method bindings =
      function
        | b :: bs ->
            let b, o = o#binding b in
              begin
		let pre_bs, b, o = 
		  match b with
		    | `Let (binder, (tyvars, tc)) ->
			let (body_bs, body_tc), _t, o = o#computation ([], tc) in
			let b = `Let (binder, (tyvars, body_tc)) in
			let body_bs', o = o#bindings body_bs in
			  body_bs', b, o
		    | _ -> [], b, o
		in
		  Debug.print ("binding " ^ (Show.show Ir.show_binding b));
                  match b with
                    | `Let ((x, (_, _, `Local)), (tyvars, `Return v)) 
		    | `Let ((x, (_, _, `Local)), (tyvars, `Return (`TApp (v, _)))) when is_inlineable_value census v ->
			Debug.f "let %d" x;
			(* bind inlineable values to the environment *)
			let v =
                          match tyvars with
                            | [] -> v
                            | tyvars -> `TAbs (tyvars, v)
			in
			let env' = Env.Int.bind env (x, (Value (fst3 (o#value v)))) in
			let bs, o = (o#with_env env')#bindings bs in
			  pre_bs @ (b :: bs), o

		    | `Fun ((v, _) as f, (tyvars, xs, body), location) when is_inlineable_fun v census ->
			(* bind inlineable functions to the environment *)
			Debug.f "fun %d" v;
			let body', _, o = o#computation body in
			let func = Fun (f, (tyvars, xs, body'), location) in
			let env' = Env.Int.bind env (v, func) in
			let bs, o = (o#with_env env')#bindings bs in
			  pre_bs @ (b :: bs), o

                    | _ ->
			let bs, o = o#bindings bs in
                          pre_bs @ (b :: bs), o
              end
        | [] -> [], o

    method computation =
      fun (bs, tc) ->
	Debug.print "computation";
	Debug.print ("env " ^ (Show.show (Env.Int.show_t show_t) o#get_env));
	let bs, o = o#bindings bs in
	  Debug.print ("tc " ^ (Show.show Ir.show_tail_computation tc));
	let tc, t, o = o#tail_computation tc in
	  Debug.print ("env " ^ (Show.show (Env.Int.show_t show_t) o#get_env));
	  Debug.print ("tc " ^ (Show.show Ir.show_tail_computation tc));
	  match tc with
	    | `Apply (f, args) ->
		(* inline known functions (beta) *)
		begin
		  let f, t, o = o#value f in
		    match f with
		      | `Variable f'
		      | `TApp (`Variable f', _) when Env.Int.has o#get_env f' ->
			  begin
			    match Env.Int.lookup o#get_env f' with
			      | Fun (_fb, (_tyvars, xs, (body_bs, body_tc)), _location) ->
				  Debug.f "inline function %d" f';
				  let args' = List.map o#value args in
				  let arg_bs = arg_lets xs args' in
				  let (bs', tc), t, o = o#computation ((arg_bs @ body_bs), body_tc) in
				    (bs @ bs', tc), t, o
			      | Value _ -> 
				  (bs, tc), t, o
			  end
		      | _ -> (bs, tc), t, o
		end
	    | `Case (c, cases, default) ->
		(* inline case expressions over known variant values *)
		begin
		  let c, _ct, o = o#value c in
		    match c with
		      | `Inject (tag, cv, _t) ->
			  let cv, cvt, o = o#value cv in
			  let (binder, (case_bs, case_tc)) = 
			    match StringMap.lookup tag cases, default with
			      | Some (binder, (case_bs, case_tc)), _
			      | None, Some (binder, (case_bs, case_tc)) ->
				  binder, (case_bs, case_tc)
			      | None, None -> assert false
			  in
			  let arg_bs = arg_lets [binder] [cv, cvt, o] in
			  let (bs', tc), t, o = o#computation ((arg_bs @ case_bs), case_tc) in
			    (bs @ bs', tc), t, o
		      | _ -> (bs, tc), t, o
		end
	    | `If (c, tcomp, ecomp) ->
		(* inline if expressions over known boolean values *)
		begin
		  let c, _ct, o = o#value c in
		    match c with
		      | `Constant (`Bool true) ->
			  let (bs', tc), t, o = o#computation tcomp in
			    (bs @ bs', tc), t, o
		      | `Constant (`Bool false) ->
			  let (bs', tc), t, o = o#computation ecomp in
			    (bs @ bs', tc), t, o
		      | _ -> (bs, tc), t, o
		end
	    | `Return v ->
		let v, t, o = o#value v in
		  (bs, `Return v), t, o
	    | tc -> 
		(bs, tc), t, o
  end

let program typing_env p census =
  fst3 ((inliner typing_env Env.Int.empty census)#computation p)
end

module Census =
struct

  let count tyenv cm =
  object (_)
    inherit Transform.visitor(tyenv) as super

    val cm = cm

    method get_cm = cm

    method incr var =
      let i' = 
	match IntMap.lookup var cm with
	  | Some i -> (i + 1)
	  | None -> 1
      in
	{< cm = IntMap.add var i' cm >}

    method value =
      function
	| `Variable var -> 
	    let e, t, o = super#value (`Variable var) in
	      e, t, o#incr var
	| v -> super#value v
  end

  let program tenv p = 
    (thd3 ((count tenv IntMap.empty)#program p))#get_cm

end

let rec applyn f arg n =
  if n = 1 then
    f arg
  else if n > 1 then
    f (applyn f arg (n-1))
  else
    arg

let pipeline tenv program = 
  let phase p = 
    Debug.print "phase";
    Ir.ElimDeadDefs.program tenv (OptimizeQuery.program tenv p (Census.program tenv p)) 
  in
(*  let p = Ir.ElimDeadDefs.program tenv (phase (phase (phase program))) in *)
  let p = applyn phase program 1 in
    Debug.print "opt finished";
    p
