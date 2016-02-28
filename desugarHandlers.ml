open Utility
open Sugartypes

(*

 [open] [shallow]handler(m) {
    case Op_i(p_i,k_i) -> ...
    case Return(x) -> ...
  } 
  =>
  fun(m) {
    handle(m) {
      case Op_i(p_i,k_i) -> ...
      case Return(x) -> ...
    }
  }

*)


let dp = Sugartypes.dummy_position

(* Computes the set of names in a given pattern *)
let rec names : pattern -> string list
  = fun (pat,_) ->
    match pat with
      `Variant (_,pat_opt) -> opt_app names [] pat_opt
    | `Record (name_pats,pat_opt) ->
       let optns = opt_app names [] pat_opt in
       (List.fold_left (fun ns p -> (names p) @ ns) [] (List.map snd name_pats)) @ optns
    | `Variable (name,_,_)        -> [name]
    | `Cons (pat,pat')            -> (names pat) @ (names pat')
    | `Tuple pats
    | `List pats                  -> List.fold_left (fun ns pat -> (names pat) @ ns ) [] pats   
    | `Negative ns'               -> List.fold_left (fun ns n -> n :: ns) [] ns'
    | `As  ((name,_,_),pat)       -> [name] @ (names pat)
    | _                           -> []

let resolve_name_conflicts : pattern -> stringset -> pattern
  = fun pat conflicts ->
    let rec hide_names : pattern -> pattern
      = fun (pat,pos) ->
	(begin
	  match pat with
	  | `Variant (label, pat_opt)    -> `Variant (label, opt_map hide_names pat_opt)
	  | `Record (name_pats, pat_opt) -> `Record  (List.map (fun (label, pat) -> (label, hide_names pat)) name_pats, opt_map hide_names pat_opt)
	  | `Variable (name,_,_)         ->
	     if StringSet.mem name conflicts
	     then `Any
	     else pat
	  | `Cons (pat, pat')            -> `Cons (hide_names pat, hide_names pat')
	  | `Tuple pats                  -> `Tuple (List.map hide_names pats)
	  | `List pats                   -> `List (List.map hide_names pats)
	  | `Negative names              -> failwith "desugarHandlers.ml: hide_names `Negative not yet implemented"
	  | `As ((name,t,pos') as n,pat) -> let (p,_) as pat = hide_names pat in
					    if StringSet.mem name conflicts
					    then p
					    else `As (n, pat)
	  | _ -> pat
	 end
	   , pos)
    in hide_names pat
    
  
let parameterize : (pattern * phrase) list -> pattern list option -> (pattern * phrase) list 
  = fun cases params ->
  let wrap_fun params body =
    (`FunLit (None, `Unl, ([params], body), `Unknown), dp)
  in
  match params with
    None
  | Some [] -> cases
  | Some params ->
     List.map (fun (pat, body) ->
       let name_conflicts =
	 let param_names = List.concat (List.map names params) in
	 let pat_names   = names pat in
	 StringSet.inter (StringSet.from_list pat_names) (StringSet.from_list param_names)
       in
       let params = List.map (fun p -> resolve_name_conflicts p name_conflicts) params in
       (pat, wrap_fun params body)
     ) cases
  

let to_var : Sugartypes.patternnode -> Sugartypes.phrasenode
  = fun p ->
  match p with
    `Variable b -> let (name,_,_) = b in
		   `Var name
  | _ -> assert false

let make_handle : Sugartypes.handlerlit -> Sugartypes.handler_spec -> Sugartypes.funlit
  = fun (m, cases, params) spec ->
  let pos = snd m in
  let (m_name,_,_) = 
	 match m with
	   `Variable b, _ -> b
	 | _ -> assert false
  in       
  let mvar = (`Var m_name, pos) in
  let cases = parameterize cases params in
  let desc  = (spec, None) in
  let handle : phrase = `Block ([], (`Handle (mvar, cases, desc), pos)),pos in
  let body =
    match params with
      None -> handle
    | Some params ->
      let params = List.map (fun (p,pos) -> (to_var p, pos)) params in
      `FnAppl (handle, params),dp
  in
  let fnparams =
    match HandlerUtils.is_closed desc with
      true -> []
    | _ -> [[]]
  in
  let fnparams =
    match params with
      Some params -> [m] :: (params :: fnparams)
    | None -> [m] :: fnparams
  in
  let fnlit = (fnparams, body) in
  fnlit
			     
let desugar_handlers_early =
object
  inherit SugarTraversals.map as super
  method phrasenode = function
    | `HandlerLit (spec, hnlit) ->      
       let handle = make_handle hnlit spec in
       let funlit : Sugartypes.phrasenode = `FunLit (None, `Unl, handle, `Unknown) in
       super#phrasenode funlit
    | e -> super#phrasenode e

  method bindingnode = function
    | `Handler (binder, spec, hnlit, annotation) ->
       let handle  = make_handle hnlit spec in
       `Fun (binder, `Unl, ([], handle), `Unknown, annotation)
    | b -> super#bindingnode b
end
