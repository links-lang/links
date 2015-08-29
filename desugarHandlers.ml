open Utility
open Sugartypes

(*

 [open|shallow] handler(m) {
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

let parameterize : (pattern * phrase) list -> pattern list option -> (pattern * phrase) list 
  = fun cases params ->
  let rec get_names_opt optpat names =
    match optpat with
      Some pat -> get_names pat names
    | None -> names
  and get_names ((pat,_) : pattern) names =
    match pat with
      `Variant (_,pat_opt) -> get_names_opt pat_opt names
    | `Record (name_pats,pat_opt) -> StringSet.union_all [(List.fold_left (fun names p -> get_names p names) names (List.map snd name_pats)); (get_names_opt pat_opt names)]
    | `Variable (name,_,_) -> StringSet.add name names
    | `Cons (pat,pat') -> StringSet.union_all [get_names pat names; get_names pat' names]
    | `Tuple pats
    | `List pats -> List.fold_left (fun names pat -> get_names pat names) names pats   
    | `Negative names' -> List.fold_left (fun names name -> StringSet.add name names) names names'
    | `As  ((name,_,_),pat) -> StringSet.add name (get_names pat names)
    | _ -> names
  in
  let find_name_clashes pat params =
    let param_names = List.fold_left (fun names pat -> get_names pat names) StringSet.empty params in
    let pat_names = get_names pat StringSet.empty in
    StringSet.inter param_names pat_names
  in
  let fix_clashes params clashes =
    let rec hide_names_opt optpat names =
      match optpat with
	Some pat -> Some (hide_names pat names)
      | None -> None
    and hide_names (pat, pos) names =
      match pat with
	`Variant (name,pat_opt) -> `Variant (name, hide_names_opt pat_opt names),pos
      | `Record (name_pats,pat_opt) -> `Record (List.map (fun (n,pat) -> (n,hide_names pat names)) name_pats, hide_names_opt pat_opt names),pos
      | `Variable (name,_,_) -> if StringSet.mem name names then
				  `Any,pos
				else
				  (pat,pos)
      | `Cons (pat,pat') -> `Cons (hide_names pat names, hide_names pat' names),pos
      | `Tuple pats
      | `List pats -> `List (List.map (fun pat -> hide_names pat names) pats),pos
      | `Negative names' -> failwith "desugarHandlers.ml: How to hide negative pattern names? Can the case ever occur?"
      | `As  ((name,t,pos'),pat) -> let pat = hide_names pat names in
				    if StringSet.mem name names then
				      `As (("_" ^ name,t,pos'),pat),pos (* Todo: Figure out whether this is *safe*, e.g. _name*)
				    else
				      `As ((name,t,pos'),pat),pos
      | _ -> (pat,pos)     
    in
    List.map (fun param -> hide_names param clashes) params
  in
  let wrap_fun params body =
    (`FunLit (None, `Unl, ([params], body)), dp)
  in
  match params with
    None
  | Some [] -> cases
  | Some params  -> List.map (fun (pat, ph) ->
			      let name_clashes = find_name_clashes pat params in
			      let params = fix_clashes params name_clashes in
			      (pat, wrap_fun params ph)
			     ) cases
  

let to_var : Sugartypes.patternnode -> Sugartypes.phrasenode
  = fun p ->
  match p with
    `Variable b -> let (name,_,_) = b in
		   `Var name
  | _ -> assert false

let make_handle : Sugartypes.handlerlit -> Sugartypes.handler_spec -> Sugartypes.funlit
  = fun (m, cases, params) spec ->
  let (m_name,_,_) = 
	 match m with
	   `Variable b, _ -> b
	 | _ -> assert false
  in       
  let mvar = (`Var m_name, dp) in
  let cases = parameterize cases params in
  let handle : phrase = `Block ([], (`Handle (mvar, cases, None, spec), dp)),dp in
  let body =
    match params with
      None -> handle
    | Some params ->
      let params = List.map (fun (p,pos) -> (to_var p, pos)) params  in
      `FnAppl (handle, params),dp
  in
  let fnparams =
    match spec with
      `Open -> [[]]
    | _ -> []
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
    | `HandlerLit (None, spec, hnlit) ->      
       let handle = make_handle hnlit spec in
       let funlit : Sugartypes.phrasenode = `FunLit (None, `Unl, handle) in       
       super#phrasenode funlit
    | e -> super#phrasenode e

  method bindingnode = function
      `Handler (binder, spec, hnlit, annotation) ->
      let handle  = make_handle hnlit spec in
      `Fun (binder, `Unl, ([], handle), `Unknown, annotation)
    | b -> super#bindingnode b
end
