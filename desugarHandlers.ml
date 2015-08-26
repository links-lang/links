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

let parameterize cases params =
  let wrap_fun params body =
    (`FunLit (None, `Unl, ([params], body)), dp)
  in
  match params with
    None
  | Some [] -> cases
  | Some params  -> List.map (fun (pat, ph) ->
		    let pat = pat in
		    (pat, wrap_fun params ph)
		   ) cases
  

let make_handle : Sugartypes.handlerlit -> Sugartypes.handler_spec -> Sugartypes.funlit
  = fun (m, cases, params) spec ->
  let (m_name,_,_) =
	 match m with
	 | `Variable b, _ -> b
	 | _ -> assert false
       in       
       let mvar = (`Var m_name, dp) in
       let cases = parameterize cases params in
       let handle = `Block ([], (`Handle (mvar, cases, None, spec), dp)) in
       let body =
	 match spec with
	   `Open ->
	   let body = `Block ([], (`FunLit (None, `Unl, ([[]], (handle, dp))),dp)) in
	   (body, dp)
	 | `Pure
	 | `Closed -> (handle, dp)
       in
       let fnlit = ([[m]], body) in
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
      `Handler (binder, spec, hnlit) ->
      let handle  = make_handle hnlit spec in
      `Fun (binder, `Unl, ([], handle), `Unknown, None)
    | b -> super#bindingnode b
end			     
