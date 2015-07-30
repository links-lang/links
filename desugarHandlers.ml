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

class desugar_handlers env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) =
    function
    | `HandlerLit (Some (effects, body_type, ht), spec, (m, cases)) ->
       let (m_name,_,_) =
	 match m with
	 | `Variable b, _ -> b
	 | _ -> assert false
       in
       let (mt,m_ops) =
	 match TypeUtils.arg_types ht with
	   [mt] -> (mt, TypeUtils.effect_row mt)
	 | _ -> assert false
       in
       let mvar = (`Var m_name, dp) in
       let handle = `Handle (mvar, cases, Some (body_type, effects), spec) in
       let body =
	 match spec with
	   `Open ->
	   let unit = Types.unit_type in
	   let emptyeff = Types.make_empty_open_row (`Unl, `Any) in
	   `FunLit (Some [(unit,emptyeff)], `Unl, ([[]], (handle, dp)))
	 | `Closed -> handle
       in
       let funlit : Sugartypes.phrasenode = `FunLit (Some [(mt,m_ops)], `Unl, ([[m]], (body, dp))) in
       (*let () = print_endline (Types.string_of_datatype ht) in*)
       (o, funlit, ht)
    | e -> super#phrasenode e
end

let desugar_handlers env = ((new desugar_handlers env) : desugar_handlers :> TransformSugar.transform)
