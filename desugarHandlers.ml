open Utility
open Sugartypes

(*
 Desugaring description here
*)


let dp = Sugartypes.dummy_position

class desugar_handlers env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) = function
    | `HandlerLit (Some (m, effects, effects'), spec, hnlit) -> failwith "desugarHandlers: `HandlerLit not yet implemented."
    | e -> super#phrasenode e
end

let desugar_handlers env = ((new desugar_handlers env) : desugar_handlers :> TransformSugar.transform)
