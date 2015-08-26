open Utility
open Sugartypes

(*
  Ensures that Op() becomes Op(p) where p is some dummy parameter.
*)


let dp = Sugartypes.dummy_position

class desugar_operations env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) =
    function
    | `DoOperation (name, Some args, Some t) ->       
       let args =
	 match t with
	   `Function (t,_,_) when t = Types.unit_type -> [`Constant (`String "Hack: Placeholder for unit"), dp]
	 | _ -> args		  
       in
       (o, `DoOperation (name, Some args, Some t), t)
    | e -> super#phrasenode e
end

let desugar_operations env = ((new desugar_operations env) : desugar_operations :> TransformSugar.transform)
