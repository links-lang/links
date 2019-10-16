open Ir

val typecheck : bool Settings.setting

module Typecheck : sig
  (* val program : Types.datatype Env.Int.t -> program -> program *)
  val program : IrTransform.state -> program -> IrTransform.result
   val bindings : Types.datatype Env.Int.t -> binding list  -> binding list
end
