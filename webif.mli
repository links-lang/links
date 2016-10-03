(** Web interface *)

val serve_request : 
  (Value.env * Ir.var Env.String.t * Types.typing_environment) -> 
  (Ir.binding list) ->
  string ->
  unit
