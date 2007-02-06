(* Web interface *)

val serve_request : 
  (Result.environment * Inferencetypes.typing_environment) -> 
  string -> 
  unit
