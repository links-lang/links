(* Web interface *)

val serve_request : 
  Syntax.definition list ->
  (Result.environment * Types.typing_environment) -> 
  string -> 
  unit
