(* Web interface *)

val serve_request : Syntax.expression list -> (Result.environment * Inferencetypes.typing_environment) -> string -> unit
