(* Web interface *)

val serve_request : Syntax.expression list -> (Result.environment * Types.typing_environment) -> string -> unit
