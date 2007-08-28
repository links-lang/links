module Check :
  sig
    open Sugartypes
    val file :
      Types.typing_environment ->
      (binding list * phrase option) *
      (pposition -> Syntax.position) -> unit
  end

