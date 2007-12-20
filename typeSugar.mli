module Check :
  sig
    open Sugartypes
    val file :
      Types.typing_environment ->
      (binding list * phrase option) *
      (position -> Syntax.position) -> unit
  end

