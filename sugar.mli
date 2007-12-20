(** Converts the tree returned by the parser into our internal
    representation *)
module LAttrs : sig
  val has_lattrs : Sugartypes.phrasenode -> bool
  val replace_lattrs : Sugartypes.phrase -> Sugartypes.phrase
end

val desugar_expression : (Sugartypes.position -> Syntax.position) -> Sugartypes.phrase -> Syntax.untyped_expression
val desugar_definitions : (Sugartypes.position -> Syntax.position) -> Sugartypes.binding list -> Syntax.untyped_definition list
