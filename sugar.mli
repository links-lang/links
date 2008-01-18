(** Converts the tree returned by the parser into our internal
    representation *)

val desugar_expression : Sugartypes.phrase -> Syntax.untyped_expression
val desugar_definitions : Sugartypes.binding list -> Syntax.untyped_definition list
val desugar_program : Sugartypes.program -> Syntax.untyped_program
