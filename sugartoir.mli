(** Converts the tree returned by the parser into our internal
    representation *)

val desugar_expression : Sugartypes.phrase -> Ir.computation
val desugar_definitions : Sugartypes.binding list -> Ir.binding list
val desugar_program : Sugartypes.program -> Ir.computation
