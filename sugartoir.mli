(** Converts the tree returned by the parser into our internal
    representation *)

type nenv = Var.var Env.String.t
type tenv = Types.datatype Env.Int.t

type env = nenv * tenv

val desugar_expression : Sugartypes.phrase -> Ir.computation
val desugar_definitions : Sugartypes.binding list -> Ir.binding list
val desugar_program : env -> Sugartypes.program -> Ir.computation
