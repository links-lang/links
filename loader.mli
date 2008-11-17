val load_file : Var.var Env.String.t * Types.typing_environment -> string ->
  (Var.var Env.String.t * Types.typing_environment) * (Ir.binding list * Ir.computation * Types.datatype)
