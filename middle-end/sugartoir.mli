(** Converts the tree returned by the parser into our internal
    representation *)
open Links_core
open Links_frontend
open Links_backend


val show_compiled_ir : bool Settings.setting

type nenv = Var.var Env.String.t
type tenv = Types.datatype Env.Int.t

type env = nenv * tenv * Types.row

module Desugar(LibTy : LibTyping.LIB_TYPING_INFO) :
sig

val desugar_expression : env -> Sugartypes.phrase -> Ir.computation
val desugar_definitions : env -> Sugartypes.binding list ->
  Ir.binding list * nenv
val desugar_program : env -> Sugartypes.program ->
  Ir.binding list * Ir.computation * nenv

end
