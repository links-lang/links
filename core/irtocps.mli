(* JavaScript code generation *)
open Utility

(** IR variable environment *)
module VEnv = Env.Int

(** Type of environments mapping IR variables to object variables *)
type venv = (string * Types.typ) VEnv.t

(** Intermediate language *)
module Code: sig
  module MetaContinuation: sig
    type nonrec t = (Ir.value -> Ir.tail_computation)
    val identity : t
  end
end

module type CPS_Compiler_sig = sig
  val generate_program : venv -> Ir.computation -> Types.typ -> venv * Ir.computation
end

module Compiler : CPS_Compiler_sig

val name: string
val program: IrTransform.state -> Ir.program -> IrTransform.result

module IrPrint: sig
  val name: string
  val program: IrTransform.state -> Ir.program -> IrTransform.result
end