open Links_core

open Ir

val typecheck : bool Settings.setting

module Typecheck(LibTy : LibTyping.LIB_TYPING_INFO) :
sig
   val program : Types.datatype Env.Int.t -> program -> program
   val bindings : Types.datatype Env.Int.t -> binding list  -> binding list
end
