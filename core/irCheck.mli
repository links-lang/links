open Ir

module Typecheck : sig
   val program : Types.datatype Env.Int.t -> program -> program
   val bindings : Types.datatype Env.Int.t -> binding list  -> binding list
end