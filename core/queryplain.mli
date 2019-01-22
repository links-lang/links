open Utility;;

val compile : Value.env -> (int * int) option * Ir.computation -> (Value.database * string * Types.datatype) option;;

val compile_update : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option * Ir.computation) -> string;;

val compile_delete : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option) -> string;;
