open Links_core
open Links_backend

val compile : Value.env -> (int * int) option * Ir.computation -> (Value.database * Sql.query * Types.datatype) option
