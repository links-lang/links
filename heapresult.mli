open Utility

type implementation_type = [ `Atom | `List ]

type outcome = Result of Value.t | Error of string

(** execute the algebra plans for errors and results on the database *)
val execute : Value.database -> implementation_type -> (ExpressionToAlgebraDefinitions.tblinfo * Algebra_dag.t list * string IntMap.t) -> outcome

