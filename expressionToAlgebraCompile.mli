open Utility

val compile : Query2.Annotate.typed_t -> ExpressionToAlgebraDefinitions.tblinfo * Algebra_dag.t list * string IntMap.t
