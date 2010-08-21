type implementation_type = [ `Atom | `List ]

(** execute the algebra plans for errors and results on the database *)
val execute : Value.database -> implementation_type -> (CompileQuery.tblinfo * Algebra_dag.t option) -> Value.t option

