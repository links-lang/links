
(* Serialise the continuation and environment, and adjust the form accordingly *)
val xml_transform : Result.environment -> (string -> Result.result) -> (Syntax.expression -> Result.continuation -> Result.result) -> Syntax.expression -> Syntax.expression
