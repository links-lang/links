(* Covnerting the tree returned by the parser into our internal
representation *)
exception ConcreteSyntaxError of (string * (Lexing.position * Lexing.position))

val desugar : (Sugartypes.pposition -> Syntax.position) -> Sugartypes.phrase -> Syntax.untyped_expression
val desugar_datatype : Sugartypes.datatype -> Types.assumption

val col_unique_name : unit -> string (* One of these things is not like the others,
                                        One of these things just doesn't belong,
                                        Can you tell which thing is not like the others
                                        By the time I finish my song? *)
