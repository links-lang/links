(** Converts the tree returned by the parser into our internal
    representation *)
exception ConcreteSyntaxError of (string * (Lexing.position * Lexing.position))
exception PatternDuplicateNameError of (Syntax.position * string * string)
exception RedundantPatternMatch of Syntax.position

module LAttrs : sig
  val has_lattrs : Sugartypes.phrasenode -> bool
  val replace_lattrs : Sugartypes.phrase -> Sugartypes.phrase
end

val desugar_expression : (Sugartypes.pposition -> Syntax.position) -> Sugartypes.phrase -> Syntax.untyped_expression
val desugar_definitions : (Sugartypes.pposition -> Syntax.position) -> Sugartypes.binding list -> Syntax.untyped_definition list
val desugar_datatype : Sugartypes.datatype -> Types.assumption
val desugar_assumption : Sugartypes.assumption -> Types.assumption
val fresh_type_variable : unit -> Sugartypes.datatype
val make_write_row : Sugartypes.row -> (string * Sugartypes.fieldconstraint list) list -> Sugartypes.row
