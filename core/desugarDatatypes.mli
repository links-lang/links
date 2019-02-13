val read : aliases:Types.tycon_environment -> string -> Types.datatype

val sentence :
  Types.typing_environment ->
  Sugartypes.sentence ->
    Types.typing_environment * Sugartypes.sentence

val program :
  Types.typing_environment ->
  Sugartypes.program ->
    Types.typing_environment * Sugartypes.program

val all_datatypes_desugared : SugarTraversals.predicate
