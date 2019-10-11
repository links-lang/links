val read : aliases:Types.tycon_environment -> string -> Types.datatype

val sentence :
  Types.typing_environment ->
  Sugartypes.sentence ->
    Sugartypes.sentence

val program :
  Types.typing_environment ->
  Sugartypes.program ->
    Sugartypes.program

val all_datatypes_desugared : SugarTraversals.predicate

include Transform.Untyped.S
