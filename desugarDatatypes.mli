val read : ?aliases:Types.alias_environment -> string -> Types.datatype

val sentence : Types.typing_environment -> Sugartypes.sentence -> Types.typing_environment * Sugartypes.sentence
val program  : Types.alias_environment -> Sugartypes.program -> Sugartypes.program

val all_datatypes_desugared : SugarTraversals.predicate
