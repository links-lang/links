val read : aliases:FrontendTypeEnv.qual_tycon_environment -> string -> Types.datatype

val sentence :
  FrontendTypeEnv.t ->
  Sugartypes.sentence ->
    FrontendTypeEnv.t * Sugartypes.sentence

val program :
  FrontendTypeEnv.t ->
  Sugartypes.program ->
    FrontendTypeEnv.t * Sugartypes.program

val all_datatypes_desugared : SugarTraversals.predicate
