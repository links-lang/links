val insert_toplevel_handlers : Types.typing_environment -> TransformSugar.transform
val desugar_session_exceptions : Types.typing_environment -> TransformSugar.transform
val wrap_linear_handlers : SugarTraversals.map
val desugar_program : TransformSugar.program_transformer

include Transform.Typeable.S
include Transform.Untyped.S
