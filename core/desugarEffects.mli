(* Act on a type that's a lib.ml signature. Used by DesugarDatatypes.read *)
val standalone_signature : Types.tycon_environment -> Sugartypes.Datatype.with_pos -> Sugartypes.Datatype.with_pos

include Transform.Untyped.S
