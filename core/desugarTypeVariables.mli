val infer_kinds : bool Settings.setting

(* Act on single type. Used by DesugarDatatypes.read*)
val datatype : Sugartypes.Datatype.with_pos -> Sugartypes.Datatype.with_pos

include Transform.Untyped.S
