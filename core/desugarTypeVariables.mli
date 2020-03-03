val infer_kinds : bool Settings.setting


val concrete_subkind : CommonTypes.Subkind.t option -> CommonTypes.Subkind.t

val free_type_variable : ?var:string -> SourceCode.Position.t -> exn

val sig_allows_implicitly_bound_vars : Sugartypes.datatype' option -> bool

(* Act on single type. Used by DesugarDatatypes.read *)
val datatype : Sugartypes.Datatype.with_pos -> Sugartypes.Datatype.with_pos

include Transform.Untyped.S
