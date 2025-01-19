val infer_kinds : bool Settings.setting

val concrete_subkind : ?is_effect:bool -> CommonTypes.Subkind.t option -> CommonTypes.Subkind.t

val free_type_variable : ?var:string -> SourceCode.Position.t -> exn

val sig_allows_implicitly_bound_vars : Sugartypes.datatype' option -> bool

val is_anonymous : Sugartypes.SugarTypeVar.t -> bool

(* Act on a type that's a lib.ml signature. Used by DesugarDatatypes.read *)
val standalone_signature : Sugartypes.Datatype.with_pos -> Sugartypes.Datatype.with_pos

include Transform.Untyped.S
