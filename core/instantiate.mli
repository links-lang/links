open Utility

type instantiation_maps = (Types.datatype IntMap.t * Types.row IntMap.t * Types.field_spec IntMap.t)

exception ArityMismatch of (int * int) (* Expected, provided *)

val show_recursion : bool Settings.setting

val var : Types.environment -> string -> (Types.type_arg list * Types.datatype)
val rigid : Types.environment -> string -> (Types.type_arg list * Types.datatype)
val typ : Types.datatype -> (Types.type_arg list * Types.datatype)
val typ_rigid : Types.datatype -> (Types.type_arg list * Types.datatype)
val datatype : instantiation_maps -> Types.datatype -> Types.datatype
val row : instantiation_maps -> Types.row -> Types.row
val presence : instantiation_maps -> Types.field_spec -> Types.field_spec
val alias : string -> Types.type_arg list -> Types.tycon_environment -> Types.datatype
val recursive_application : string -> Types.quantifier list -> Types.type_arg list -> Types.datatype -> Types.datatype

(* Given a quantified type and a list of type arguments, create the corresponding instantiation maps *)
val instantiation_maps_of_type_arguments : bool -> Types.datatype -> Types.type_arg list -> (Types.datatype * instantiation_maps)

val apply_type : Types.datatype -> Types.type_arg list -> Types.datatype
val freshen_quantifiers : Types.datatype -> Types.datatype
val replace_quantifiers : Types.datatype -> Types.quantifier list -> Types.datatype
