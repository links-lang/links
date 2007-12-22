val desugar_datatype : Sugartypes.datatype -> Types.datatype
val desugar_datatype' : (Types.meta_type_var Utility.StringMap.t *
                           Types.meta_row_var Utility.StringMap.t) -> Sugartypes.datatype -> Types.datatype
val desugar_row : Types.meta_type_var Utility.StringMap.t *
                  Types.row_var Utility.StringMap.t -> Sugartypes.row -> Types.row

type var_env = Types.meta_type_var Utility.StringMap.t *
               Types.meta_row_var Utility.StringMap.t

val make_write_row : Sugartypes.row -> (string * Sugartypes.fieldconstraint list) list -> Sugartypes.row

val var_mapping_from_binding : Sugartypes.binding -> Types.quantifier list * var_env

val read_datatype : string -> Types.datatype
