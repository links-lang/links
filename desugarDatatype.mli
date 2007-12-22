val desugar_datatype : Sugartypes.datatype -> Types.datatype
val desugar_datatype' : (Types.meta_type_var Utility.StringMap.t *
                           Types.meta_row_var Utility.StringMap.t) -> Sugartypes.datatype -> Types.datatype
type assumption = Sugartypes.quantifier list * Sugartypes.datatype
val desugar_assumption : assumption -> Types.datatype
val desugar_row : Types.meta_type_var Utility.StringMap.t *
                  Types.row_var Utility.StringMap.t -> Sugartypes.row -> Types.row

type var_env = Types.meta_type_var Utility.StringMap.t *
               Types.meta_row_var Utility.StringMap.t
val generate_var_mapping : Sugartypes.quantifier list -> Types.quantifier list * var_env

val generalize : Sugartypes.datatype -> Sugartypes.assumption
val make_write_row : Sugartypes.row -> (string * Sugartypes.fieldconstraint list) list -> Sugartypes.row
val get_type_vars : Sugartypes.binding -> Sugartypes.quantifier list

val read_datatype : string -> Types.datatype

