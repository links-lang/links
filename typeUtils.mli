(** type destructors *)
exception TypeDestructionError of string

val concrete_type : Types.datatype -> Types.datatype

val project_type : string -> Types.datatype -> Types.datatype
val erase_type   : string -> Types.datatype -> Types.datatype
val inject_type  : string -> Types.datatype -> Types.datatype
val return_type  : Types.datatype -> Types.datatype
val arg_types    : Types.datatype -> Types.datatype list
val element_type : Types.datatype -> Types.datatype
val abs_type     : Types.datatype -> Types.datatype
val app_type     : Types.datatype -> Types.datatype -> Types.datatype

val extract_row : Types.datatype -> Types.row
val split_row : string -> Types.row -> (Types.datatype * Types.row)
val split_variant_type : string -> Types.datatype -> (Types.datatype * Types.datatype)
val variant_at : string -> Types.datatype -> Types.datatype

val quantifiers : Types.datatype -> Types.quantifier list

val record_without : Types.datatype -> Utility.StringSet.t -> Types.datatype

val fresh_type_quantifier : unit -> Types.quantifier * Types.datatype
val fresh_row_quantifier : unit -> Types.quantifier * Types.row_var

