(** type destructors *)
exception TypeDestructionError of string

val concrete_type : Types.datatype -> Types.datatype

val project_type : string -> Types.datatype -> Types.datatype
val erase_type   : Utility.stringset -> Types.datatype -> Types.datatype
val inject_type  : string -> Types.datatype -> Types.datatype
val return_type  : Types.datatype -> Types.datatype
val arg_types    : Types.datatype -> Types.datatype list
val effect_row   : Types.datatype -> Types.row
val is_function_type : Types.datatype -> bool
val is_thunk_type : Types.datatype -> bool
val is_builtin_effect : string -> bool
val element_type : Types.datatype -> Types.datatype

val table_read_type : Types.datatype -> Types.datatype
val table_write_type : Types.datatype -> Types.datatype
val table_needed_type : Types.datatype -> Types.datatype

val abs_type     : Types.datatype -> Types.datatype
val app_type     : Types.datatype -> Types.datatype -> Types.datatype

val extract_row : Types.datatype -> Types.row
val split_row : string -> Types.row -> (Types.datatype * Types.row)
val split_variant_type : string -> Types.datatype -> (Types.datatype * Types.datatype)
val variant_at : string -> Types.datatype -> Types.datatype

val quantifiers : Types.datatype -> Types.quantifier list

val record_without : Types.datatype -> Utility.StringSet.t -> Types.datatype

(* Session stuff *)
(* val session_of_type : Types.datatype -> Types.session_type *)
val select_type : string -> Types.datatype -> Types.datatype
val split_choice_type : string -> Types.datatype -> (Types.datatype * Types.datatype)
val choice_at : string -> Types.datatype -> Types.datatype
