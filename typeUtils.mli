val expand_aliases : aenv:Types.alias_environment -> Types.datatype -> Types.datatype

(** type destructors *)
exception TypeDestructionError of string

val project_type : ?aenv:Types.alias_environment -> string -> Types.datatype -> Types.datatype
val erase_type : ?aenv:Types.alias_environment -> string -> Types.datatype -> Types.datatype
val inject_type : ?aenv:Types.alias_environment -> string -> Types.datatype -> Types.datatype
val return_type : ?aenv:Types.alias_environment -> Types.datatype -> Types.datatype
val arg_types : ?aenv:Types.alias_environment -> Types.datatype -> Types.datatype list
val element_type : ?aenv:Types.alias_environment -> Types.datatype -> Types.datatype
val abs_type : ?aenv:Types.alias_environment -> Types.datatype -> Types.datatype
val app_type : ?aenv:Types.alias_environment -> Types.datatype -> Types.datatype -> Types.datatype

val split_row : string -> Types.row -> (Types.datatype * Types.row)
val split_variant_type : ?aenv:Types.alias_environment -> string -> Types.datatype -> (Types.datatype * Types.datatype)
