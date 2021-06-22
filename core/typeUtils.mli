open CommonTypes

(** type destructors *)
exception TypeDestructionError of string

val concrete_type : Types.datatype -> Types.datatype

val project_type : ?overstep_quantifiers:bool -> string -> Types.datatype -> Types.datatype
val erase_type   : ?overstep_quantifiers:bool -> Utility.stringset -> Types.datatype -> Types.datatype
val inject_type  : string -> Types.datatype -> Types.datatype
val return_type  : ?overstep_quantifiers:bool -> Types.datatype -> Types.datatype
val arg_types    : ?overstep_quantifiers:bool -> Types.datatype -> Types.datatype list
val effect_row   : ?overstep_quantifiers:bool -> Types.datatype -> Types.row
val is_function_type : Types.datatype -> bool
val is_thunk_type : Types.datatype -> bool
val is_builtin_effect : string -> bool
val element_type : ?overstep_quantifiers:bool -> Types.datatype -> Types.datatype

val table_read_type : Types.datatype -> Types.datatype
val table_write_type : Types.datatype -> Types.datatype
val table_needed_type : Types.datatype -> Types.datatype

val abs_type     : Types.datatype -> Types.datatype
val app_type     : Types.datatype -> Types.datatype -> Types.datatype

val extract_row : Types.datatype -> Types.row
val extract_row_parts : Types.datatype -> Types.row'
val iter_row : (string -> Types.field_spec -> unit) -> Types.row -> unit
val split_row : string -> Types.row -> (Types.datatype * Types.row)
val split_variant_type : string -> Types.datatype -> (Types.datatype * Types.datatype)
val variant_at : ?overstep_quantifiers:bool -> string -> Types.datatype -> Types.datatype

val quantifiers : Types.datatype -> Quantifier.t list
val split_quantified_type : Types.datatype -> (Quantifier.t list * Types.datatype)

val record_without : Types.datatype -> Utility.StringSet.t -> Types.datatype

(* Session stuff *)
(* val session_of_type : Types.datatype -> Types.session_type *)
val select_type : string -> Types.datatype -> Types.datatype
val split_choice_type : string -> Types.datatype -> (Types.datatype * Types.datatype)
val choice_at : string -> Types.datatype -> Types.datatype

val primary_kind_of_type : Types.datatype -> PrimaryKind.t
val check_type_wellformedness : PrimaryKind.t option -> Types.datatype -> unit

val row_present_types : Types.datatype -> Types.datatype Utility.StringMap.t

val pack_types : Types.datatype list -> Types.datatype

val from_present : Types.field_spec -> Types.datatype
