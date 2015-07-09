(** type destructors *)
exception TypeDestructionError of string

val concrete_type : Types.datatype -> Types.datatype

val project_type : string -> Types.datatype -> Types.datatype
val erase_type   : Utility.stringset -> Types.datatype -> Types.datatype
val inject_type  : string -> Types.datatype -> Types.datatype
val return_type  : Types.datatype -> Types.datatype
val arg_types    : Types.datatype -> Types.datatype list
val effect_row   : Types.datatype -> Types.row

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

(* Handler stuff *)
type operation_signature = Single of Types.datatype  (* Takes a single parameter.  *)
			 | Binary of Types.datatype * Types.datatype (* Takes two parameters: one regular parameter and the continuation. *)
			 | Invalid (* Cannot be an operation, i.e. the entity is not well-formed. *)

type operation           = string * operation_signature
					   
val handles_operation                   : Types.row -> string -> bool
val return_case                         : string
val extract_operations                  : Types.row -> operation list
val simplify_operation_signatures       : operation list -> operation list						
val effectrow_of_oplist                 : operation list -> Types.row
