(* Representations of types *)

open Type_basis

type type_var_set = Type_basis.type_var_set
type primitive = Type_basis.primitive

type kind = (kind, row) type_basis
and field_spec = kind field_spec_basis
and field_spec_map = kind field_spec_map_basis
and row_var = row row_var_basis
and row = (kind, row_var) row_basis


type type_variable = Type_basis.type_variable
type quantifier = Type_basis.quantifier

type 'typ assumption_basis = ((quantifier list) * 'typ)
type 'typ environment_basis = ((string * 'typ assumption_basis) list)

type assumption = kind assumption_basis
type environment = kind environment_basis

val (-->) : kind -> kind -> kind


val split_fields : 'typ field_spec_map_basis -> (string * 'typ) list * string list
	
val get_present_fields : 'typ field_spec_map_basis -> (string * 'typ) list
val get_absent_fields : 'typ field_spec_map_basis -> string list

val string_type : kind
val xml : kind

(* Type printers *)
val string_of_primitive : primitive -> string

exception Not_tuple

val free_bound_type_vars : kind -> type_var_set
val free_bound_row_type_vars : row -> type_var_set

(* string conversions *)
val string_of_kind : kind -> string
val string_of_kind_raw : kind -> string

val string_of_row : row -> string

val string_of_quantifier : quantifier -> string
val string_of_assumption : assumption -> string
val string_of_environment : environment -> string

(* serialisation *) 
val serialise_primitive : primitive Pickle.serialiser 
val deserialise_primitive : primitive Pickle.deserialiser

val serialise_kind : kind Pickle.serialiser 
val serialise_field_spec : field_spec Pickle.serialiser
val serialise_row_var : row_var Pickle.serialiser
val serialise_row : char -> row Pickle.serialiser
  

val deserialise_kind : kind Pickle.deserialiser
val deserialise_field_spec : field_spec Pickle.deserialiser
val deserialise_row_var : row_var Pickle.deserialiser
val deserialise_row : row Pickle.deserialiser

val serialise_quantifier : quantifier Pickle.serialiser
val deserialise_quantifier : quantifier Pickle.deserialiser

val serialise_assumption : assumption Pickle.serialiser 
val deserialise_assumption : assumption Pickle.deserialiser

val serialise_environment : environment Pickle.serialiser
val deserialise_environment : environment Pickle.deserialiser



module BasicTypeOps :
  (BASICTYPEOPS with type typ = kind
		and type row_var' = row_var)

module TypeOps :
  (TYPEOPS with type typ = kind
	   and type row_var = row_var)

val unit_type : kind

(* From library.ml; there's probably another name for these *)
val fresh_type : unit -> type_variable * kind
val fresh_row : unit -> type_variable * row

val perhaps_process_children : (kind -> kind option) ->  kind -> kind option
