(* Representations of types *)

open Type_basis

type type_var_set = Type_basis.type_var_set
type primitive = Type_basis.primitive

type datatype = (datatype, row) type_basis
and field_spec = datatype field_spec_basis
and field_spec_map = datatype field_spec_map_basis
and row_var = row row_var_basis
and row = (datatype, row_var) row_basis


type type_variable = Type_basis.type_variable
type quantifier = Type_basis.quantifier

type assumption = datatype assumption_basis
type environment = datatype environment_basis

val (-->) : datatype -> datatype -> datatype

val show_mailbox_annotations : bool Settings.setting
val using_mailbox_typing : unit -> bool
val with_mailbox_typing : bool -> (unit -> 'a) -> 'a

val split_fields : 'typ field_spec_map_basis -> (string * 'typ) list * string list
	
val get_present_fields : 'typ field_spec_map_basis -> (string * 'typ) list
val get_absent_fields : 'typ field_spec_map_basis -> string list

val string_type : datatype
val xml : datatype

(* Type printers *)
val string_of_primitive : primitive -> string

exception Not_tuple

val free_bound_type_vars : datatype -> type_var_set
val free_bound_row_type_vars : row -> type_var_set

val freshen_free_type_vars : (int Utility.IntMap.t) ref -> datatype -> datatype
val type_vars : datatype -> int list

(* string conversions *)
val string_of_datatype : datatype -> string
val string_of_datatype_raw : datatype -> string

val string_of_row : row -> string
val string_of_row_var : row_var -> string

val string_of_quantifier : quantifier -> string
val string_of_assumption : assumption -> string
val string_of_environment : environment -> string

module BasicTypeOps :
  (BASICTYPEOPS with type typ = datatype
		and type row_var' = row_var)

module TypeOps :
  (TYPEOPS with type typ = datatype
	   and type row_var = row_var)

val unit_type : datatype
val tuplify : datatype list -> datatype

(* From library.ml; there's probably another name for these *)
val fresh_type : unit -> type_variable * datatype
val fresh_row : unit -> type_variable * row

val perhaps_process_children : (datatype -> datatype option) ->  datatype -> datatype option


(* Eventually all this stuff should be generated *)
module Show_datatype : Show.Show with type a = datatype
module Show_assumption : Show.Show with type a = assumption
module Show_environment : Show.Show with type a = environment

module Pickle_datatype : Pickle.Pickle with type a = datatype
module Pickle_assumption : Pickle.Pickle with type a = assumption
module Pickle_environment : Pickle.Pickle with type a = environment
