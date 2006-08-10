(** Definitions of types used during inference--not for public consumption. *)
type type_var_set = Type_basis.type_var_set

type datatype = [
  | (datatype, row) Type_basis.type_basis
  | `MetaTypeVar of datatype Unionfind.point 
  | `RigidTypeVar of int
]
and field_spec = datatype Type_basis.field_spec_basis
and field_spec_map = field_spec Utility.StringMap.t
and row_var = [
  | row Type_basis.row_var_basis
  | `MetaRowVar of row Unionfind.point 
  | `RigidRowVar of int
]
and row = (datatype, row_var) Type_basis.row_basis

type type_variable = Type_basis.type_variable
type quantifier = Type_basis.quantifier

val string_type : datatype

type inference_expression = (Syntax.position * datatype * string option (* label *)) Syntax.expression'

(* [TODO]
      change the return type of these functions to be IntSet.t
*)
val free_type_vars : datatype -> int list
val free_row_type_vars : row -> int list

type assumption = datatype Type_basis.assumption_basis
type environment = datatype Type_basis.environment_basis

module BasicInferenceTypeOps :
  (Type_basis.BASICTYPEOPS
   with type typ = datatype
   and type row_var' = row_var)

val field_env_union : (field_spec_map * field_spec_map) -> field_spec_map

val contains_present_fields : field_spec_map -> bool

val is_canonical_row_var : row_var -> bool
val is_rigid_row : row -> bool

val is_flattened_row : row -> bool
val is_empty_row : row -> bool

val flatten_row : row -> row

(** 
 Convert a row to the form (field_env, row_var)
 where row_var is of the form: {[
    `RowVar None
  | `MetaRowVar (`RowVar (Some var))
  | `MetaRowVar (`RecRowVar (var, rec_row))
]}
 *)


(*
 As flatten_row except if the flattened row_var is of the form:

  `MetaRowVar (`RecRowVar (var, rec_row))

then it is unwrapped. This ensures that all the fields are exposed
in field_env.

Also returns the outermost RecRowVar that was unwrapped if it exists,
or None otherwise.
 *)
val unwrap_row : row -> (row * (int * row) option)

module InferenceTypeOps :
  (Type_basis.TYPEOPS
   with type typ = datatype
   and type row_var = row_var)

type inference_type_map =
    ((datatype Unionfind.point) Utility.IntMap.t ref *
       (row Unionfind.point) Utility.IntMap.t ref)

val empty_var_maps : unit -> inference_type_map

val inference_type_of_type : inference_type_map -> Types.datatype -> datatype
val inference_field_spec_of_field_spec : inference_type_map -> Types.field_spec -> field_spec
val inference_row_of_row : inference_type_map -> Types.row -> row

val type_of_inference_type : datatype -> Types.datatype
val field_spec_of_inference_field_spec : field_spec -> Types.field_spec
val row_of_inference_row : row -> Types.row

val inference_assumption_of_assumption : inference_type_map -> Types.assumption -> assumption
val assumption_of_inference_assumption : assumption -> Types.assumption


val inference_environment_of_environment : inference_type_map -> Types.environment -> environment
val environment_of_inference_environment : environment -> Types.environment

val inference_expression_of_expression : inference_type_map -> Syntax.expression -> inference_expression
val expression_of_inference_expression : inference_expression -> Syntax.expression

val string_of_datatype : datatype -> string
val string_of_datatype_raw : datatype -> string
val string_of_row : row -> string
val string_of_row_var : row_var -> string

(* val string_of_quantifier : quantifier -> string *)
val string_of_assumption : assumption -> string
val string_of_environment : environment -> string

val is_negative : int -> datatype -> bool
val is_negative_row : int -> row -> bool
val is_negative_field_env : int -> field_spec_map -> bool
val is_negative_row_var : int -> row_var -> bool

val is_positive : int -> datatype -> bool
val is_positive_row : int -> row -> bool
val is_positive_field_env : int -> field_spec_map -> bool
val is_positive_row_var : int -> row_var -> bool
