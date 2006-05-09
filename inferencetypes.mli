(* inference types *)
type type_var_set = Type_basis.type_var_set

type inference_type = [
  | (inference_type, inference_row) Type_basis.type_basis
  | `MetaTypeVar of inference_type Unionfind.point ]
and inference_field_spec = inference_type Type_basis.field_spec_basis
and inference_field_spec_map = inference_field_spec Utility.StringMap.t
and inference_row_var = [
  | inference_row Type_basis.row_var_basis
  | `MetaRowVar of inference_row Unionfind.point ]
and inference_row = (inference_type, inference_row_var) Type_basis.row_basis

type type_variable = Type_basis.type_variable
type quantifier = Type_basis.quantifier

val inference_string_type : inference_type

type inference_expression = (Syntax.position * inference_type * string option (* label *)) Syntax.expression'

(* [TODO]
      change the return type of these functions to be IntSet.t
*)
val free_type_vars : inference_type -> int list
val free_row_type_vars : inference_row -> int list

type inference_assumption = inference_type Type_basis.assumption_basis
type inference_environment = inference_type Type_basis.environment_basis



module BasicInferenceTypeOps :
  (Type_basis.BASICTYPEOPS
   with type typ = inference_type
   and type row_var' = inference_row_var)

val field_env_union : (inference_field_spec_map * inference_field_spec_map) -> inference_field_spec_map

val contains_present_fields : inference_field_spec_map -> bool

val is_flattened_row : inference_row -> bool

(* 
 convert a row to the form (field_env, row_var)
 where row_var is of the form:
    `RowVar None
  | `MetaRowVar (`RowVar (Some var))
  | `MetaRowVar (`RecRowVar (var, rec_row))
 *)
val flatten_row : inference_row -> inference_row


(*
 As flatten_row except if the flattened row_var is of the form:

  `MetaRowVar (`RecRowVar (var, rec_row))

then it is unwrapped. This ensures that all the fields are exposed
in field_env.
 *)
val unwrap_row : inference_row -> inference_row

module InferenceTypeOps :
  (Type_basis.TYPEOPS
   with type typ = inference_type
   and type row_var = inference_row_var)

val type_to_inference_type : Kind.kind -> inference_type
val field_spec_to_inference_field_spec : Kind.field_spec -> inference_field_spec
val row_to_inference_row : Kind.row -> inference_row

val inference_type_to_type : inference_type -> Kind.kind
val inference_field_spec_to_field_spec : inference_field_spec -> Kind.field_spec
val inference_row_to_row : inference_row -> Kind.row

val assumption_to_inference_assumption : Kind.assumption -> inference_assumption
val inference_assumption_to_assumption : inference_assumption -> Kind.assumption


val environment_to_inference_environment : Kind.environment -> inference_environment
val inference_environment_to_environment : inference_environment -> Kind.environment

val string_of_type : inference_type -> string
val string_of_type_raw : inference_type -> string
val string_of_row : inference_row -> string

val string_of_assumption : inference_assumption -> string
val string_of_environment : inference_environment -> string

