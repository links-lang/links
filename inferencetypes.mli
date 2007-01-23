(*pp deriving *)
(** Definitions of types used during inference--not for public consumption. *)
type type_var_set = Type_basis.type_var_set

(* type datatype = (datatype, row) type_basis *)
(* and field_spec = datatype field_spec_basis *)
(* and field_spec_map = datatype field_spec_map_basis *)
(* and row_var = row row_var_basis *)
(* and row = (datatype, row_var) row_basis deriving (Show, Pickle) *)


type datatype = [
  | (datatype, row) Type_basis.type_basis
  | `MetaTypeVar of datatype Unionfind.point
]
and field_spec = datatype Type_basis.field_spec_basis
and field_spec_map = datatype Type_basis.field_spec_map_basis
and row_var = [
  | row Type_basis.row_var_basis
  | `MetaRowVar of row Unionfind.point 
  | `RigidRowVar of int
]
and row = (datatype, row_var) Type_basis.row_basis deriving (Show, Pickle)

type type_variable = Type_basis.type_variable
type quantifier = Type_basis.quantifier

val string_type : datatype
val xml_type : datatype

(* [TODO]
      change the return type of these functions to be IntSet.t
*)
val free_type_vars : datatype -> int list
val free_row_type_vars : row -> int list

type assumption = datatype Type_basis.assumption_basis deriving (Show, Pickle)
type environment = datatype Type_basis.environment_basis

type alias_environment = datatype Type_basis.alias_environment_basis
type typing_environment = environment * alias_environment

val concat_environment : typing_environment -> typing_environment -> typing_environment

module BasicInferenceTypeOps :
  (Type_basis.BASICTYPEOPS
   with type typ = datatype
   and type row_var' = row_var)

val field_env_union : (field_spec_map * field_spec_map) -> field_spec_map

val contains_present_fields : field_spec_map -> bool

val is_canonical_row_var : row_var -> bool
val is_rigid_row : row -> bool

val is_rigid_row_with_var : int -> row -> bool

val is_flattened_row : row -> bool
val is_empty_row : row -> bool

val flatten_row : row -> row
(** Convert a row to the form (field_env, row_var)
    where row_var is of the form:
{[
    `RowVar None
  | `MetaRowVar (`RowVar (Some var))
  | `MetaRowVar (`RecRowVar (var, rec_row))
]}
*)

val unwrap_row : row -> (row * (int * row) option)
(**
 As flatten_row except if the flattened row_var is of the form:

   `MetaRowVar (`RecRowVar (var, rec_row))

then it is unwrapped. This ensures that all the fields are exposed
in field_env.

Also returns the outermost RecRowVar that was unwrapped if it exists,
or None otherwise.
*)

(* check for free aliases *)
exception UndefinedAlias of string

type type_alias_set = Utility.StringSet.t

val free_alias_check : alias_environment -> datatype -> unit
val free_alias_check_row : alias_environment -> row -> unit

val type_aliases : datatype -> type_alias_set
val row_type_aliases : row -> type_alias_set

module InferenceTypeOps :
  (Type_basis.TYPEOPS
   with type typ = datatype
   and type row_var = row_var)

type inference_type_map =
    ((datatype Unionfind.point) Utility.IntMap.t ref *
       (row Unionfind.point) Utility.IntMap.t ref)

(*type context = environment * inference_type_map*)

val make_type_variable : int -> datatype

val show_mailbox_annotations : bool Settings.setting
val using_mailbox_typing : unit -> bool
val with_mailbox_typing : bool -> (unit -> 'a) -> 'a

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

val unit_type : datatype
