(*pp deriving *)
(** Core types *)

(* field environments *)
(*module FieldEnv : Map.S with type key = string*)
(*type 'a stringmap = 'a Utility.StringMap.t*)

(* module Typeable_stringmap (A : Typeable.Typeable) : Typeable.Typeable with type a = A.a stringmap *)
(* module Show_stringmap (A : Show.Show) : Show.Show with type a = A.a stringmap *)
(* module Pickle_stringmap (A : Pickle.Pickle) : Pickle.Pickle with type a = A.a stringmap *)

type 'a stringmap = 'a Utility.StringMap.t
type 'a field_env = 'a stringmap deriving (Eq, Pickle, Typeable, Show, Shelve)

(* type var sets *)
module TypeVarSet : Set.S with type elt = int
type type_var_set = Utility.IntSet.t

(* points *)
type 'a point = 'a Unionfind.point 

module Show_point (A : Show.Show) : Show.Show with type a = A.a Unionfind.point
module Pickle_point (A : Pickle.Pickle) : Pickle.Pickle with type a = A.a Unionfind.point


type primitive = [ `Bool | `Int | `Char | `Float | `XmlItem | `DB
                 | `Abstract ]
    deriving (Typeable, Show, Pickle)

type 't meta_type_var_basis =
    [ `Flexible of int
    | `Rigid of int
    | `Recursive of (int * 't)
    | `Body of 't ]
      deriving (Eq, Show, Pickle, Typeable, Shelve)

(* this is what we should replace meta row vars with *)
type 't meta_row_var_basis =
     [ 't meta_type_var_basis | `Closed ]
      deriving (Eq, Show, Pickle, Typeable, Shelve)

type datatype =
    [ `Not_typed
    | `Primitive of primitive
    | `Function of (datatype * datatype * datatype)
    | `Record of row
    | `Variant of row
    | `Table of datatype * datatype
    | `Application of (string * (datatype) list)
    | `MetaTypeVar of meta_type_var ]
and field_spec = [ `Present of datatype | `Absent ]
and field_spec_map = field_spec field_env
and row_var = meta_row_var
and row = field_spec_map * row_var
and meta_type_var = (datatype meta_type_var_basis) point
and meta_row_var = (row meta_row_var_basis) point
    deriving (Eq, Show, Pickle, Typeable, Shelve)

val concrete_type : datatype -> datatype

type type_variable = [`TypeVar of int | `RigidTypeVar of int | `RowVar of int]
    deriving (Typeable, Show, Pickle)
type quantifier = type_variable
    deriving (Typeable, Show, Pickle)

type assumption = ((quantifier list) * datatype)
    deriving (Eq, Show, Pickle, Typeable, Shelve)
type environment = ((string * assumption) list)
    deriving (Show, Pickle)
type alias_environment = assumption stringmap
    deriving (Show, Pickle)
type typing_environment = environment * alias_environment
    deriving (Show, Pickle)

(* useful types *)
val unit_type : datatype
val string_type : datatype
val xml_type : datatype

(* [TODO]
      change the return type of these functions to be IntSet.t
*)
val free_type_vars : datatype -> int list
val free_row_type_vars : row -> int list

(* used to freshen mailboxes in typename aliases *)
val freshen_mailboxes : datatype -> datatype

(* type operations *)
module type TYPEOPS =
sig
  (* type variable construction *)
  val make_type_variable : int -> datatype
  val make_rigid_type_variable : int -> datatype
  val make_row_variable : int -> row_var

  (* fresh type variable generation *)
  val fresh_type_variable : unit -> datatype
  val fresh_rigid_type_variable : unit -> datatype
  val fresh_row_variable : unit -> row_var
  val fresh_rigid_row_variable : unit -> row_var

  (* empty row constructors *)
  val make_empty_closed_row : unit -> row
  val make_empty_open_row : unit -> row
  val make_empty_open_row_with_var : int -> row

  (* singleton row constructors *)
  val make_singleton_closed_row : (string * field_spec) -> row
  val make_singleton_open_row : (string * field_spec) -> row
  val make_singleton_open_row_with_var : (string * field_spec) -> int -> row

  (* row predicates *)
  val is_closed_row : row -> bool
  val is_absent_from_row : string -> row -> bool

  (* row_var retrieval *)
  val get_row_var : row -> int option

  (* row update *)
  val set_field : (string * field_spec) -> row -> row

  (* constants *)
  val empty_field_env : field_spec_map
  val closed_row_var : row_var
end

module TypeOps : TYPEOPS

val field_env_union : (field_spec_map * field_spec_map) -> field_spec_map

val contains_present_fields : field_spec_map -> bool

val is_canonical_row_var : row_var -> bool
val is_rigid_row : row -> bool

val is_rigid_row_with_var : int -> row -> bool

val is_flattened_row : row -> bool
val is_empty_row : row -> bool

(** Convert a row to the form (field_env, row_var)
    where row_var is of the form:
{[
    `RowVar None
  | `MetaRowVar (`RowVar (Some var))
  | `MetaRowVar (`RecRowVar (var, rec_row))
]}
*)
val flatten_row : row -> row

(**
 As flatten_row except if the flattened row_var is of the form:

   `MetaRowVar (`RecRowVar (var, rec_row))

then it is unwrapped. This ensures that all the fields are exposed
in field_env.

Also returns the outermost RecRowVar that was unwrapped if it exists,
or None otherwise.
*)
val unwrap_row : row -> (row * row_var option)

(* check for free aliases *)
exception UndefinedAlias of string

type type_alias_set = Utility.StringSet.t

val free_alias_check : alias_environment -> datatype -> unit
val free_alias_check_row : alias_environment -> row -> unit

val type_aliases : datatype -> type_alias_set
val row_type_aliases : row -> type_alias_set

type inference_type_map =
    ((datatype Unionfind.point) Utility.IntMap.t ref *
       (row Unionfind.point) Utility.IntMap.t ref)

(*type context = environment * inference_type_map*)

(** Generate a fresh type variable *)
val fresh_raw_variable : unit -> int

(* environments *)
val concat_environment : typing_environment -> typing_environment -> typing_environment
val environment_values : environment -> assumption list
val lookup : string -> environment -> assumption

(* mailboxes *)
val show_mailbox_annotations : bool Settings.setting

(* pretty printing *)
val string_of_datatype : datatype -> string
val string_of_datatype_raw : datatype -> string
val string_of_row : row -> string
val string_of_row_var : row_var -> string

(* val string_of_quantifier : quantifier -> string *)
val string_of_assumption : assumption -> string
val string_of_environment : environment -> string

val make_fresh_envs : datatype -> datatype Utility.IntMap.t * row_var Utility.IntMap.t
val make_rigid_envs : datatype -> datatype Utility.IntMap.t * row_var Utility.IntMap.t
val make_wobbly_envs : datatype -> datatype Utility.IntMap.t * row_var Utility.IntMap.t

(* alias lookup *)
exception AliasMismatch of string
val lookup_alias : string * datatype list -> alias_environment -> assumption
