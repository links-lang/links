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
module TypeVarSet : Utility.INTSET

(* points *)
type 'a point = 'a Unionfind.point 

module Show_point (A : Show.Show) : Show.Show with type a = A.a Unionfind.point
module Pickle_point (A : Pickle.Pickle) : Pickle.Pickle with type a = A.a Unionfind.point

type primitive = [ `Bool | `Int | `Char | `Float | `XmlItem | `DB
                 | `Abstract | `NativeString]
    deriving (Typeable, Show, Pickle)

type 't meta_type_var_basis =
    [ `Flexible of int
    | `Rigid of int
    | `Recursive of (int * 't)
    | `Body of 't ]
      deriving (Eq, Show, Pickle, Typeable, Shelve)

type 't meta_row_var_basis =
     [ 't meta_type_var_basis | `Closed ]
      deriving (Eq, Show, Pickle, Typeable, Shelve)

type type_variable =
    [`TypeVar of int | `RigidTypeVar of int
    |`RowVar of int | `RigidRowVar of int]
      deriving (Eq, Typeable, Show, Pickle, Shelve)

type quantifier = type_variable
    deriving (Typeable, Show, Pickle)

val type_var_number : type_variable -> int

type datatype =
    [ `Not_typed
    | `Primitive of primitive
    | `Function of (datatype * datatype * datatype)
    | `Record of row
    | `Variant of row
    | `Table of datatype * datatype
    | `Application of (string * (datatype) list)
    | `MetaTypeVar of meta_type_var 
    | `ForAll of (quantifier list * datatype)]
and field_spec = [ `Present of datatype | `Absent ]
and field_spec_map = field_spec field_env
and row_var = meta_row_var
and row = field_spec_map * row_var
and meta_type_var = (datatype meta_type_var_basis) point
and meta_row_var = (row meta_row_var_basis) point
    deriving (Eq, Show, Pickle, Typeable, Shelve)

type environment        = datatype Env.String.t
 and alias_environment  = datatype Env.String.t
 and typing_environment = environment * alias_environment
    deriving (Show)

val extend_env : typing_environment -> typing_environment -> typing_environment

val concrete_type : datatype -> datatype

val for_all : quantifier list * datatype -> datatype

(** useful types *)
val unit_type : datatype
val string_type : datatype
val char_type : datatype
val bool_type : datatype
val int_type : datatype
val float_type : datatype
val database_type : datatype
val xml_type : datatype
val page_type : datatype
val native_string_type : datatype

(** get type variables *)
val free_type_vars : datatype -> TypeVarSet.t
val free_row_type_vars : row -> TypeVarSet.t

val free_bound_type_vars : datatype -> TypeVarSet.t
val free_bound_row_type_vars : row -> TypeVarSet.t

(** used to freshen mailboxes in typename aliases *)
val freshen_mailboxes : datatype -> datatype

(** Fresh type variables *)
val fresh_raw_variable : unit -> int

val bump_variable_counter : int -> unit

(** type variable construction *)
val make_type_variable : int -> datatype
val make_rigid_type_variable : int -> datatype
val make_row_variable : int -> row_var
  
(** fresh type variable generation *)
val fresh_type_variable : unit -> datatype
val fresh_rigid_type_variable : unit -> datatype

val fresh_row_variable : unit -> row_var
val fresh_rigid_row_variable : unit -> row_var

(** {0 rows} *)
(** empty row constructors *)
val make_empty_closed_row : unit -> row
val make_empty_open_row : unit -> row

(** singleton row constructors *)
val make_singleton_closed_row : (string * field_spec) -> row
val make_singleton_open_row : (string * field_spec) -> row

(** row predicates *)
val is_closed_row : row -> bool
val is_absent_from_row : string -> row -> bool

val is_tuple : ?allow_onetuples:bool -> row -> bool

(** row_var retrieval *)
val get_row_var : row -> int option

(** building rows *)
val make_row : datatype field_env -> row
val row_with : (string * field_spec) -> row -> row
val extend_row : datatype field_env -> row -> row

(** constants *)
val empty_field_env : field_spec_map
val closed_row_var : row_var

val field_env_union : (field_spec_map * field_spec_map) -> field_spec_map

val contains_present_fields : field_spec_map -> bool

val is_canonical_row_var : row_var -> bool
val is_rigid_row : row -> bool

val is_rigid_row_with_var : int -> row -> bool

val is_flattened_row : row -> bool
val is_empty_row : row -> bool

(** Convert a row to the form (field_env, row_var)
    where row_var is of the form:
      [ `Closed
      | `Flexible var
      | `Rigid var
      | `Recursive
      ]
*)
val flatten_row : row -> row

(**
 As flatten_row except if the flattened row_var is of the form:

   `Recursive (var, rec_row)

then it is unwrapped. This ensures that all the fields are exposed
in field_env.

Also returns the outermost `Recursive that was unwrapped if it exists,
or None otherwise.
*)
val unwrap_row : row -> (row * row_var option)

val extract_tuple : row -> datatype list

(** type constructors *)
val make_formlet_type : datatype -> datatype
val make_tuple_type : datatype list -> datatype
val make_list_type : datatype -> datatype
val make_mailbox_type : datatype -> datatype
val make_record_type  : datatype field_env -> datatype
val make_variant_type : datatype field_env -> datatype
val make_table_type : datatype * datatype -> datatype

(** subtyping *)
val is_sub_type : datatype * datatype -> bool
val is_sub_row : row * row -> bool

(** type aliases *)
exception UndefinedAlias of string

type type_alias_set = Utility.StringSet.t

val register_alias : string * int list * datatype -> alias_environment -> alias_environment

val free_alias_check : alias_environment -> datatype -> unit
val free_alias_check_row : alias_environment -> row -> unit

val is_mailbox_free : alias_environment -> datatype -> bool
val is_mailbox_free_row : alias_environment -> row -> bool

val type_aliases : datatype -> type_alias_set
val row_type_aliases : row -> type_alias_set

(** alias lookup *)
exception AliasMismatch of string
val lookup_alias : string * datatype list -> alias_environment -> datatype

(** environments *)
type inference_type_map =
    ((datatype Unionfind.point) Utility.IntMap.t ref *
       (row Unionfind.point) Utility.IntMap.t ref)

val concat_typing_environment : typing_environment -> typing_environment -> typing_environment

val make_fresh_envs : datatype -> datatype Utility.IntMap.t * row_var Utility.IntMap.t
val make_rigid_envs : datatype -> datatype Utility.IntMap.t * row_var Utility.IntMap.t
val make_wobbly_envs : datatype -> datatype Utility.IntMap.t * row_var Utility.IntMap.t

(** mailboxes *)
val show_mailbox_annotations : bool Settings.setting

(** pretty printing *)
val string_of_datatype : datatype -> string
val string_of_datatype_raw : datatype -> string
val string_of_row : row -> string
val string_of_row_var : row_var -> string
val string_of_environment : environment -> string
val string_of_typing_environment : typing_environment -> string
