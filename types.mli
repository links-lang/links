(*pp deriving *)
(** Core types *)

(* field environments *)
type 'a stringmap = 'a Utility.StringMap.t
type 'a field_env = 'a stringmap deriving (Show)

(* type var sets *)
module TypeVarSet : Utility.INTSET

(* points *)
type 'a point = 'a Unionfind.point 

val show_point : 'a Show.show -> 'a Unionfind.point Show.show

type primitive = [ `Bool | `Int | `Char | `Float | `XmlItem | `DB | `String ]
    deriving (Show)

type subkind = [ `Any | `Base ]
    deriving (Show)

type kind = [ `Type | `BaseType | `Row | `BaseRow | `Presence ]
    deriving (Show)

type 't meta_type_var_basis =
    [ `Flexible of (int * subkind)
    | `Rigid of (int * subkind)
    | `Recursive of (int * 't)
    | `Body of 't ]
      deriving (Show)

type 't meta_row_var_basis =
     [ 't meta_type_var_basis | `Closed ]
      deriving (Show)

type 't meta_presence_var_basis = 
    [ `Flexible of int
    | `Rigid of int
    | `Body of 't ]
      deriving (Show)

module Abstype :
sig
  type t deriving (Show, Eq)
  val make  : string -> kind list -> t
  val arity : t -> kind list
  val name  : t -> string
  val compare : t -> t -> int
end

val process  : Abstype.t
val list     : Abstype.t 
val event    : Abstype.t 
val dom_node : Abstype.t

type typ =
    [ `Not_typed
    | `Primitive of primitive
    | `Function of (typ * row * typ)
    | `Record of row
    | `Variant of row
    | `Table of typ * typ * typ
    | `Alias of ((string * type_arg list) * typ)
    | `Application of (Abstype.t * type_arg list)
    | `MetaTypeVar of meta_type_var 
    | `ForAll of (quantifier list ref * typ)]
and presence_flag  = [ `Present | `Absent | `Var of meta_presence_var ]
and field_spec = presence_flag * typ
and field_spec_map = field_spec field_env
and row_var = meta_row_var
and row = field_spec_map * row_var
and meta_type_var = (typ meta_type_var_basis) point
and meta_row_var = (row meta_row_var_basis) point
and meta_presence_var = (presence_flag meta_presence_var_basis) point
and quantifier =
    [ `TypeVar of (int * subkind) * meta_type_var
    | `RowVar of (int * subkind) * meta_row_var
    | `PresenceVar of int * meta_presence_var ]
and type_arg = 
    [ `Type of typ | `Row of row | `Presence of presence_flag ]
      deriving (Show)

type datatype = typ
      deriving (Show)

type type_variable =
    (int * [`Rigid | `Flexible] *
       [`Type of meta_type_var | `Row of meta_row_var | `Presence of meta_presence_var])
      deriving (Show)

(* base kind stuff *)
val is_base_type : datatype -> bool
val is_base_row : row -> bool

val is_baseable_type : datatype -> bool
val is_baseable_row : row -> bool

val basify_type : datatype -> unit
val basify_row : row -> unit

val type_var_number : quantifier -> int

type tycon_spec = [`Alias of quantifier list * datatype | `Abstract of Abstype.t]

type environment        = datatype Env.String.t
 and tycon_environment  = tycon_spec Env.String.t
 and typing_environment = { var_env   : environment ;
                            tycon_env : tycon_environment ;
                            effect_row : row }
    deriving (Show)

val concrete_type : datatype -> datatype

val hoist_quantifiers : datatype -> unit

val is_rigid_quantifier : quantifier -> bool

val box_quantifiers : quantifier list -> quantifier list ref
val unbox_quantifiers : quantifier list ref -> quantifier list

val flexible_of_type : datatype -> datatype option

val normalise_quantifier : quantifier -> quantifier
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

(** get type variables *)
val free_type_vars : datatype -> TypeVarSet.t
val free_row_type_vars : row -> TypeVarSet.t

val var_of_quantifier : quantifier -> int
val type_arg_of_quantifier : quantifier -> type_arg
val freshen_quantifier : quantifier -> quantifier * type_arg
val freshen_quantifier_flexible : quantifier -> quantifier * type_arg

val quantifiers_of_type_args : type_arg list -> quantifier list

(* val free_bound_type_vars : ?include_aliases:bool -> datatype -> TypeVarSet.t *)
(* val free_bound_row_type_vars : ?include_aliases:bool -> row -> TypeVarSet.t *)

val flexible_type_vars : TypeVarSet.t -> datatype -> quantifier Utility.IntMap.t

(** Fresh type variables *)
val type_variable_counter : int ref
val fresh_raw_variable : unit -> int
val bump_variable_counter : int -> unit

(** type variable construction *)
val make_type_variable : int -> subkind -> datatype
val make_rigid_type_variable : int -> subkind -> datatype
val make_row_variable : int -> subkind -> row_var
val make_rigid_row_variable : int -> subkind -> row_var

(** fresh type variable generation *)
val fresh_type_variable : subkind -> datatype
val fresh_rigid_type_variable : subkind -> datatype

val fresh_row_variable : subkind -> row_var
val fresh_rigid_row_variable : subkind -> row_var

val fresh_presence_variable : unit -> presence_flag
val fresh_rigid_presence_variable : unit -> presence_flag

(** fresh quantifiers *)
val fresh_type_quantifier : subkind -> quantifier * datatype
val fresh_flexible_type_quantifier : subkind -> quantifier * datatype

val fresh_row_quantifier : subkind -> quantifier * row
val fresh_flexible_row_quantifier : subkind -> quantifier * row

val fresh_presence_quantifier : unit -> quantifier * presence_flag
val fresh_flexible_presence_quantifier : unit -> quantifier * presence_flag

(** {0 rows} *)
(** empty row constructors *)
val make_empty_closed_row : unit -> row
val make_empty_open_row : subkind -> row

(** singleton row constructors *)
val make_singleton_closed_row : (string * field_spec) -> row
val make_singleton_open_row : (string * field_spec) -> subkind -> row

(** row predicates *)
val is_closed_row : row -> bool
val is_absent_from_row : string -> row -> bool

val is_tuple : ?allow_onetuples:bool -> row -> bool

(** row_var retrieval *)
val get_row_var : row -> int option

(** building rows *)
val make_closed_row : datatype field_env -> row
val row_with : (string * field_spec) -> row -> row
val extend_row : datatype field_env -> row -> row

(** constants *)
val empty_field_env : field_spec_map
val closed_row_var : row_var

val field_env_union : (field_spec_map * field_spec_map) -> field_spec_map

(* val contains_present_fields : field_spec_map -> bool *)

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
val make_tuple_type : datatype list -> datatype
val make_list_type : datatype -> datatype
val make_process_type : row -> datatype
val make_record_type  : datatype field_env -> datatype
val make_variant_type : datatype field_env -> datatype
val make_table_type : datatype * datatype * datatype -> datatype

(** subtyping *)
val is_sub_type : datatype * datatype -> bool
val is_sub_row : row * row -> bool

(** environments *)
type inference_type_map =
    ((datatype Unionfind.point) Utility.IntMap.t ref *
       (row Unionfind.point) Utility.IntMap.t ref)

val extend_typing_environment : typing_environment -> typing_environment -> typing_environment

val make_fresh_envs : datatype -> datatype Utility.IntMap.t * row Utility.IntMap.t * presence_flag Utility.IntMap.t
val make_rigid_envs : datatype -> datatype Utility.IntMap.t * row Utility.IntMap.t * presence_flag Utility.IntMap.t
val make_wobbly_envs : datatype -> datatype Utility.IntMap.t * row Utility.IntMap.t * presence_flag Utility.IntMap.t

(** mailboxes *)
val show_mailbox_annotations : bool Settings.setting

(** pretty printing *)
val string_of_datatype : datatype -> string
(*val string_of_datatype_raw : datatype -> string*)
val string_of_row : row -> string
val string_of_presence : presence_flag -> string
val string_of_type_arg : type_arg -> string
val string_of_row_var : row_var -> string
val string_of_environment : environment -> string
val string_of_typing_environment : typing_environment -> string

val string_of_tycon_spec : tycon_spec -> string
