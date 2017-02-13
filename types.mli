(*pp deriving *)
(** Core types *)

(* field environments *)
type 'a stringmap = 'a Utility.StringMap.t
type 'a field_env = 'a stringmap deriving (Show)

(* type var sets *)
module TypeVarSet : Utility.INTSET

(* points *)
type 'a point = 'a Unionfind.point
  deriving (Show)

type primitive = [ `Bool | `Int | `Char | `Float | `XmlItem | `DB | `String ]
    deriving (Show)

type restriction = [ `Any | `Base | `Session ]
    deriving (Eq, Show)
type linearity   = [ `Any | `Unl ]
    deriving (Eq, Show)

type subkind = linearity * restriction
    deriving (Eq, Show)

type freedom = [`Rigid | `Flexible]
    deriving (Eq, Show)

type primary_kind = [ `Type | `Row | `Presence ]
    deriving (Eq, Show)

type kind = primary_kind * subkind
    deriving (Eq, Show)

type 't meta_type_var_non_rec_basis =
    [ `Var of (int * subkind * freedom)
    | `Body of 't ]
      deriving (Show)

type 't meta_type_var_basis =
    [ 't meta_type_var_non_rec_basis
    | `Recursive of (int * 't) ]
      deriving (Show)

type 't meta_row_var_basis =
     [ 't meta_type_var_basis | `Closed ]
      deriving (Show)

type 't meta_presence_var_basis = 't meta_type_var_non_rec_basis
      deriving (Show)

module Abstype :
sig
  type t deriving (Show, Eq)
  val make  : string -> kind list -> t
  val arity : t -> kind list
  val name  : t -> string
  val compare : t -> t -> int
end

module Vars : sig
  type flavour = [`Rigid | `Flexible | `Recursive]
  type kind    = primary_kind
  type scope   = [`Free | `Bound]
  type vars_list = (int * (flavour * kind * scope)) list
end

module Print : sig
  type policy = {quantifiers:bool; flavours:bool; hide_fresh:bool; kinds:string}

  val default_policy : unit -> policy
end


val process        : Abstype.t
val list           : Abstype.t
val event          : Abstype.t
val dom_node       : Abstype.t
val access_point   : Abstype.t
val socket         : Abstype.t

type ('t, 'r) session_type_basis =
    [ `Input of 't * 't
    | `Output of 't * 't
    | `Select of 'r
    | `Choice of 'r
    | `Dual of 't
    | `End ]
      deriving (Show)

type typ =
    [ `Not_typed
    | `Primitive of primitive
    | `Function of (typ * row * typ)
    | `Lolli of (typ * row * typ)
    | `Record of row
    | `Variant of row
    | `Table of typ * typ * typ
    | `Alias of ((string * type_arg list) * typ)
    | `Application of (Abstype.t * type_arg list)
    | `MetaTypeVar of meta_type_var
    | `ForAll of (quantifier list ref * typ)
    | (typ, row) session_type_basis ]
and field_spec = [ `Present of typ | `Absent | `Var of meta_presence_var ]
and field_spec_map = field_spec field_env
and row_var = meta_row_var
and row = field_spec_map * row_var * bool
and meta_type_var = (typ meta_type_var_basis) point
and meta_row_var = (row meta_row_var_basis) point
and meta_presence_var = (field_spec meta_presence_var_basis) point
and meta_var = [ `Type of meta_type_var | `Row of meta_row_var | `Presence of meta_presence_var ]
and quantifier = int * subkind * meta_var
and type_arg =
    [ `Type of typ | `Row of row | `Presence of field_spec ]
      deriving (Show)

type session_type = (typ, row) session_type_basis
  deriving (Show)

type datatype = typ
      deriving (Show)

(* base kind stuff *)
val is_base_type : datatype -> bool
val is_base_row : row -> bool

val is_baseable_type : datatype -> bool
val is_baseable_row : row -> bool

val basify_type : datatype -> unit
val basify_row : row -> unit

(* unl stuff *)
val is_unl_type : datatype -> bool
val is_unl_row : row -> bool

val type_can_be_unl : datatype -> bool
val row_can_be_unl : row -> bool
(* val session_can_be_unl : datatype -> bool *)

val make_type_unl : datatype -> unit
val make_row_unl : row -> unit
(* val make_session_unl : datatype -> unit *)

(* session kind stuff *)
val is_session_type : datatype -> bool
val is_session_row : row -> bool

val is_sessionable_type : datatype -> bool
val is_sessionable_row : row -> bool

val sessionify_type : datatype -> unit
val sessionify_row : row -> unit

(* val dual_session : datatype -> datatype *)
val dual_row : row -> row
val dual_type : datatype -> datatype

val type_var_number : quantifier -> int

type tycon_spec = [`Alias of quantifier list * datatype | `Abstract of Abstype.t]

type environment        = datatype Env.String.t
 and tycon_environment  = tycon_spec Env.String.t
 and typing_environment = { var_env   : environment ;
                            tycon_env : tycon_environment ;
                            effect_row : row }
    deriving (Show)

val empty_typing_environment : typing_environment

val concrete_type : datatype -> datatype
val concrete_field_spec : field_spec -> field_spec

val normalise_datatype : datatype -> datatype
val normalise_row : row -> row
val normalise_typing_environment : typing_environment -> typing_environment

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
val keys_type : datatype
val char_type : datatype
val bool_type : datatype
val int_type : datatype
val float_type : datatype
val database_type : datatype
val xml_type : datatype

(** get type variables *)
val free_type_vars : datatype -> TypeVarSet.t
val free_row_type_vars : row -> TypeVarSet.t
val free_bound_type_vars     : ?include_aliases:bool -> typ -> Vars.vars_list
val free_bound_row_type_vars : ?include_aliases:bool -> row -> Vars.vars_list

val var_of_quantifier : quantifier -> int
val primary_kind_of_quantifier : quantifier -> primary_kind
val kind_of_quantifier : quantifier -> kind
val type_arg_of_quantifier : quantifier -> type_arg
val freshen_quantifier : quantifier -> quantifier * type_arg
val freshen_quantifier_flexible : quantifier -> quantifier * type_arg

val primary_kind_of_type_arg : type_arg -> primary_kind

val quantifiers_of_type_args : type_arg list -> quantifier list

val flexible_type_vars : TypeVarSet.t -> datatype -> quantifier Utility.IntMap.t

(** Fresh type variables *)
val type_variable_counter : int ref
val fresh_raw_variable : unit -> int

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

val fresh_session_variable : linearity -> datatype

val fresh_presence_variable : subkind -> field_spec
val fresh_rigid_presence_variable : subkind -> field_spec

(** fresh quantifiers *)
val fresh_type_quantifier : subkind -> quantifier * datatype
val fresh_flexible_type_quantifier : subkind -> quantifier * datatype

val fresh_row_quantifier : subkind -> quantifier * row
val fresh_flexible_row_quantifier : subkind -> quantifier * row

val fresh_presence_quantifier : subkind -> quantifier * field_spec
val fresh_flexible_presence_quantifier : subkind -> quantifier * field_spec

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
val make_endbang_type : datatype

(** subtyping *)
val is_sub_type : datatype * datatype -> bool
val is_sub_row : row * row -> bool

(** environments *)
type inference_type_map =
    ((datatype Unionfind.point) Utility.IntMap.t ref *
       (row Unionfind.point) Utility.IntMap.t ref)

val extend_typing_environment : typing_environment -> typing_environment -> typing_environment

val make_fresh_envs : datatype -> datatype Utility.IntMap.t * row Utility.IntMap.t * field_spec Utility.IntMap.t
val make_rigid_envs : datatype -> datatype Utility.IntMap.t * row Utility.IntMap.t * field_spec Utility.IntMap.t
val make_wobbly_envs : datatype -> datatype Utility.IntMap.t * row Utility.IntMap.t * field_spec Utility.IntMap.t

(** mailboxes *)
val show_mailbox_annotations : bool Settings.setting

(** pretty printing *)
val string_of_datatype   : ?policy:(unit -> Print.policy)
                        -> ?refresh_tyvar_names:bool -> datatype   -> string
val string_of_row        : ?policy:(unit -> Print.policy)
                        -> ?refresh_tyvar_names:bool -> row        -> string
val string_of_presence   : ?policy:(unit -> Print.policy)
                        -> ?refresh_tyvar_names:bool -> field_spec -> string
val string_of_type_arg   : ?policy:(unit -> Print.policy)
                        -> ?refresh_tyvar_names:bool -> type_arg   -> string
val string_of_row_var    : ?policy:(unit -> Print.policy)
                        -> ?refresh_tyvar_names:bool -> row_var    -> string
val string_of_tycon_spec : ?policy:(unit -> Print.policy)
                        -> ?refresh_tyvar_names:bool -> tycon_spec -> string
val string_of_environment        : environment -> string
val string_of_typing_environment : typing_environment -> string

(** generating type variable names *)
val build_tyvar_names : ('a -> Vars.vars_list)
                     -> ('a list)
                     -> unit
val add_tyvar_names : ('a -> Vars.vars_list)
                   -> ('a list)
                   -> unit
(* Function type constructors *)
val make_pure_function_type : datatype -> datatype -> datatype		   
val make_function_type      : datatype -> row -> datatype -> datatype
val make_thunk_type : row -> datatype -> datatype  
