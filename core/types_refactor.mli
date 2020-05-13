(** Core types *)
open CommonTypes

(* field environments *)
type 'a stringmap = 'a Utility.StringMap.t [@@deriving show]
type 'a field_env = 'a stringmap [@@deriving show]

(* type var sets *)
module TypeVarSet : sig
  include Utility.INTSET

  val add_quantifiers : Quantifier.t list -> t -> t
end

module TypeVarMap : Utility.INTMAP

val tag_expectation_mismatch : exn

(* points *)
type 'a point = 'a Unionfind.point

module Abstype :
sig
  type t [@@deriving eq,show]
  val make  : string -> Kind.t list -> t
  val arity : t -> Kind.t list
  val name  : t -> string
  val compare : t -> t -> int
end

module Vars : sig
  type flavour = [`Rigid | `Flexible | `Recursive]
  type kind    = PrimaryKind.t
  type scope   = [`Free | `Bound]
  type vars_list = (int * (flavour * kind * scope)) list
end

module Print : sig
  type policy = {quantifiers:bool; flavours:bool; hide_fresh:bool; kinds:string; effect_sugar:bool}

  val default_policy : unit -> policy
end

val process      : Abstype.t
val list         : Abstype.t
val event        : Abstype.t
val dom_node     : Abstype.t
val access_point : Abstype.t
val socket       : Abstype.t
val spawn_location : Abstype.t

(* Type groups *)

type rec_id =
  | MuBoundId of int
  | NominalId of string [@@deriving show]

module type RECIDMAP = Utility.Map with type key = rec_id
module RecIdMap : RECIDMAP
module type RECIDSET = Utility.Set with type elt = rec_id
module RecIdSet : RECIDSET

type tygroup = {
  id: int;
  type_map: ((Quantifier.t list * typ) Utility.StringMap.t);
  linearity_map: bool Utility.StringMap.t
}

(* Types *)
and rec_appl = {
  r_name: string;
  r_dual: bool;
  r_unique_name: string;
  r_quantifiers : Kind.t list;
  r_args: typ list;
  r_unwind: typ list -> bool -> typ;
  r_linear: unit -> bool option
  }
and tid = int
and typ =
  (* Unspecified kind *)
  | Not_typed
  | Var of (tid * Kind.t * Freedom.t)
  | Recursive of (tid * Kind.t * typ)
  | Alias of ((string * Kind.t list * type_arg list) * typ)
  | Application of (Abstype.t * type_arg list)
  | RecursiveApplication of rec_appl
  | Meta of typ point
  (* Type *)
  | Primitive of Primitive.t
  | Function of (typ * row * typ)
  | Lolli of (typ * row * typ)
  | Record of row
  | Variant of row
  | Table of (typ * typ * typ)
  | Lens of Lens.Type.t
  | ForAll of (Quantifier.t list * typ)
  (* Effect *)
  | Effect of row
  (* Row *)
  | Row of (field_spec_map * row_var * bool)
  | Closed
  (* Presence *)
  | Absent
  | Present of typ
  (* Session *)
  | Input of (typ * session_type)
  | Output of (typ * session_type)
  | Select of row
  | Choice of row
  | Dual of typ
  | End
and t = typ
and session_type = typ
and datatype = typ
and type_arg = typ
and field_spec = typ
and field_spec_map = field_spec Utility.StringMap.t
and meta_type_var = typ point
and meta_row_var = row point
and meta_presence_var = typ point
and meta_var = typ point
and row = typ
and row' = field_spec_map * row_var * bool
and row_var = meta_row_var
  [@@deriving show]

(** A constraint that a subkind imposes on types. *)
module type Constraint = sig
  val type_satisfies : datatype -> bool
  val row_satisfies : row -> bool

  (** Can this type be modified using {!make_type} to satisfy this constraint?
     *)
  val can_type_be : datatype -> bool
  val can_row_be : row -> bool

  (** Attempt to modify this type to satisfy this constraint. One should call
     {!can_type_be} before calling this.

     This will attempt to convert any flexible type variables with compatible
     subkinds to one with a more restrictive one, so that {!is_type} now returns
     true. *)
  val make_type : datatype -> unit
  val make_row : row -> unit
end

module Base : Constraint
module Unl : Constraint
module Session : Constraint
module Mono : Constraint

(** Get a {!Constraint} for a specific subkind {!Restriction.t}. *)
val get_restriction_constraint : Restriction.t -> (module Constraint) option

val dual_row : row -> row
val dual_type : datatype -> datatype

type alias_type = Quantifier.t list * typ [@@deriving show]

type tycon_spec = [
  | `Alias of alias_type
  | `Abstract of Abstype.t
  | `Mutual of (Quantifier.t list * tygroup ref) (* Type in same recursive group *)
]

type environment        = datatype Env.String.t
type tycon_environment  = tycon_spec Env.String.t
type typing_environment = { var_env    : environment ;
                            rec_vars   : Utility.StringSet.t ;
                            tycon_env  : tycon_environment ;
                            effect_row : row ;
                            desugared : bool }

val empty_typing_environment : typing_environment

val concrete_type : datatype -> datatype
val concrete_field_spec : field_spec -> field_spec

val normalise_datatype : datatype -> datatype
val normalise_row : row -> row
val normalise_typing_environment : typing_environment -> typing_environment

val for_all : Quantifier.t list * datatype -> datatype

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
val empty_type : datatype

(** get type variables *)
val free_type_vars : datatype -> TypeVarSet.t
val free_row_type_vars : row -> TypeVarSet.t
val free_tyarg_vars : type_arg -> TypeVarSet.t
val free_bound_type_vars          : typ      -> Vars.vars_list
val free_bound_row_type_vars      : row      -> Vars.vars_list
val free_bound_type_arg_type_vars : type_arg -> Vars.vars_list

val type_arg_of_quantifier : Quantifier.t -> type_arg
val quantifier_of_type_arg : type_arg -> Quantifier.t
val quantifiers_of_type_args : type_arg list -> Quantifier.t list

val primary_kind_of_type_arg : type_arg -> PrimaryKind.t

(** Fresh type variables *)
val type_variable_counter : int ref
val fresh_raw_variable : unit -> int

(** type variable construction *)
val make_type_variable : int -> Subkind.t -> datatype
val make_rigid_type_variable : int -> Subkind.t -> datatype
val make_row_variable : int -> Subkind.t -> row_var
val make_rigid_row_variable : int -> Subkind.t -> row_var
val make_rigid_presence_variable : int -> Subkind.t -> field_spec
val make_rigid_variable : int -> Kind.t -> datatype

(** fresh type variable generation *)
val fresh_type_variable : Subkind.t -> datatype
val fresh_rigid_type_variable : Subkind.t -> datatype

val fresh_row_variable : Subkind.t -> row_var
val fresh_rigid_row_variable : Subkind.t -> row_var

val fresh_session_variable : Linearity.t -> datatype

val fresh_presence_variable : Subkind.t -> field_spec
val fresh_rigid_presence_variable : Subkind.t -> field_spec

(** fresh quantifiers *)
val fresh_type_quantifier : Subkind.t -> Quantifier.t * datatype
val fresh_row_quantifier : Subkind.t -> Quantifier.t * row
val fresh_presence_quantifier : Subkind.t -> Quantifier.t * field_spec
val fresh_quantifier : Kind.t -> Quantifier.t * type_arg

(** {0 rows} *)
(** empty row constructors *)
val make_empty_closed_row : unit -> row
val make_empty_open_row : Subkind.t -> row

(** singleton row constructors *)
val make_singleton_closed_row : (string * field_spec) -> row
val make_singleton_open_row : (string * field_spec) -> Subkind.t -> row

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
val extend_row_safe : datatype field_env -> row -> row option

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
val unwrap_list_type : typ -> typ

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

val effect_sugar : bool Settings.setting

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
val build_tyvar_names : refresh_tyvar_names:bool
                     -> ('a -> Vars.vars_list)
                     -> ('a list)
                     -> unit
val add_tyvar_names : ('a -> Vars.vars_list)
                   -> ('a list)
                   -> unit
(* Function type constructors *)
val make_pure_function_type : datatype list -> datatype -> datatype
val make_function_type      : ?linear:bool -> datatype list -> row -> datatype -> datatype
val make_thunk_type : row -> datatype -> datatype

(* val pp_datatype : Format.formatter -> datatype -> unit *)
val pp_tycon_spec: Format.formatter -> tycon_spec -> unit

(* Recursive type applications *)
val recursive_applications : datatype -> string list


module type TYPE_VISITOR =
sig
  class visitor :
  object ('self_type)
    method remove_rec_row_binding : int -> 'self_type
    method remove_rec_type_binding : int ->'self_type

    method primitive : Primitive.t -> (Primitive.t * 'self_type)
    method list : ('self_type -> 'a -> 'b * 'self_type) -> 'a list -> ('b list * 'self_type)
    method types : typ list -> (typ list * 'self_type)
    method typ : typ -> (typ * 'self_type)
    method row : row -> (row * 'self_type)
    method row_var : row_var -> (row_var * 'self_type)
    method meta_type_var : meta_type_var -> (meta_type_var * 'self_type)
    method meta_row_var : meta_row_var -> (meta_row_var * 'self_type)
    method meta_var : meta_var -> (meta_var * 'self_type)
    method meta_presence_var : meta_presence_var -> (meta_presence_var * 'self_type)
    method field_spec : field_spec -> (field_spec * 'self_type)
    method field_spec_map : field_spec_map -> (field_spec_map * 'self_type)
    method quantifier : Quantifier.t -> (Quantifier.t * 'self_type)
    method type_arg : type_arg -> (type_arg * 'self_type)
  end
end

type visit_context = Utility.StringSet.t * TypeVarSet.t * TypeVarSet.t
class virtual type_predicate :
  object('self_type)
    method var_satisfies : (int * Kind.t * Freedom.t) -> bool
    method type_satisfies : visit_context -> typ -> bool
    method point_satisfies : (visit_context -> typ -> bool) -> visit_context -> typ point -> bool
    method field_satisfies : visit_context -> field_spec -> bool
    method row_satisfies : visit_context -> row -> bool
    method type_satisfies_arg : visit_context -> type_arg -> bool
    method predicates : ((typ -> bool) * (row -> bool))
  end

module Transform : TYPE_VISITOR
module ElimRecursiveTypeCyclesTransform : TYPE_VISITOR
