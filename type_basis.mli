(*pp deriving *)
(** Low-level representation of Links types. *)

(** {1 Representation of datatypes} *)

type primitive = [ `Bool | `Int | `Char | `Float | `XmlItem | `DB | `Abstract of string ]

type ('typ, 'row) type_basis = [
  | `Not_typed
  | `Primitive of primitive
  | `TypeVar of int
  | `RigidTypeVar of int
  | `Function of ('typ * 'typ * 'typ)
  | `Record of 'row
  | `Variant of 'row
  | `Table of 'row
  | `Recursive of (int * 'typ)
  | `Application of (string * 'typ list)
 ] deriving (Show,Pickle)

type 'typ field_spec_basis = [ `Present of 'typ | `Absent ] 
and 'typ field_spec_map_basis = ('typ field_spec_basis) Utility.StringMap.t 
and ('typ, 'row_var) row_basis = 'typ field_spec_map_basis * 'row_var 
and 'row row_var_basis =
    [ `RowVar of int option 
    | `RecRowVar of int * 'row ] deriving (Show, Pickle)

type type_variable = [`TypeVar of int | `RigidTypeVar of int | `RowVar of int]
type quantifier = type_variable

type type_var_set = Utility.IntSet.t

type 'typ assumption_basis = ((quantifier list) * 'typ) 
and 'typ environment_basis = ((string * 'typ assumption_basis) list) deriving (Show, Pickle)

type 'typ alias_environment_basis = ('typ assumption_basis) Utility.StringMap.t

val environment_values : 'typ environment_basis -> 'typ assumption_basis list
val lookup : string -> 'typ environment_basis -> 'typ assumption_basis
  (** [lookup var typing_env] returns the type of the variable [var] in
      the environment [typing_env]*)

(** Generate a fresh type variable *)
val fresh_raw_variable : unit -> int

module type TYPEOPS =
sig
  type typ
  type row_var

  type field_spec = typ field_spec_basis
  type field_spec_map = typ field_spec_map_basis
  type row = (typ, row_var) row_basis

  (* fresh type variable generation *)
  val fresh_type_variable : unit -> typ
  val fresh_rigid_type_variable : unit -> typ
  val fresh_row_variable : unit -> row_var

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
  val empty_field_env : typ field_spec_map_basis
  val closed_row_var : row_var
end

module type BASICTYPEOPS =
sig
  type typ
  type row_var'
 
  type field_spec = typ field_spec_basis
  type field_spec_map = typ field_spec_map_basis
  type row = (typ, row_var') row_basis

  val make_type_variable : int -> typ
  val make_rigid_type_variable : int -> typ
  val make_row_variable : int -> row_var'

  val empty_field_env : typ field_spec_map_basis
  val closed_row_var : row_var'

  val is_closed_row : row -> bool
  val get_row_var : row -> int option
end

module TypeOpsGen(BasicOps: BASICTYPEOPS) :
  (TYPEOPS
   with type typ = BasicOps.typ 
   and type row_var = BasicOps.row_var'
)
