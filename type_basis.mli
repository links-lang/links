(* Basis for types *)

type type_var_set = Utility.IntSet.t

(* Types for kinds *)
type primitive = [ `Bool | `Int | `Char | `Float | `XMLitem ]

type ('typ, 'row) type_basis = [
  | `Not_typed
  | `Primitive of primitive
  | `TypeVar of int
  | `Function of ('typ * 'typ)
  | `Record of 'row
  | `Variant of 'row
  | `Recursive of (int * 'typ)
  | `List of ('typ)
  | `DB ]

type 'typ field_spec_basis = [ `Present of 'typ | `Absent ]
type 'typ field_spec_map_basis = ('typ field_spec_basis) Utility.StringMap.t
type ('typ, 'row_var) row_basis = 'typ field_spec_map_basis * 'row_var 
type 'row row_var_basis =
    [ `RowVar of int option 
    | `RecRowVar of int * 'row ]

type type_variable = [`TypeVar of int | `RowVar of int]
type quantifier = type_variable

type 'typ assumption_basis = ((quantifier list) * 'typ)
type 'typ environment_basis = ((string * 'typ assumption_basis) list)

val environment_values : 'typ environment_basis -> 'typ assumption_basis list
val lookup : string -> 'typ environment_basis -> 'typ assumption_basis

(* Generation of fresh type variables *)
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
  val make_row_variable : int -> row_var'

  val empty_field_env : typ field_spec_map_basis
  val closed_row_var : row_var'

  val is_closed_row : row -> bool
end

module TypeOpsGen(BasicOps: BASICTYPEOPS) :
  (TYPEOPS
   with type typ = BasicOps.typ 
   and type row_var = BasicOps.row_var'
)
