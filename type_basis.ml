(* Basis for types *)

open Utility

type type_var_set = Utility.IntSet.t


type primitive = [ `Bool | `Int | `Char | `Float | `XMLitem | `Abstract of string ]
    deriving (Show, Pickle)

type ('typ, 'row) type_basis = [
  | `Not_typed
  | `Primitive of primitive
  | `TypeVar of int
  | `Function of ('typ * 'typ)
  | `Record of 'row
  | `Variant of 'row
  | `Recursive of (int * 'typ)
  | `List of ('typ)
  | `Mailbox of ('typ)
  | `DB ]
    deriving (Show, Pickle)

type 'a stringmap = 'a Utility.StringMap.t

module Show_stringmap (A : Show) = Show_unprintable (struct type a = A.a stringmap end)
module Pickle_stringmap (A : Pickle) = Pickle_unpicklable (struct type a = A.a stringmap end)


type 'typ field_spec_basis = [ `Present of 'typ | `Absent ]     deriving (Show, Pickle)
type 'typ field_spec_map_basis = ('typ field_spec_basis) stringmap     deriving (Show, Pickle)
type ('typ, 'row_var) row_basis = 'typ field_spec_map_basis * 'row_var      deriving (Show, Pickle)
type 'row row_var_basis =
    [ `RowVar of int option 
    | `RecRowVar of int * 'row ]
    deriving (Show, Pickle)

type type_variable = [`TypeVar of int | `RowVar of int]
    deriving (Show, Pickle)
type quantifier = type_variable
    deriving (Show, Pickle)

type 'typ assumption_basis = ((quantifier list) * 'typ)
    deriving (Show, Pickle)
type 'typ environment_basis = ((string * 'typ assumption_basis) list)
    deriving (Show, Pickle)

(* Functions on environments *)
let environment_values = fun env -> snd (List.split env)
let lookup = fun x -> List.assoc x


(* Generation of fresh type variables *)
let type_variable_counter = ref 0

let fresh_raw_variable : unit -> int =
  function () -> 
    incr type_variable_counter; !type_variable_counter



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
) =
struct
  type typ = BasicOps.typ
  type row_var = BasicOps.row_var'

  type field_spec = BasicOps.field_spec
  type field_spec_map = BasicOps.field_spec_map
  type row = BasicOps.row

  let get_row_var = BasicOps.get_row_var

  let is_closed_row = BasicOps.is_closed_row

  let fresh_type_variable = BasicOps.make_type_variable -<- fresh_raw_variable
  let fresh_row_variable = BasicOps.make_row_variable -<- fresh_raw_variable

  let empty_field_env = BasicOps.empty_field_env
  let closed_row_var = BasicOps.closed_row_var

  let make_empty_closed_row () = empty_field_env, closed_row_var
  let make_empty_open_row () = empty_field_env, fresh_row_variable ()
  let make_empty_open_row_with_var var = empty_field_env, BasicOps.make_row_variable var

  let make_singleton_closed_row (label, field_spec) =
    StringMap.add label field_spec empty_field_env, closed_row_var
  let make_singleton_open_row (label, field_spec) =
    StringMap.add label field_spec empty_field_env, fresh_row_variable ()
  let make_singleton_open_row_with_var (label, field_spec) var =
    StringMap.add label field_spec empty_field_env, BasicOps.make_row_variable var

  let is_absent_from_row label (field_env, _ as row) =
    if StringMap.mem label field_env then
      StringMap.find label field_env = `Absent
    else
      is_closed_row row

  let set_field (label, f) (field_env, row_var) =
    StringMap.add label f field_env, row_var
end
