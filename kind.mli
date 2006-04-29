(* Representations of types *)

type type_var_set = Utility.IntSet.t

(* Types for kinds *)
type primitive = [ `Bool | `Int | `Char | `Float | `XMLitem ]

type collection_type = [`Set | `Bag | `List | `CtypeVar of int]

type ('typ, 'row, 'ctype) type_basis = [
  | `Not_typed
  | `Primitive of primitive
  | `TypeVar of int
  | `Function of ('typ * 'typ)
  | `Record of 'row
  | `Variant of 'row
  | `Recursive of (int * 'typ)
  | `Collection of ('ctype * 'typ)
  | `DB ]

type 'typ field_spec_basis = [ `Present of 'typ | `Absent ]
type 'typ field_spec_map_basis = ('typ field_spec_basis) Utility.StringMap.t
type ('typ, 'row_var) row_basis = 'typ field_spec_map_basis * 'row_var 
type 'row row_var_basis =
    [ `RowVar of int option 
    | `RecRowVar of int * 'row ]

type kind = (kind, row, collection_type) type_basis
and field_spec = kind field_spec_basis
and field_spec_map = kind field_spec_map_basis
and row_var = row row_var_basis
and row = (kind, row_var) row_basis

			  
type equivalence =   Var_equiv of (int * kind) 
                   | Row_equiv of (int * row)
                   | Colltype_equiv of (int * collection_type)
type substitution = (equivalence list)


type type_variable = [`TypeVar of int | `RowVar of int | `CtypeVar of int]
type quantifier = type_variable

type 'typ assumption_basis = ((quantifier list) * 'typ)
type 'typ environment_basis = ((string * 'typ assumption_basis) list)

type assumption = kind assumption_basis
type environment = kind environment_basis

val (-->) : kind -> kind -> kind


val split_fields : 'typ field_spec_map_basis -> (string * 'typ) list * string list
	
val get_present_fields : 'typ field_spec_map_basis -> (string * 'typ) list
val get_absent_fields : 'typ field_spec_map_basis -> string list

val string_type : kind
val xml : kind

(* Type printers *)
val coll_name : collection_type -> string 
val coll_prefix : string Utility.IntMap.t -> collection_type -> string
val string_of_primitive : primitive -> string

exception Not_tuple

val free_bound_type_vars : kind -> type_var_set
val free_bound_row_type_vars : row -> type_var_set

(* string conversions *)
val string_of_kind : kind -> string
val string_of_kind_raw : kind -> string

val string_of_row : row -> string

val string_of_quantifier : quantifier -> string
val string_of_assumption : assumption -> string
val string_of_environment : environment -> string

(* serialisation *) 
val serialise_colltype : collection_type Pickle.serialiser
val deserialise_colltype : collection_type Pickle.deserialiser

val serialise_primitive : primitive Pickle.serialiser 
val deserialise_primitive : primitive Pickle.deserialiser

val serialise_kind : kind Pickle.serialiser 
val serialise_field_spec : field_spec Pickle.serialiser
val serialise_row_var : row_var Pickle.serialiser
val serialise_row : char -> row Pickle.serialiser
  

val deserialise_kind : kind Pickle.deserialiser
val deserialise_field_spec : field_spec Pickle.deserialiser
val deserialise_row_var : row_var Pickle.deserialiser
val deserialise_row : row Pickle.deserialiser

val serialise_quantifier : quantifier Pickle.serialiser
val deserialise_quantifier : quantifier Pickle.deserialiser

val serialise_assumption : assumption Pickle.serialiser 
val deserialise_assumption : assumption Pickle.deserialiser

val serialise_environment : environment Pickle.serialiser
val deserialise_environment : environment Pickle.deserialiser

(* Generation of fresh type variables *)
val new_raw_variable : unit -> int

module type TYPEOPS =
sig
  type typ
  type row_var
  type collection_type

  type field_spec = typ field_spec_basis
  type field_spec_map = typ field_spec_map_basis
  type row = (typ, row_var) row_basis

  val make_type_var : int -> typ
  val make_row_var : int -> row_var
  val make_collection_var : int -> collection_type

  (* fresh type variable generation *)
  val new_type_variable : unit -> typ
  val new_row_variable : unit -> row_var
  val new_collection_variable : unit -> collection_type

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
  type collection_type'
 
  type field_spec = typ field_spec_basis
  type field_spec_map = typ field_spec_map_basis
  type row = (typ, row_var') row_basis

  val make_type_var : int -> typ
  val make_row_var : int -> row_var'
  val make_collection_var : int -> collection_type'

  val empty_field_env : typ field_spec_map_basis
  val closed_row_var : row_var'

  val is_closed_row : row -> bool
end

module BasicTypeOps :
  (BASICTYPEOPS with type typ = kind
		and type row_var' = row_var
		and type collection_type' = collection_type)

module TypeOpsGen(BasicOps: BASICTYPEOPS) :
  (TYPEOPS
   with type typ = BasicOps.typ 
   and type row_var = BasicOps.row_var'
   and type collection_type = BasicOps.collection_type'
)

module TypeOps :
  (TYPEOPS with type typ = kind
	   and type row_var = row_var
	   and type collection_type = collection_type)

val make_unit : unit -> kind
val make_empty_record_with_row_var : int -> kind

(* From library.ml; there's probably another name for these *)
val fresh_type_variable : unit -> [> `TypeVar of int]
val fresh_row_variable : unit -> [> `RowVar of int]
val fresh_collection_variable : unit -> [> `CtypeVar of int]

val new_type_variable : unit -> kind
val new_row_variable : unit -> row_var
val new_collection_variable : unit -> collection_type

(* Functions on environments *)
val lookup : string -> 'typ environment_basis -> 'typ assumption_basis
