(*pp deriving *)
(** Monadic IR *)

type scope = Var.scope
  deriving (Show, Pickle)

(* term variables *)
type var = Var.var
  deriving (Show, Pickle)
type var_info = Var.var_info
  deriving (Show)
type binder = Var.binder
  deriving (Show)

(* type variables *)
type tyvar = Types.quantifier
  deriving (Show)
type tyarg = Types.type_arg
  deriving (Show)

type name = string
  deriving (Show)
type 'a name_map = 'a Utility.stringmap
  deriving (Show)

type language = string

type constant = Constant.constant
  deriving (Show)

type location = Syntax.location
  deriving (Show)

(* INVARIANT: all IR binders have unique names *)

type value =
  [ `Constant of constant
  | `Variable of var
  | `Extend of value name_map * value option
  | `Project of name * value
  | `Erase of name * value    (* should be implemented using coerce *) 
  | `Inject of name * value * Types.datatype

  | `TAbs of tyvar list * value
  | `TApp of value * tyarg list

  | `XmlNode of name * value name_map * value list
  | `ApplyPure of value * value list
  | `Comparison of value * Syntaxutils.comparison * value    (* should really be implemented as constants *)

  | `Coerce of value * Types.datatype
  ]
and tail_computation =
  [ `Return of value
  | `Apply of value * value list
  | `Special of special
  | `Case of value * (binder * computation) name_map * (binder * computation) option
  | `If of value * computation * computation
  ]
and binding =
  [ `Let of binder * (tyvar list * tail_computation)
  | `Fun of binder * (tyvar list * binder list * computation) * location
  | `Rec of (binder * (tyvar list * binder list * computation) * location) list
  | `Alien of binder * language
  | `Module of (string * binding list option) ]
and special =
  [ `Wrong of Types.datatype
  | `Database of value
  | `SqlQuery of SqlQuery.sqlQuery
  | `Table of value * value * (Types.datatype * Types.datatype)
  | `Query of (value * value) option * computation * Types.datatype
  | `CallCC of value ]
and computation = binding list * tail_computation
  deriving (Show, Pickle)

val letm : binder * tail_computation -> binding
val letmv : binder * value -> binding
(*val letv : tybinder * value -> binding*)

type program = computation
  deriving (Show, Pickle)

val is_atom : value -> bool

val with_bindings : binding list -> computation -> computation

val string_of_var : var -> string

val string_of_value : value -> string
val string_of_tail_computation : tail_computation -> string
val string_of_binding : binding -> string
val string_of_special : special -> string
val string_of_computation : computation -> string
val string_of_program : program -> string

val string_of_ir : string Utility.IntMap.t -> program -> string

module type TRANSFORM =
sig
  type environment = Types.datatype Env.Int.t

  class visitor : environment ->
  object ('self_type)
    val tyenv : environment

    method lookup_type : var -> Types.datatype
    method constant : constant -> (constant * Types.datatype * 'self_type)
    method optionu :
      'a.
      ('self_type -> 'a -> ('a * 'self_type)) ->
      'a option -> 'a option * 'self_type
    method option :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype * 'self_type)) ->
      'a option -> 'a option * Types.datatype option * 'self_type
    method list :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype * 'self_type)) ->
      'a list -> 'a list * Types.datatype list * 'self_type
    method name_map :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype * 'self_type)) ->
      'a name_map -> 'a name_map * Types.datatype name_map * 'self_type        
    method var : var -> (var * Types.datatype * 'self_type)
    method value : value -> (value * Types.datatype * 'self_type)
                                                
    method tail_computation :
      tail_computation -> (tail_computation * Types.datatype * 'self_type)
    method special : special -> (special * Types.datatype * 'self_type)      
    method bindings : binding list -> (binding list * 'self_type)
    method computation : computation -> (computation * Types.datatype * 'self_type)
    method binding : binding -> (binding * 'self_type)
    method binder : binder -> (binder * 'self_type)

    method program : program -> (program * Types.datatype * 'self_type)

    method get_type_environment : environment
  end  
end

module Transform : TRANSFORM

module Inline :
sig
  val program : Types.datatype Env.Int.t -> program -> program
end

module ElimDeadDefs :
sig
  val program : Types.datatype Env.Int.t -> program -> program
end

type closures = Utility.intset Utility.intmap
    deriving (Show, Pickle)

module ClosureTable :
sig
  type t = closures

  val value : Types.datatype Env.Int.t -> value -> t
  val tail_computation : Types.datatype Env.Int.t -> tail_computation -> t
  val computation : Types.datatype Env.Int.t -> computation -> t
  val bindings : Types.datatype Env.Int.t -> binding list -> t
  val program : Types.datatype Env.Int.t -> program -> t
end
