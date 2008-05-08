(*pp deriving *)
(** Monadic IR *)

type scope = Var.scope
(* term variables *)
type var = Var.var
  deriving (Show)
type var_info = Var.var_info
type binder = Var.binder

(* type variables *)
type tyvar = Types.quantifier
type tyarg = Types.type_arg
type tybinder = tyvar list * binder

type name = string
type 'a name_map = 'a Utility.stringmap
  deriving (Show)

type language = string

type constant = Constant.constant
  deriving (Show)

type location = Syntax.location

type value =
  [ `Constant of constant
  | `Variable of var
  | `Extend of value name_map * value option
  | `Project of name * value
  | `Erase of name * value    (* should be implemented using coerce *) 
  | `Inject of name * value

  | `TApp of value * tyarg list

  | `XmlNode of name * value name_map * value list
  | `ApplyPure of value * value list
  | `Comparison of value * Syntaxutils.comparison * value    (* should really be implemented as constants *)

  | `Coerce of value * Types.datatype
  | `Abs of value
  ]
and tail_computation =
  [ `Return of value
  | `Apply of value * value list
  | `Special of special
  | `Case of value * (binder * computation) name_map * (binder * computation) option
  | `If of value * computation * computation
  ]
and binding =
  [ `Let of tybinder * tail_computation
  | `Fun of tybinder * binder list * computation * location
  | `Rec of (tybinder * binder list * computation * location) list
  | `Alien of tybinder * language
  | `Module of (string * binding list option) ]
and special =
  [ `App of value * value
  | `Wrong of Types.datatype
  | `Database of value
  | `Query of SqlQuery.sqlQuery
  | `Table of value * value * (Types.datatype * Types.datatype)
  | `CallCC of value ]
and computation = binding list * tail_computation
  deriving (Show)  

val letm : binder * tail_computation -> binding
val letmv : binder * value -> binding
val letv : tybinder * value -> binding

type program = computation

val is_atom : value -> bool

val with_bindings : binding list -> computation -> computation

val string_of_var : var -> string

val string_of_value : value -> string
val string_of_tail_computation : tail_computation -> string
val string_of_binding : binding -> string
val string_of_special : special -> string
val string_of_computation : computation -> string
val string_of_program : program -> string

module type TRANSFORM =
sig
  type environment = Types.datatype Env.Int.t
  type typing_environment = environment

  class visitor : typing_environment ->
  object ('self_type)
    val tyenv : typing_environment
    val tenv : environment

    method lookup_type : var -> Types.datatype
    method constant : constant -> (constant * Types.datatype * 'self_type)
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
