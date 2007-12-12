(*pp deriving *)
(** Monadic IR *)

type scope = [ `Local | `Global ]
  deriving (Show)
(* term variables *)
type var = int
type var_info = Types.datatype * string * scope
type binder = var * var_info

(* type variables *)
type tyvar = int
type tyname = string
(* type tybinder = tyvar * var_info *)

type name = string
type 'a name_map = 'a Utility.stringmap
  deriving (Show)

type language = string

(*
type constant =
  | Boolean of bool
  | Integer of Num.num
  | Char of char
  | String of string
  | Float of float
*)

type constant = Syntax.constant
  deriving (Show)

type location = Syntax.location

type value =
  [ `Constant of constant
  | `Variable of var
  | `Extend of value name_map * value option
  | `Project of name * value
  | `Erase of name * value    (* should be implemented using coerce *) 
  | `Inject of name * value

  | `XmlNode of name * value name_map * value list
  | `ApplyPrim of value * value list
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
  [ `Let of binder * tail_computation
  | `Fun of binder * binder list * computation * location
  | `Rec of (binder * binder list * computation * location) list
  | `Alien of binder * language
  | `Alias of tyname * tyvar list * Types.datatype ]
and special =
  [ `App of value * value
  | `Wrong of Types.datatype
  | `Database of value
  | `Query of SqlQuery.sqlQuery
  | `Table of value * value * (Types.datatype * Types.datatype)
  | `CallCC of value ]
and computation = binding list * tail_computation
  deriving (Show)  

type program = computation

val is_atom : value -> bool

module MapTy :
sig
  type environment = Types.datatype Env.Int.t
  type alias_environment = Types.alias_environment
  type typing_environment = environment * alias_environment

  class maptyenv : typing_environment ->
  object ('self_type)
    val tyenv : typing_environment
    val tenv : environment
    val alias_env : alias_environment

    method lookup_type : var -> Types.datatype
    method constant : constant -> (constant * Types.datatype)
    method option :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype)) ->
      'a option -> 'a option * Types.datatype option
    method list :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype)) ->
      'a list -> 'a list * Types.datatype list
    method name_map :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype)) ->
      'a name_map -> 'a name_map * Types.datatype name_map        
    method var : var -> (var * Types.datatype)       
    method value : value -> (value * Types.datatype)
                                                
    method tail_computation :
      tail_computation -> (tail_computation * Types.datatype)                 
    method special : special -> (special * Types.datatype)      
    method bindings : binding list -> (binding list * 'self_type)
    method computation : computation -> (computation * Types.datatype)
    method binding : binding -> (binding * 'self_type)
    method binder : binder -> (binder * 'self_type)
  end  
end


module Inline :
sig
  val program : (Types.datatype Env.Int.t * Types.alias_environment) -> program -> program
end
