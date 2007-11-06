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
  | `Alien of binder * language * Types.datatype
  | `Alias of tyname * tyvar list * Types.datatype ]
and special =
  [ `App of value * value
  | `Wrong
  | `Database of value
  | `Query of SqlQuery.sqlQuery
  | `Table of value * value * (Types.datatype * Types.datatype)
  | `CallCC of value ]
and computation = binding list * tail_computation
  deriving (Show)  

type program = computation

val is_atom : value -> bool

module Inline :
sig
  val program : program -> program
end

