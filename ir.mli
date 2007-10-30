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
  | `Alien of binder * language * Types.assumption
  | `Alias of tyname * tyvar list * Types.datatype ]
and special =
  [ `App of value * value
  | `Wrong
  | `Database of value
  | `TableQuery of value name_map * SqlQuery.sqlQuery
  | `TableHandle of value * value * (Types.datatype * Types.datatype)
  | `CallCC of value ]
and computation = binding list * tail_computation
  deriving (Show)  

type program = computation

val is_atom : value -> bool

module Inline :
sig
  val program : program -> program
end

class map :
  object
    method array : ('c -> 'd) -> 'c array -> 'd array
    method bool : bool -> bool
    method char : char -> char
    method float : float -> float
    method int : tyvar -> tyvar
    method list : ('e -> 'f) -> 'e list -> 'f list
    method option : ('i -> 'j) -> 'i option -> 'j option
    method ref : ('k -> 'l) -> 'k ref -> 'l ref

    method _SqlQuery_sqlQuery : SqlQuery.sqlQuery -> SqlQuery.sqlQuery
    method _Syntax_constant : constant -> constant
    method _Syntax_location : location -> location
    method _Syntaxutils_comparison : Syntaxutils.comparison -> Syntaxutils.comparison
    method _Types_assumption : Types.assumption -> Types.assumption
    method _Types_datatype : Types.datatype -> Types.datatype
    method _Utility_stringmap : ('a -> 'b) -> 'a Utility.stringmap -> 'b Utility.stringmap

    method binder : binder -> binder
    method binding : binding -> binding
    method computation : program -> program
    method constant : constant -> constant
    method language : language -> language
    method location : location -> location
    method name : language -> language
    method name_map : ('g -> 'h) -> 'g name_map -> 'h name_map
    method program : program -> program
    method scope : scope -> scope
    method special : special -> special
    method string : language -> language
    method tail_computation : tail_computation -> tail_computation
    method tyname : language -> language
    method tyvar : tyvar -> tyvar
    method value : value -> value
    method var : tyvar -> tyvar
    method var_info : var_info -> var_info
  end

class fold :
  object ('a)
    method array : ('a -> 'c -> 'a) -> 'c array -> 'a
    method bool : bool -> 'a
    method char : char -> 'a
    method float : float -> 'a
    method int : tyvar -> 'a
    method option : ('a -> 'f -> 'a) -> 'f option -> 'a
    method ref : ('a -> 'g -> 'a) -> 'g ref -> 'a
    method string : language -> 'a
    method list : ('a -> 'd -> 'a) -> 'd list -> 'a

    method _SqlQuery_sqlQuery : SqlQuery.sqlQuery -> 'a
    method _Syntax_constant : constant -> 'a
    method _Syntax_location : location -> 'a
    method _Syntaxutils_comparison : Syntaxutils.comparison -> 'a
    method _Types_assumption : Types.assumption -> 'a
    method _Types_datatype : Types.datatype -> 'a
    method _Utility_stringmap : ('a -> 'b -> 'a) -> 'b Utility.stringmap -> 'a

    method binder : binder -> 'a
    method binding : binding -> 'a
    method computation : program -> 'a
    method constant : constant -> 'a
    method language : language -> 'a
    method location : location -> 'a
    method name : language -> 'a
    method name_map : ('a -> 'e -> 'a) -> 'e name_map -> 'a
    method program : program -> 'a
    method scope : scope -> 'a
    method special : special -> 'a
    method tail_computation : tail_computation -> 'a
    method tyname : language -> 'a
    method tyvar : tyvar -> 'a
    method value : value -> 'a
    method var : tyvar -> 'a
    method var_info : var_info -> 'a
  end
