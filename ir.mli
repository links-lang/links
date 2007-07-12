(** Monadic IR *)


type scope = [ `Local | `Global ]

(* term variables *)
type var = int
type var_info = Types.datatype * string * scope
type binder = var * var_info

(* type variables *)
type tyvar = int
type tyname = string
(* type tybinder = tyvar * var_info *)

type name = string
type 'a name_map = 'a Utility.StringMap.t

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

type location = Syntax.location

type value =
  [ `Constant of constant
  | `Variable of var
  | `Extend of (value name_map * value option)
  | `Project of (name * value)
  | `Inject of (name * value)

  | `Nil
  | `Cons of (value * value)
  | `Concat of (value * value)
  | `XmlNode of (name * value name_map * value list)

  | `Coerce of (value * Types.datatype)
  | `Abs of value
  ]
and tail_computation =
  [ `Return of (value)
  | `Apply of (value * value list)

  | `Special of special

  | `Case of (value * (binder * computation) name_map * (binder * computation) option)
  | `If of (value * computation * computation)
  ]
and binding =
  [ `Let of (binder * tail_computation)
  | `Fun of (binder * binder list * computation * location)
  | `Rec of (binder * binder list * computation * location) list
  | `Alien of (binder * language * Types.assumption)
  | `Alias of (tyname * tyvar list * Types.datatype)
  | `For of (binder * value) ]
and special =
  [ `App of value * value
  | `Wrong
  | `Database of (value)
  | `TableQuery of (value name_map * Query.query)
  | `TableHandle of (value * value * (Types.datatype * Types.datatype))
  | `SortBy of (value * value)
  | `CallCC of (value) ]
and computation = binding list * tail_computation

type program = computation
