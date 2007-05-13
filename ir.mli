(** Monadic IR *)

(* term variables *)
type var = int
type var_info = Types.datatype * string
type binder = var * var_info

(* type variables *)
type tyvar = int
type tyname = string
(* type tybinder = tyvar * var_info *)

type name = string
type 'a name_map = 'a Utility.StringMap.t

(*
type constant =
  | Boolean of bool
  | Integer of Num.num
  | Char of char
  | String of string
  | Float of float
*)

type constant = Syntax.constant

type value =
  [ `Constant of constant
  | `Variable of var
  | `Extend of (value name_map * value option)
  | `Project of (name * value)
  | `Inject of (name * value)

  | `Nil
  | `Cons of (value * value)
  | `XmlNode of (name * value name_map * value list)
  | `Coerce of (value * Types.datatype * Types.datatype)
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
  | `Abs of (binder * binder list * computation)
  | `Rec of (binder * binder list * computation) list
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

type definition =
  [ `Define of (binder * computation * Syntax.location)
  | `Alias of (tyname * tyvar list * Types.datatype)
  | `Alien of (binder * string * Types.assumption)
  ]

type program = definition list * computation
