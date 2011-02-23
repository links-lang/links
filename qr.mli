(*pp deriving *)

val prelude_primitive_names : string list

val prelude_primitives : unit -> unit

val prelude_primitive_vars : Utility.IntSet.t option ref
val prelude_primitive_namemap : string Utility.IntMap.t option ref

val complete_tyenv : Types.datatype Env.Int.t -> Ir.computation -> Types.datatype Env.Int.t

type scope = Var.scope
  deriving (Show)

(* term variables *)
type var = Var.var
  deriving (Show)
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

type name_set = Utility.stringset
  deriving (Show)

type 'a name_map = 'a Utility.stringmap
  deriving (Show)

type constant = Constant.constant
  deriving (Show)

type qr =
  [ `Constant of constant
  | `Variable of var
  | `Extend of qr name_map * qr option
  | `Project of name * qr
  | `Erase of name_set * qr
  | `Inject of name * qr * Types.datatype

  | `TApp of qr * tyarg list
  | `TAbs of tyvar list * qr
  
  | `Database of Value.database * string
  | `Table of Value.table
  | `List of qr list
  | `PrimitiveFun of string * Var.var option

  | `Apply of qr * qr list
  | `Case of qr * (binder * qr) name_map * (binder * qr) option
  | `If of qr * qr * qr
  | `Computation of binding list * qr
  
  | `Wrong of Types.datatype ]
and funct = binder * binder list * qr * tyvar list
and binding = 
  [ `Let of (binder * tyvar list * qr)
  | `Fun of funct ]
and env = qr Env.Int.t 
    deriving (Show)

val computation : Ir.computation -> qr

val qr_of_query : Types.datatype Env.Int.t -> Value.env -> Ir.computation -> (qr * Types.datatype Env.Int.t)

val type_qr : Types.datatype Env.Int.t -> qr -> Types.datatype


