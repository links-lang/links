(*pp deriving *)

val prelude_primitive_names : string list

val prelude_primitives : unit -> unit

val prelude_primitive_vars : Utility.IntSet.t option ref
val prelude_primitive_namemap : string Utility.IntMap.t option ref

val complete_tyenv_ir : Types.datatype Env.Int.t -> Ir.computation -> Types.datatype Env.Int.t

val used_database : Value.database option ref

(* term variables *)
type var = Var.var
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
  | `Inject of name * qr

  | `Table of Value.table
  | `Singleton of qr
  | `Concat of qr list
  | `Apply of qr * qr list
  | `Case of qr * (var * qr) name_map * (var * qr) option
  | `If of qr * qr * qr option
  
  | `Computation of binding list * qr
  | `Primitive of string
  | `Lambda of Var.var list * qr

  | `Wrong ]
and binding = [ `Let of var * qr ]
    deriving (Show)

module ImpType :
sig

  type imptype = [`Atom | `List] deriving (Show)

  type tqr =
      [ `Lambda of ((Var.var list * tqr) * imptype)
      | `If of (tqr * tqr * tqr option) * imptype
      | `Table of Value.table * imptype
      | `Singleton of tqr * imptype 
      | `Concat of tqr list * imptype
      | `Extend of tqr name_map * tqr option * imptype
      | `Project of (string * tqr) * imptype
      | `Erase of (name_set * tqr) * imptype
      | `Inject of (string * tqr) * imptype
      | `Apply of (tqr * tqr list) * imptype
      | `Primitive of string 
      | `Variable of Var.var * imptype
      | `Constant of Constant.constant * imptype
      | `Box of tqr * imptype
      | `Unbox of tqr * imptype
      | `Case of (tqr * (Var.var * tqr) name_map * (Var.var * tqr) option) * imptype
      | `Computation of binding list * tqr * imptype
      | `Wrong of imptype ]
  and binding = [ `Let of Var.var * tqr ]
      deriving (Show)

  val typeof_tqr : tqr -> imptype

  val string_of_tqr : tqr -> string

end

val pipeline : Value.env -> Types.datatype Env.Int.t -> (Num.num * Num.num) option -> Ir.computation -> ImpType.tqr


