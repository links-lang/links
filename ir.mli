(*pp deriving *)
(** Monadic IR *)

type scope = Var.scope
  deriving (Show)

(* term variables *)
type var = Var.var
  deriving (Show, Eq, Typeable, Pickle, Dump)
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

type 'a var_map = 'a Utility.intmap
  deriving (Show)

type language = string

type constant = Constant.constant
  deriving (Show)

type location = Sugartypes.location
  deriving (Show)

(* INVARIANT: all IR binders have unique names *)

type value =
  [ `Constant of constant                     (* constant: c *)
  | `Variable of var                          (* variable use: x *)
  | `Extend of value name_map * value option  (* record extension: (l1=v1, ..., lk=vk|r) or (l1=v1, ..., lk=vk) *)
  | `Project of name * value                  (* record projection: r.l *)
  | `Erase of name_set * value                (* erase fields from a record: r\{ls} *)
  | `Inject of name * value * Types.datatype  (* variant injection: L(v) *)

  | `TAbs of tyvar list * value       (* type abstraction: /\xs.v *)
  | `TApp of value * tyarg list       (* type application: v ts *)

  | `XmlNode of name * value name_map * value list
                                      (* XML node construction: <tag attributes>body</tag> *)
  | `ApplyPure of value * value list  (* non-side-effecting application: v ws *)

  | `Closure of var * value           (* closure creation: f env *)

  | `Coerce of value * Types.datatype (* type coercion: v:A *)

  | `Premarshaled of string   (* Unusuable on the client, but can be unpacked and used upon returning to the server *)
  ]
and tail_computation =
  [ `Return of value
  | `Apply of value * value list
  (* | `ApplyClosure of value * value list *)
  | `Special of special
  | `Case of value * (binder * computation) name_map * (binder * computation) option
  | `If of value * computation * computation
  ]
and fun_def = binder * (tyvar list * binder list * computation) * binder option * location
and binding =
  [ `Let of binder * (tyvar list * tail_computation)
  | `Fun of fun_def
  | `Rec of fun_def list
  | `Alien of binder * language
  | `Module of (string * binding list option) ]
and special =
  [ `Wrong of Types.datatype
  | `Database of value
  | `Table of value * value * value * (Types.datatype * Types.datatype * Types.datatype)
  | `Query of (value * value) option * computation * Types.datatype
  | `Update of (binder * value) * computation option * computation
  | `Delete of (binder * value) * computation option
  | `CallCC of value
  | `Select of (name * value)
  | `Choice of (value * (binder * computation) name_map) ]
and computation = binding list * tail_computation
  deriving (Show)

val binding_scope : binding -> scope
val binder_of_fun_def : fun_def -> binder

val tapp : value * tyarg list -> value

val letm : binder * tail_computation -> binding
val letmv : binder * value -> binding
(*val letv : tybinder * value -> binding*)

type program = computation
  deriving (Show)

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

  class visitor : environment ->
  object ('self_type)
    val tyenv : environment

    method private lookup_type : var -> Types.datatype
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
    method var_map :
      'a.
      ('self_type -> 'a -> ('a * Types.datatype * 'self_type)) ->
      'a var_map -> 'a var_map * Types.datatype var_map * 'self_type
    method var : var -> (var * Types.datatype * 'self_type)
    (* method closure_var : var -> (var * Types.datatype * 'self_type) *)
    method value : value -> (value * Types.datatype * 'self_type)

    method tail_computation :
      tail_computation -> (tail_computation * Types.datatype * 'self_type)
    method special : special -> (special * Types.datatype * 'self_type)
    method bindings : binding list -> (binding list * 'self_type)
    method computation : computation -> (computation * Types.datatype * 'self_type)
    method binding : binding -> (binding * 'self_type)
    method binder : binder -> (binder * 'self_type)
    (* method closure_binder : binder -> (binder * 'self_type) *)

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

type eval_fun_def = var_info * (var list * computation) * Var.var option * location
  deriving (Show)
