(** Monadic IR *)

[@@@ocaml.warning "-39"] (** disables warnings about unused rec flags **)

type scope = Var.scope
  [@@deriving show]
(* term variables *)
type var = Var.var
  [@@deriving show,eq,yojson]
type var_info = Var.var_info
  [@@deriving show]
type binder = Var.binder
  [@@deriving show]

(* type variables *)
type tyvar = Types.quantifier
  [@@deriving show]
type tyarg = Types.type_arg
  [@@deriving show]

type name = string
  [@@deriving show]

type name_set = Utility.stringset
  [@@deriving show]
type 'a name_map = 'a Utility.stringmap
  [@@deriving show]

type 'a var_map = 'a Utility.intmap
  [@@deriving show]

type language = string
  [@@deriving show]

type constant = Constant.constant
  [@@deriving show]

type location = Sugartypes.location
  [@@deriving show]

type value =
  [ `Constant of constant
  | `Variable of var
  | `Extend of (value name_map * value option)
  | `Project of (name * value)
  | `Erase of (name_set * value)
  | `Inject of (name * value * Types.datatype)

  | `TAbs of tyvar list * value
  | `TApp of value * tyarg list

  | `XmlNode of (name * value name_map * value list)
  | `ApplyPure of (value * value list)

  | `Closure of var * value

  | `Coerce of (value * Types.datatype)
  ]
and tail_computation =
  [ `Return of (value)
  | `Apply of (value * value list)
  (* | `ApplyClosure of (value * value list) *)

  | `Special of special

  | `Case of (value * (binder * computation) name_map * (binder * computation) option)
  | `If of (value * computation * computation)
  ]
and fun_def = binder * (tyvar list * binder list * computation) * binder option * location
and binding =
  [ `Let of binder * (tyvar list * tail_computation)
  | `Fun of fun_def
  | `Rec of fun_def list
  | `Alien of (binder * name * language)
  | `Module of (string * binding list option) ]
and special =
  [ `Wrong of Types.datatype
  | `Database of value
  | `Table of (value * value * value * (Types.datatype * Types.datatype * Types.datatype))
  | `Query of (value * value) option * computation * Types.datatype
  | `Update of (binder * value) * computation option * computation
  | `Delete of (binder * value) * computation option
  | `CallCC of (value)
  | `Select of (name * value)
  | `Choice of (value * (binder * computation) name_map)
  | `Handle of handler
  | `DoOperation of (name * value list * Types.datatype) ]
and computation = binding list * tail_computation
and effect_case = binder * binder * computation
and handler = {
    ih_comp: computation;
    ih_cases: effect_case name_map;
    ih_return: binder * computation;
    ih_depth: handler_depth;
}
and handler_depth = [`Deep of (binder * value) list | `Shallow]
  [@@deriving show]

let binding_scope : binding -> scope =
  function
  | `Let (b, _)
  | `Fun (b, _, _, _)
  | `Rec ((b, _, _, _)::_)
  | `Alien (b, _, _) -> Var.scope_of_binder b
  | `Rec []
  | `Module _ -> assert false

let binder_of_fun_def (fb, _, _, _) = fb

let tapp (v, tyargs) =
  match tyargs with
    | [] -> v
    | _ -> `TApp (v, tyargs)

let letm (b, tc) = `Let (b, ([], tc))
let letmv (b, v) = letm (b, `Return v)
(*let letv (b, v) = `Let (b, `Return v)*)

let rec is_atom =
  function
    | `Constant (`Bool _)
    | `Constant (`Int _)
    | `Constant (`Char _)
    | `Constant (`Float _)
    | `Variable _ -> true
(*
  This can only be an atom if
  Erase is just an upcast, and our language
  is properly parametric.
*)
(*    | `Erase (_, v) *)
    | `Coerce (v, _) -> is_atom v
    | _ -> false

let with_bindings bs' (bs, tc) = (bs' @ bs, tc)

type program = computation
  [@@deriving show]

let string_of_var = string_of_int

let string_of_value _ = "[VALUE]"
let string_of_tail_computation _ = "[TAIL_COMPUTATION]"
let string_of_binding _ = "[BINDING]"
let string_of_special _ = "[SPECIAL]"
let string_of_computation _ = "[COMPUTATION]"
let string_of_program _ = "[PROGRAM]"

type eval_fun_def = var_info * (var list * computation) * Var.var option * location
  [@@deriving show]
