(*pp deriving *)
(** Monadic IR *)

open Utility

type scope = [ `Local | `Global ]
  deriving (Show)
(* term variables *)
type var = int
  deriving (Show)
type var_info = Types.datatype * string * scope
  deriving (Show)
type binder = var * var_info
  deriving (Show)

(* type variables *)
type tyvar = int
  deriving (Show)
type tyname = string
  deriving (Show)
(* type tybinder = tyvar * var_info *)

type name = string
  deriving (Show)
type 'a name_map = 'a Utility.stringmap
  deriving (Show)

type language = string
  deriving (Show)
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
  deriving (Show)

type value =
  [ `Constant of constant
  | `Variable of var
  | `Extend of (value name_map * value option)
  | `Project of (name * value)
  | `Erase of (name * value)
  | `Inject of (name * value)

  | `XmlNode of (name * value name_map * value list)

  | `ApplyPrim of (value * value list)

  (* should really be implemented as constants *)
  | `Comparison of (value * Syntaxutils.comparison * value)

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
  | `Alien of (binder * language * Types.datatype)
  | `Alias of (tyname * tyvar list * Types.datatype) ]
and special =
  [ `App of value * value
  | `Wrong
  | `Database of value
  | `Query of SqlQuery.sqlQuery
  | `Table of (value * value * (Types.datatype * Types.datatype))
  | `CallCC of (value) ]
and computation = binding list * tail_computation
  deriving (Show)  

let rec is_atom =
  function
    | `Constant (Syntax.Boolean _)
    | `Constant (Syntax.Integer _)
    | `Constant (Syntax.Char _)
    | `Constant (Syntax.Float _)
    | `Variable _ -> true
    | `Erase (_, v)
    | `Coerce (v, _)
    | `Abs v -> is_atom v
    | _ -> false

module Inline =
struct
  (*
    A rather ad-hoc mostly-harmless let-inliner
    
    This inlines atoms, projections and injections. It isn't clear
    that it's always a good idea to inline projections and
    injections, but it's unlikely to cost much for small examples
    and it does lead to faster and more readable javascript.
  *)
  
  let rec is_inlineable_value =
    function
      | v when is_atom v -> true
      | `Project (_, v)
      | `Inject (_, v) -> is_inlineable_value v
      | _ -> false

  (* 
     NOTE:
     
     Most of this is just boilerplate. It would be nice if we could
     generate it automatically somehow...
  *)

  let rec value env v : value =
    let iv = value env in
      match v with
        | `Variable var when IntMap.mem var env -> IntMap.find var env
        | `Constant _ | `Variable _ -> v
        | `Extend (vmap, vopt) -> `Extend (StringMap.map iv vmap, opt_map iv vopt)
        | `Project (name, v) -> `Project (name, iv v)
        | `Erase (name, v) -> `Erase (name, iv v)
        | `Inject (name, v) -> `Inject (name, iv v)
        | `XmlNode (name, vmap, vs) -> `XmlNode (name, StringMap.map iv vmap, List.map iv vs)
        | `ApplyPrim (v, vs) -> `ApplyPrim (iv v, List.map iv vs)
        | `Comparison (v, c, w) -> `Comparison (iv v, c, iv w)
        | `Coerce (v, t) -> `Coerce (iv v, t)
        | `Abs v -> `Abs (iv v)

  and tail_computation env tc : tail_computation =
    let iv = value env in
    let ic = computation env in
      match tc with
        | `Return v -> `Return (iv v)
        | `Apply (v, vs) -> `Apply (iv v, List.map iv vs)
        | `Special s -> `Special (special env s)
        | `Case (v, cases, default) ->
            `Case (iv v, StringMap.map (fun (x, c) -> (x, ic c)) cases, opt_map (fun (x, c) -> (x, ic c)) default)
        | `If (v, c1, c2) ->
            `If (iv v, ic c1, ic c2)

  and special env s =
    let iv = value env in
      match s with
        | `App (v, w) -> `App (iv v, iv w)
        | `Wrong -> `Wrong
        | `Database v -> `Database (iv v)
        | `Query q -> `Query q
            (* [WARNING] perhaps we need to look inside the query *)
        | `Table (v, w, t) -> `Table (iv v, iv w, t)
        | `CallCC v -> `CallCC (iv v)
    
  and binding env =
    function
      | `Let (x, tc) -> `Let (x, tail_computation env tc)
      | `Fun (f, xs, c, l) -> `Fun (f, xs, computation env c, l)
      | `Rec (defs) -> `Rec (List.map (fun (f, xs, c, l) -> (f, xs, computation env c, l)) defs)
      | (`Alien _ | `Alias _) as b -> b
(*      | `For (x, v) -> `For (x, value env v) *)

  and bindings env =
    function
      | `Let ((x, (_, _, `Local)), `Return v) :: bs when is_inlineable_value v ->
          bindings (IntMap.add x (value env v) env) bs
      | b :: bs ->
          let env, bs' = bindings env bs in
            env, (binding env b) :: bs'
      | [] ->
          env, []
          
  and computation env (bs, tc)  : computation =
    let env, bs = bindings env bs in
      (bs, tail_computation env tc)

  let program = computation (IntMap.empty)  
end

type program = computation


