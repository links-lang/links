(*pp deriving *)
(** IR variables *)

open Utility

type scope = [ `Local | `Global ]
  deriving (Show, Pickle)
(* term variables *)
type var = int
  deriving (Show, Pickle)
type var_info = Types.datatype * string * scope
  deriving (Show, Pickle)
type binder = var * var_info
  deriving (Show, Pickle)

(* Generation of fresh variables *)
let variable_counter = ref 0

let fresh_raw_var : unit -> var =
  fun () ->
    incr variable_counter;
    !variable_counter

let fresh_binder : var_info -> binder =
  fun info ->
    let var = fresh_raw_var () in
      (var, info)

let fresh_var : var_info -> binder * var =
  fun info ->
    let (var, info) = fresh_binder info in
      (var, info), var

let info_type (t, _, _) = t
let info_of_type t = (t, "", `Local)

let make_local_info (t, name) = (t, name, `Local)
let make_global_info (t, name) = (t, name, `Global)

let fresh_binder_of_type = info_of_type ->- fresh_binder
let fresh_var_of_type = info_of_type ->- fresh_var

let var_of_binder (var, _) = var
let type_of_binder (var, (t, _, _)) = t
let name_of_binder (_, (_, name, _)) = name
let scope_of_binder (_, (_, _, scope)) = scope

(* create a copy of a type environment mapping vars (= ints) to types
   instead of strings to types
*)
let varify_env (nenv, tenv) : Types.datatype Env.Int.t =
  Env.String.fold
    (fun name t tenv ->
       Env.Int.bind tenv (Env.String.lookup nenv name, t))
    tenv
    Env.Int.empty
