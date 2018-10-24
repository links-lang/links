(* JavaScript code generation *)
open Utility

(** IR variable environment *)
module VEnv = Env.Int

(** Type of environments mapping IR variables to source variables *)
type venv = string VEnv.t

(** Intermediate language *)
(* type code = | Var    of string
 *             | Lit    of string
 *             | Fn     of (string list * code)
 * 
 *             | LetFun of ((string * string list * code * Ir.location) * code)
 *             | LetRec of ((string * string list * code * Ir.location) list * code)
 *             | Call   of (code * code list)
 *             | Unop   of (string * code)
 *             | Binop  of (code * string * code)
 *             | If     of (code * code * code)
 *             | Case   of (string * (string * code) stringmap * (string * code) option)
 *             | Dict   of ((string * code) list)
 *             | Arr    of (code list)
 * 
 *             | Bind   of (string * code * code)
 *             | Return of code
 * 
 *             | Die    of (string)
 *             | Nothing *)
  (* [@@deriving show] *)

module type JS_COMPILER = sig
  type source = [
    | `Bindings of Ir.binding list
    | `Program of Ir.program ]

  val compile : source -> Value.env -> Js.program
end

module type JS_PAGE_COMPILER = sig
  include JS_COMPILER

  val generate_program : venv -> Ir.computation -> venv * Js.js
  val generate_stubs : Value.env -> Ir.bindings -> Js.js
  val generate_toplevel_bindings : Value.env -> Json.json_state -> venv -> Ir.binding list -> Json.json_state * venv * string list * Js.js
  val wrap_with_server_lib_stubs : Js.js -> Js.js (* TODO remove *)
  val primitive_bindings : string (* TODO remove *)
end

module Compiler : JS_PAGE_COMPILER
