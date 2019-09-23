(* JavaScript code generation *)
open Utility

(** IR variable environment *)
module VEnv = Env.Int

(** Type of environments mapping IR variables to source variables *)
type venv = string VEnv.t

(** Intermediate language *)
type code = | Var    of string
            | Lit    of string
            | Fn     of (string list * code)

            | LetFun of ((string * string list * code * Ir.location) * code)
            | LetRec of ((string * string list * code * Ir.location) list * code)
            | Call   of (code * code list)
            | Unop   of (string * code)
            | Binop  of (code * string * code)
            | If     of (code * code * code)
            | Case   of (string * (string * code) stringmap * (string * code) option)
            | Dict   of ((string * code) list)
            | Arr    of (code list)

            | Bind   of (string * code * code)
            | Return of code

            | Die    of (string)
            | Nothing
  [@@deriving show]

module type JS_PAGE_COMPILER = sig
  val generate_program : venv -> Ir.computation -> venv * code
  val generate_stubs : Value.env -> Ir.binding list -> code -> code
  val generate_toplevel_bindings : Value.env -> Json.json_state -> venv -> Ir.binding list -> Json.json_state * venv * string list * (code -> code)
  val wrap_with_server_lib_stubs : code -> code
  val primitive_bindings : string
end

module Compiler : JS_PAGE_COMPILER

module type JS_CODEGEN = sig
  val string_of_js : code -> string
end
module Js_CodeGen : JS_CODEGEN
