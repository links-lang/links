(* JavaScript code generation *)
open Utility

(** IR variable environment *)
module VEnv = Env.Int

(** Type of environments mapping IR variables to object variables *)
type venv = string VEnv.t

(** Intermediate language *)
module Code: sig
  module Var: sig
    type t = string
    [@@deriving show]
  end

  module Label: sig
    type t = string
    [@@deriving show]
  end

  type t = Var    of Var.t
         | Lit    of string
         | Fn     of Var.t list * t

         | LetFun of (Var.t * Var.t list * t * Ir.location) * t
         | LetRec of (Var.t * Var.t list * t * Ir.location) list * t
         | Call   of t * t list
         | Unop   of Var.t * t
         | Binop  of t * Var.t * t
         | If     of t * t * t
         | Case   of Var.t * (Var.t * t) stringmap * (Var.t * t) option
         | Dict   of (Label.t * t) list
         | Arr    of t list

         | Bind   of Var.t * t * t
         | Return of t

         | Die    of string
         | Nothing
         [@@deriving show]

  module MetaContinuation: sig
    type nonrec t = (t -> t)
    val identity : t
  end
end

module type JS_PAGE_COMPILER = sig
  val generate_program : venv -> Ir.computation -> venv * Code.t
  val generate_stubs : Value.env -> Ir.binding list -> Code.MetaContinuation.t
  val generate_toplevel_bindings : Value.env -> Json.json_state -> venv -> Ir.binding list -> Json.json_state * venv * string list * Code.MetaContinuation.t
  val wrap_with_server_lib_stubs : Code.t -> Code.t
  val primitive_bindings : string
end

module Compiler : JS_PAGE_COMPILER

module type JS_CODEGEN = sig
  val string_of_js : Code.t -> string
  val output : out_channel -> Code.t -> unit
end
module Js_CodeGen : JS_CODEGEN
