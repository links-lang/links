(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Value
open Ident
open Lambda

val ns_table: Ns.table ref

val register_op: string -> (t list -> t) -> unit

val get_globals: (Compunit.t -> t array) ref
val get_external: (Compunit.t -> int -> t) ref
val set_external: (Compunit.t -> int -> t -> unit) ref
val get_builtin: (string -> t) ref

val expr: Lambda.expr -> int -> t

val eval_toplevel: code_item list -> unit
val eval_var: var_loc -> t

val eval_unit: Value.t array -> code_item list -> unit


val eval_apply: Value.t -> Value.t -> Value.t
