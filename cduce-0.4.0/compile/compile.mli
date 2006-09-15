(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

(* From Ast to Lambda. *)

open Ident 
open Lambda

type env
val global_size: env -> int

val empty : Compunit.t -> env
val empty_toplevel : env

val find : id -> env -> var_loc
val find_slot : id -> env -> int

val compile_eval_expr : env -> Typed.texpr -> Value.t

val comp_unit:
  ?run:bool ->
  ?show:(id option -> Types.t -> Value.t option -> unit) ->
  ?directive:(Typer.t -> env -> Ast.toplevel_directive -> unit) ->

  Typer.t -> env -> Ast.pmodule_item list -> 
  Typer.t * env * Lambda.code_item list


val from_comp_unit: (Compunit.t -> env) ref
  (* Defined in Librarian *)
