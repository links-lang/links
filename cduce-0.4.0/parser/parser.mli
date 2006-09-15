(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

exception Error of string

val expr : char Stream.t -> Ast.pexpr
val pat : char Stream.t -> Ast.ppat
val prog : char Stream.t -> Ast.pmodule_item list
val top_phrases : char Stream.t -> Ast.pmodule_item list

val sync : unit -> unit
val localize_exn: (unit -> 'a) -> 'a

(* Hooks to extend the syntax *)

module Hook: sig
  val expr: Ast.pexpr Grammar.Entry.e
  val pat: Ast.ppat Grammar.Entry.e
  val keyword: string Grammar.Entry.e
end

(* Helpers *)
(* TODO: put this in Ast *)

val logical_and: Ast.pexpr -> Ast.pexpr -> Ast.pexpr
val logical_or: Ast.pexpr -> Ast.pexpr -> Ast.pexpr
val logical_not: Ast.pexpr -> Ast.pexpr

val if_then_else: Ast.pexpr -> Ast.pexpr -> Ast.pexpr -> Ast.pexpr
