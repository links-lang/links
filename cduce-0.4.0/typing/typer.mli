(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Ident

type t

exception NonExhaustive of Types.descr
exception Constraint of Types.descr * Types.descr
exception ShouldHave of Types.descr * string
exception WrongLabel of Types.descr * label
exception UnboundId of id * bool
exception UnboundExtId of Compunit.t * id
exception ShouldHave2 of Types.descr * string * Types.descr
exception Error of string
exception Warning of string * Types.t

val empty_env: t

val register_types : string -> t -> unit
  (* Register types of the environment for the pretty-printer *)


val find_value: id -> t -> Types.t
val enter_type: id -> Types.t -> t -> t
val iter_values: t -> (id -> Types.t -> unit) -> unit

val typ: t -> Ast.ppat -> Types.Node.t
val pat: t -> Ast.ppat -> Patterns.node

val dump_types: Format.formatter -> t -> unit
val dump_ns: Format.formatter -> t -> unit
val set_ns_table_for_printer: t -> unit


val type_using: t -> Location.loc -> U.t -> U.t -> t
val type_schema: t -> Location.loc -> U.t -> string -> t
val type_ns : t -> Location.loc -> U.t -> Ast.ns_expr -> t
val type_open: t -> Location.loc -> U.t list -> t

val type_keep_ns : t -> bool -> t

val type_expr: t -> Ast.pexpr -> Typed.texpr * Types.descr

val type_defs: t -> (Location.loc * U.t * Ast.ppat) list -> t

val type_let_decl: t -> Ast.ppat -> Ast.pexpr -> 
  t * Typed.let_decl * (id * Types.t) list

val type_let_funs: t -> Ast.pexpr list -> 
  t * Typed.texpr list * (id * Types.t) list
  (* Assume that all the expressions are Abstractions *)



(* Operators *)

type type_fun = Types.t -> bool -> Types.t

val register_op: string -> int -> (type_fun list -> type_fun) -> unit
val flatten: type_fun -> type_fun

(* Forward definitions *)
val from_comp_unit: (Compunit.t -> t) ref
  (* From Librarian *)
val load_comp_unit: (U.t -> Compunit.t) ref
  (* From Librarian *)

val has_ocaml_unit: (U.t -> bool) ref
val has_static_external: (string -> bool) ref


val load_schema: 
  (string -> string -> Ns.Uri.t * (Types.t * Schema_validator.t) Ident.Env.t) ref
