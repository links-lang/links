(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Caml_cduce
open Asttypes
open Types

exception Error of string

type t = { uid : int; mutable recurs : int; mutable def : def }
and def =
  | Link of t
  | Arrow of string * t * t
  | Tuple of t list
  | PVariant of (string * t option) list  (* Polymorphic variant *)
  | Variant of string * (string * t list) list * bool
  | Record of string * (string * t) list * bool
  | Builtin of string * t list
  | Abstract of string
  | Var of int


val reg_uid: t -> unit

(* Load an external .cmi *)
val has_cmi: string -> bool
val load_module: string -> (string * t) list

(* Load the .cmi corresponding to a CDuce compilation unit *)
val read_cmi: string -> string * (string * Types.type_expr * t) list

val print : Format.formatter -> t -> unit
val print_ocaml : Format.formatter -> Types.type_expr -> unit


val find_value: string -> t * int

