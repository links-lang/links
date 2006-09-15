(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

exception Error of string
open Ident

(* Pattern algebra *)

type descr
type node
module Node: Custom.T with type t = node

val make: fv -> node
val define: node -> descr -> unit

val constr : Types.t -> descr
val cup    : descr -> descr -> descr
val cap    : descr -> descr -> descr

val times  : node -> node -> descr 
val xml    : node -> node -> descr
val record : label -> node -> descr

val capture : id -> descr
val constant: id -> Types.const -> descr

val id: node -> int
val descr: node -> descr
val fv : node -> fv

(* Pretty-printing *)

module Print : sig
  val print: Format.formatter -> descr -> unit
end

(* Pattern matching: static semantics *)

val accept : node -> Types.Node.t
val filter : Types.t -> node -> Types.Node.t id_map


(* Pattern matching: compilation *)

module Compile: sig
  open Auto_pat
  val make_branches : Types.t -> (node * 'a) list -> state * 'a rhs array
  val make_checker : Types.t -> Types.t -> state
end


