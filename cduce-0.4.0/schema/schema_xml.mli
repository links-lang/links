(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Encodings

exception Error of string
type node

val xml_parser: (string -> (string -> (string*string) list -> unit) -> (unit -> unit) -> unit) ref
val node_of_uri: string -> node

val _may_attr: string -> node -> Utf8.t option
val _is_attr: string -> node -> string -> bool
val _attr: string -> node -> Utf8.t

val _resolve_qname: node -> Utf8.t -> Ns.QName.t
val _may_qname_attr: string -> node -> Ns.QName.t option
val _qname_attr: string -> node -> Ns.QName.t

val _tag: node -> string
val _elems: string -> node -> node list
val _fold_elems: node -> 'a -> ('a -> node -> string -> 'a) -> 'a
val _filter_elems: string list -> node -> node list
val _may_elem: string -> node -> node option
val _iter_elems: node -> (node -> string -> unit) -> unit


val xsd: Ns.Uri.t
val xsi: Ns.Uri.t
