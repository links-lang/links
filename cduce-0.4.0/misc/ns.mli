(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Encodings


module Uri : Upool.S with type value = Utf8.t
  (* Namespaces URIs *)

module QName : sig
  include Custom.T with type t = Uri.t * Utf8.t

  val to_string: t -> string
  val print: Format.formatter -> t -> unit
(*
  val mk_ascii: string -> t
  val get_ascii: t -> string
*)
end

module Label : sig
  (* Qualified names of the form (nsuri:localname). Used
     as labels in records *)
  include Upool.S with type value = QName.t
  val print_tag: Format.formatter -> t -> unit
  val print_attr: Format.formatter -> t -> unit

  val print_quote: Format.formatter -> t -> unit
  val mk_ascii: string -> t
  val get_ascii: t -> string

  val string_of_attr: t -> string
  val string_of_tag: t -> string

(*
  val to_string: t -> string
  val print: Format.formatter -> t -> unit
  val of_qname: Uri.t -> Utf8.t -> t
  val split: t -> Utf8.t * Utf8.t
*)
end

exception UnknownPrefix of Utf8.t

val empty : Uri.t
val xml_ns: Uri.t

type table  (* prefix => namespace *)
val empty_table: table  (* Contains only xml *)
val def_table: table (* Contains xml,xsd,xsi *)
val add_prefix: Utf8.t -> Uri.t -> table -> table
val merge_tables: table -> table -> table
val dump_table: Format.formatter -> table -> unit

val get_table: table -> (Utf8.t * Uri.t) list
val mk_table: (Utf8.t * Uri.t) list -> table

val process_start_tag:
  table -> string -> (string * string) list -> 
    table * QName.t * (Label.t * Utf8.t) list

val map_tag: table -> Utf8.t -> QName.t
val map_attr: table -> Utf8.t -> QName.t
val map_prefix: table -> Utf8.t -> Uri.t

(* Support for printing XML documents *)

module Printer : sig
  type printer

  val printer: table -> printer
  val register_ns: printer -> Uri.t -> unit
  val register_qname: printer -> QName.t -> unit

  val prefixes: printer -> (Utf8.t * Uri.t) list
  val tag: printer -> QName.t -> string
  val attr: printer -> QName.t -> string
end
    

(***)

module InternalPrinter : sig
  val set_table: table -> unit

  val any_ns: Uri.t -> string
  val tag: QName.t -> string
  val attr: QName.t -> string

  val print_tag: Format.formatter -> QName.t -> unit
  val print_any_ns: Format.formatter -> Uri.t -> unit

  val dump: Format.formatter -> unit
end


