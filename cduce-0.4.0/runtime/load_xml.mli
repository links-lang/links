(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

val load_xml: ?ns:bool -> string -> Value.t
val load_html: string -> Value.t


(* To define and register a parser *)

val xml_parser: (string -> unit) ref

val start_element_handler : string -> (string * string) list -> unit
val end_element_handler : 'a -> unit
val text_handler : string -> unit

