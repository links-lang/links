(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

(** Schema common functionalities depending only on Schema_types *)

open Encodings
open Schema_types

exception XSD_validation_error of string
exception XSI_validation_error of string

val name_of_type_definition : type_definition -> Ns.QName.t
val content_type_of_type : type_definition -> content_type

val first_of_model_group: model_group -> Atoms.t
val first_of_wildcard_constraint: wildcard_constraint -> Atoms.t
val nullable_of_model_group: model_group -> bool

(** empty set of facets (with the only exception of "whiteSpace", which is set
    to <`Collapse, true>, the mandatory value for all non string derived simple
    types) *)
val no_facets: facets

(** perform white space normalization according to XML recommendation *)
val normalize_white_space: white_space_handling -> Utf8.t -> Utf8.t

(** {2 event interface on top of CDuce values} *)

val stream_of_value: Value.t -> event Stream.t
val string_of_event: event -> string


val simple_restrict: 
  Ns.QName.t option -> simple_type_definition -> facets -> simple_type_definition
val simple_list: 
  Ns.QName.t option -> simple_type_definition -> simple_type_definition
val simple_union: 
  Ns.QName.t option -> simple_type_definition list -> simple_type_definition


val xsi_nil_type: Types.t
val xsi_nil_atom: Atoms.V.t
val xsi_nil_label: Ident.label


val merge_attribute_uses: attribute_uses list -> attribute_uses
