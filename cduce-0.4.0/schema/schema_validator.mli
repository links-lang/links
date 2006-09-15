(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Schema_types


val validate_type : type_definition -> Value.t -> Value.t
val validate_element : element_declaration -> Value.t -> Value.t
val validate_attribute_group : attribute_group_definition -> Value.t -> Value.t
val validate_model_group : model_group_definition -> Value.t -> Value.t
val validate_simple_type : simple_type_definition -> Encodings.Utf8.t -> Value.t


type t =
  | VAttrGp of attribute_group_definition
  | VModelGp of model_group_definition
  | VType of type_definition
  | VElem of element_declaration

val run: t -> Value.t -> Value.t

