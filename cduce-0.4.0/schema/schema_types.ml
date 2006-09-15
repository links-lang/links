(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

(** 
  Internal representation of an XML Schema.

  Note: this module comes in .mli part only, hence no value and/or
  exceptions are available here. See Schema_common.
*)

open Encodings

type derivation_type = [ `Extension | `Restriction ]
type white_space_handling = [ `Preserve | `Replace | `Collapse ]

type facets = {
  length: (int * bool) option;      (* length, fixed *)
  minLength: (int * bool) option;   (* length, fixed *)
  maxLength: (int * bool) option;   (* length, fixed *)
(*   pattern: Schema_regexp.regexp list;           (* list of ANDed patterns *) *)
  enumeration: value_ref list option;
  whiteSpace: white_space_handling * bool;           (* handling, fixed *)
  maxInclusive: (value_ref * bool) option;        (* max, fixed *)
  maxExclusive: (value_ref * bool) option;        (* max, fixed *)
  minInclusive: (value_ref * bool) option;        (* min, fixed *)
  minExclusive: (value_ref * bool) option;        (* min, fixed *)
(*
  totalDigits: (xs_positiveInteger * bool) option;     (* digits, fixed *)
  fractionDigits: (xs_nonNegativeInteger * bool) option;  (* digits, fixed *)
*)
}

and value_ref = Value.t
and value_constraint = 
    [ `Fixed of Utf8.t * Value.t | `Default of Utf8.t * Value.t ]

and type_ref = type_definition Lazy.t

and simple_type_definition = {
  st_name: Ns.QName.t option;
  st_variety: variety;
  st_facets: facets;
  st_base: simple_type_definition option;
}

and variety =
  | Atomic of simple_type_definition
  | List of simple_type_definition
  | Union of simple_type_definition list

and attribute_declaration =
    { attr_name : Ns.Label.t;
      attr_typdef : simple_type_definition;
      attr_cstr : value_constraint option }

and attribute_use =
    { attr_required : bool;
      attr_decl : attribute_declaration;
      attr_use_cstr : value_constraint option }

and term =
  | Elt of element_declaration
  | Model of model_group
  | Wildcard of wildcard

and model_group =
  | All of particle list
  | Choice of particle list
  | Sequence of particle list

and content_type =
  | CT_empty
  | CT_simple of simple_type_definition
  | CT_model of particle * bool        (* mixed *)

and particle =
    { part_min: int;
      part_max: int option;  (* None = unbounded *)
      part_term: term;
      part_first: Atoms.t;
      part_nullable: bool }

and element_declaration =
    { elt_name: Atoms.V.t;
      elt_typdef: type_ref;
      elt_cstr: value_constraint option;
      elt_nillable: bool;
    }

and complex_type_definition =
    { ct_uid: int;
      ct_name: Ns.QName.t option;
      ct_typdef: type_definition;
      ct_deriv: derivation_type;
      ct_attrs: attribute_uses;
      ct_content: content_type;
    }

and attribute_uses = attribute_use list * bool  
  (* true = allow other attribs *)

and type_definition =
  | AnyType
  | Simple of simple_type_definition
  | Complex of complex_type_definition

and wildcard_constraint =
  | WAny
  | WNot of Ns.Uri.t
  | WOne of Ns.Uri.t list

and wildcard = {
  wild_cstr: wildcard_constraint;
  wild_process: [`Lax | `Skip | `Strict];
  wild_first: Atoms.t;
}

type model_group_definition =
    { mg_name : Ns.QName.t;
      mg_def : model_group }

type attribute_group_definition =
    { ag_name : Ns.QName.t;
      ag_def : attribute_uses }

type schema = {
  targetNamespace: Ns.Uri.t;
  types: type_definition list;
  attributes: attribute_declaration list;
  elements: element_declaration list;
  attribute_groups: attribute_group_definition list;
  model_groups: model_group_definition list;
}

type event =
  | E_start_tag of Atoms.V.t
  | E_end_tag
  | E_attribute of Ns.Label.t * Utf8.t
  | E_char_data of Utf8.t
