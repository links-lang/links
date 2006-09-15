(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Printf

open Encodings
open Schema_pcre
open Schema_types

let xsd = Schema_xml.xsd

let no_facets = {
  length = None;
  minLength = None;
  maxLength = None;
  enumeration = None;
  whiteSpace = `Collapse, true;
  maxInclusive = None;
  maxExclusive = None;
  minInclusive = None;
  minExclusive = None;
}

(** naive implementation: doesn't follow XML Schema constraints on facets
 * merging. Here all new facets override old ones *)
let merge_facets old_facets new_facets =
  let maxInclusive, maxExclusive =
    match new_facets.maxInclusive, new_facets.maxExclusive with
    | None, None -> old_facets.maxInclusive, old_facets.maxExclusive
    | Some _, Some _ -> assert false
    | v -> v
  in
  let minInclusive, minExclusive =
    match new_facets.minInclusive, new_facets.minExclusive with
    | None, None -> old_facets.minInclusive, old_facets.minExclusive
    | Some _, Some _ -> assert false
    | v -> v
  in
  {   length =
        (match new_facets.length with
        | None -> old_facets.length
        | v -> v);
      minLength =
        (match new_facets.minLength with
        | None -> old_facets.minLength
        | v -> v);
      maxLength =
        (match new_facets.maxLength with
        | None -> old_facets.maxLength
        | v -> v);
      enumeration =
        (match new_facets.enumeration with
        | None -> old_facets.enumeration
        | v -> v);
      whiteSpace = new_facets.whiteSpace;
      maxInclusive = maxInclusive;
      maxExclusive = maxExclusive;
      minInclusive = minInclusive;
      minExclusive = minExclusive;
  }

let name_of_simple_type_definition = function
  | { st_name = Some name } -> name
  | _ -> raise (Invalid_argument "anonymous simple type definition")
let name_of_complex_type_definition = function
  | { ct_name = Some name } -> name
  | _ -> raise (Invalid_argument "anonymous complex type definition")
let name_of_type_definition = function
  | AnyType -> (xsd, Utf8.mk "anyType")
  | Simple st -> name_of_simple_type_definition st
  | Complex ct -> name_of_complex_type_definition ct

let simple_type_of_type = function
  | Simple s -> s
  | _ -> raise (Invalid_argument "simple_type_of_type")
let complex_type_of_type = function
  | Complex c -> c
  | _ -> raise (Invalid_argument "complex_type_of_type")
let content_type_of_type = function
  | AnyType -> assert false
  | Complex { ct_content = ct } -> ct
  | Simple st -> CT_simple st

exception XSD_validation_error of string
exception XSI_validation_error of string

let rec normalize_white_space =
  let ws_RE = pcre_regexp "[\t\r\n]" in
  let spaces_RE = pcre_regexp "[ ]+" in
  let margins_RE = pcre_regexp "^ (.*) $" in
  fun handling s ->
  match handling with
  | `Preserve -> s
  | `Replace -> pcre_replace ~rex:ws_RE ~templ:(Utf8.mk " ") s
  | `Collapse ->
      let s' =
        pcre_replace ~rex:spaces_RE ~templ:(Utf8.mk " ")
          (normalize_white_space `Replace s)
      in
      pcre_replace ~rex:margins_RE ~templ:(Utf8.mk "$1") s'


let first_of_wildcard_constraint = function
  | WAny -> Atoms.any
  | WNot ns -> Atoms.diff Atoms.any (Atoms.any_in_ns ns)
  | WOne l -> 
      List.fold_left (fun acc ns -> Atoms.cup acc (Atoms.any_in_ns ns))
	Atoms.empty l
let first_of_model_group = function
  | All particles | Choice particles ->
      List.fold_left (fun acc p -> Atoms.cup acc p.part_first)
	Atoms.empty particles
  | Sequence particles ->
      let rec aux = function
        | hd::tl when hd.part_nullable -> Atoms.cup hd.part_first (aux tl)
        | hd::tl -> hd.part_first
        | [] -> Atoms.empty
      in
      aux particles

let nullable_of_model_group = function
  | All particles | Sequence particles -> 
      List.for_all (fun p -> p.part_nullable) particles
  | Choice particles -> List.exists (fun p -> p.part_nullable) particles

type to_be_visited =
  | Fully of Value.t  (* xml values still to be visited *)
  | Half of Value.t   (* xml values half visited (i.e. E_start_tag generated) *)
  | Other of Encodings.Utf8.t (* other values *)
  | Backlog of event  (* old events not yet delivered *)

let stream_of_value v =
  let stack = ref [Fully v] in
  let f _ = (* lazy visit of a tree of CDuce XML values, stack keeps track of
            what has still to be visited *)
    match !stack with
    | (Fully ((Value.Xml (Value.Atom atom, attrs, _)) as v)) :: tl ->
        stack := (Half v) :: tl;
        let children = ref [] in  (* TODO inefficient *)
        let push v s = (s := v :: !s) in
        Value.iter_xml
          (fun pcdata -> push (Other pcdata) children)
          (fun v ->
            match v with
            | (Value.Xml (_, _, _)) as v -> push (Fully v) children
            | v -> raise (Invalid_argument "Schema_events.stream_of_value"))
          v;
        stack := (List.rev !children) @ !stack;
        List.iter (* push attributes as events on the stack *)
          (fun (qname, v) ->
            push (Backlog (E_attribute (qname, fst (Value.get_string_utf8 v))))
              stack)
          (Value.get_fields attrs);
        Some (E_start_tag atom)
    | Half _ :: tl ->
        stack := tl;
        Some E_end_tag
    | (Backlog ev) :: tl -> (* consume backlog *)
        stack := tl;
        Some ev
    | (Other v) :: tl ->
        stack := tl;
        Some (E_char_data v)
    | [] -> None
    | _ -> Value.failwith'  "Validate: non XML element"
  in
  Stream.from f

let string_of_event = function
  | E_start_tag t -> sprintf "<%s>" (Atoms.V.to_string t)
  | E_end_tag -> sprintf "</>"
  | E_attribute (qname, value) ->
      sprintf "@%s=%s" (Ns.Label.string_of_attr qname) (Utf8.to_string value)
  | E_char_data value -> Utf8.to_string value


let simple_restrict name base new_facets =
  { st_name = name;
    st_variety = base.st_variety;
    st_facets = merge_facets base.st_facets new_facets;
    st_base = Some base }

let simple_list name item =
  { st_name = name;
    st_variety = List item;
    st_facets = no_facets;
    st_base = None }

let simple_union name members =
  { st_name = name;
    st_variety = Union members;
    st_facets = no_facets;
    st_base = None }


let xsi_nil_atom = Atoms.V.mk (Schema_xml.xsi, Utf8.mk "nil")
let xsi_nil_type = Types.atom (Atoms.atom xsi_nil_atom)
let xsi_nil_label = Ns.Label.mk (Schema_xml.xsi, Utf8.mk "nil")


let merge_attribute_uses l =
  List.fold_left (fun (l,a) (l',a') -> (l @ l', a || a')) ([],false) l
