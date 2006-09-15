(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Encodings
open Schema_pcre

exception Error of string
let error s = raise (Error s)

let xsd_namespace = Utf8.mk "http://www.w3.org/2001/XMLSchema"
let xsi_namespace = Utf8.mk "http://www.w3.org/2001/XMLSchema-instance"
let xsd_prefix = Utf8.mk "xsd"
let xsi_prefix = Utf8.mk "xsi"
let xsd = Ns.Uri.mk xsd_namespace
let xsi = Ns.Uri.mk xsi_namespace

let xml_parser = ref (fun uri _ _ -> failwith "No active XML parser")

(* A simple tree model sufficient to parse schemas *)

type node = {
  tag: string;
  attrs: (Ns.Label.t * Utf8.t) list;
  mutable children: node list;
  ns: Ns.table;
  father: node
}

let rec root={tag="***";
	      attrs=[];children=[];ns=Ns.empty_table;father=root}
let current = ref root

let start_element_handler name att =
  let (table,(ns,local),attrs) = Ns.process_start_tag !current.ns name att in
  let tag = 
    if Ns.Uri.equal ns xsd then "xsd:"^(Utf8.get_str local) else "***" in
  current := { tag=tag; attrs=attrs; children=[]; ns=table; father= !current }

let end_element_handler () =
  (!current).children <- List.rev (!current).children;
  let f = (!current).father in
  f.children <- !current :: f.children;
  current := f

let node_of_uri uri = 
  current := root;
  root.children <- [];
  !xml_parser uri start_element_handler end_element_handler;
  let c = root.children in
  root.children <- [];
  match c with
    | [r] -> r
    | _ -> assert false

let _may_attr name n =
  let name = Ns.Label.mk (Ns.empty, Utf8.mk name) in
  let rec aux = function
    | [] -> None
    | (nm,v)::_ when Ns.Label.equal nm name -> Some v
    | _::r -> aux r
  in
  aux n.attrs

let _is_attr name n v =
  match _may_attr name n with
    | None -> false
    | Some v' -> Utf8.get_str v' = v
    
let _attr name n =
  match _may_attr name n with
    | Some v -> v
    | None -> error ("Attribute " ^ name ^ " is missing")
	
let _tag n = n.tag

let _elems name n = List.filter (fun n -> n.tag = name) n.children

let _fold_elems n x f = 
  List.fold_left (fun x n -> f x n n.tag) x n.children

let _filter_elems names n =
  List.filter (fun n -> List.mem n.tag names) n.children

let _may_elem name n =
  try Some (List.find (fun n -> n.tag = name) n.children)
  with Not_found -> None

let _iter_elems n f =
  List.iter (fun n -> f n n.tag) n.children

let _resolve_qname n qname = Ns.map_tag n.ns qname

let _may_qname_attr name n =
  match _may_attr name n with
    | Some qname -> Some (_resolve_qname n qname)
    | None -> None
	
let _qname_attr name n =
  match _may_attr name n with
    | Some qname -> _resolve_qname n qname
    | None -> error ("Cannot find qname attribute " ^ name)

