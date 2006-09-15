(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Printf

open Encodings
open Schema_pcre
open Schema_common
open Schema_types
open Schema_validator
open Schema_xml

module QTable = Hashtbl.Make(Ns.QName)

let error s = raise (XSD_validation_error s)

let particle min max term first nullable =
  { part_min = min;
    part_max = max;
    part_term = term;
    part_first = first;
    part_nullable = nullable }

let particle_model min max mg =
  particle min max 
    (Model mg)
    (first_of_model_group mg)
    (nullable_of_model_group mg)

let check_force v =
  try Lazy.force v
  with Lazy.Undefined -> error "Cyclic type definition"


let bool_attr attr n =
  (* todo: normalization of whitespace ? *)
  match _may_attr attr n with
    | None -> false
    | Some v -> (match Utf8.get_str v with
	| "true" | "1" -> true
	| "false" | "0" -> false
	| s -> 
	    error ("Invalid boolean value (" ^ s ^ ") for attribute " ^ attr))

let element name type_def constr nillable =
  { elt_name = name;
    elt_typdef = type_def;
    elt_cstr = constr;
    elt_nillable = nillable
  }

let counter = ref 0

let complex name (type_def: type_definition) deriv attrs ct =
  incr counter;
  { ct_uid = !counter;
    ct_name = name;
    ct_typdef = type_def;
    ct_deriv = deriv;
    ct_attrs = attrs;
    ct_content = ct;
  }

let space_RE = pcre_regexp " "
let split s = pcre_split ~rex:space_RE s

(*let unqualify s = snd (Ns.split_qname s)*)

let hashtbl_deref tbl = 
  QTable.fold (fun _ v acc -> (check_force v) :: acc) tbl []
let hashtbl_values tbl = 
  QTable.fold (fun _ v acc -> v :: acc) tbl []

let parse_facets base n =
  let validate_base_type v = 
    Schema_validator.validate_simple_type base v in 
  let parse_nonneg n =
    let s = Utf8.get_str (_attr "value" n) in
    let i = int_of_string s in
    if (i < 0) then error "Unexpected negative integer";
    i
  in
  let aux facets n tag =
    let fixed = bool_attr "fixed" n in
    match tag with
      | "xsd:length" ->
          let length = parse_nonneg n in
          { facets with length = Some (length, fixed) }
      | "xsd:minLength" ->
          let length = parse_nonneg n in
          { facets with minLength = Some (length, fixed) }
      | "xsd:maxLength" ->
          let length = parse_nonneg n in
          { facets with maxLength = Some (length, fixed) }
      | "xsd:enumeration" ->
          let value = validate_base_type (_attr "value" n) in
          let new_enumeration =
	    match facets.enumeration with
	      | None -> Some [ value ]
	      | Some entries -> Some (value :: entries)
          in
          { facets with enumeration = new_enumeration }
      | "xsd:whiteSpace" ->
          let value = Utf8.get_str (_attr "value" n) in
	  let k = match value with
		  | "collapse" -> `Collapse
		  | "preserve" -> `Preserve
		  | "replace" -> `Replace
		  | _ -> error "Unknown value for whiteSpace facet" in
          { facets with whiteSpace = (k,fixed) }
      | "xsd:maxInclusive" ->
          let value = _attr "value" n in
          { facets with maxInclusive = Some (validate_base_type value, fixed) }
      | "xsd:maxExclusive" ->
          let value = _attr "value" n in
          { facets with maxExclusive = Some (validate_base_type value, fixed) }
      | "xsd:minInclusive" ->
          let value = _attr "value" n in
          { facets with minInclusive = Some (validate_base_type value, fixed) }
      | "xsd:minExclusive" ->
          let value = _attr "value" n in
          { facets with minExclusive = Some (validate_base_type value, fixed) }
      | _ ->
	  facets
  in
  _fold_elems n no_facets aux

let default_fixed n f =
  match _may_attr "default" n with
    | Some v -> Some (`Default (v,(f v)))
    | None ->
	match _may_attr "fixed" n with
	  | Some v -> Some (`Fixed (v,(f v)))
	  | None -> None

let parse_att_value_constraint st n =
  default_fixed n (fun v -> validate_simple_type st v)

let parse_min_max n =
  (match _may_attr "minOccurs" n with 
     | Some v -> int_of_string (Utf8.get_str v)
     | None -> 1),
  (match _may_attr "maxOccurs" n with 
     | Some v ->
	 (match Utf8.get_str v with 
	    | "unbounded" -> None 
	    | v -> Some (int_of_string v))
     | None -> Some 1)

let rec first n f = function
  | [] -> None
  | x::l -> match f x n with None -> first n f l | x -> x

let find_particles n =
  _filter_elems ["xsd:element"; "xsd:group"; "xsd:choice"; "xsd:sequence";
		 "xsd:any" ] n

let find_particle n =
  first n _may_elem ["xsd:all"; "xsd:choice"; "xsd:group"; "xsd:sequence"]
    

let register_builtins typs =
  Schema_builtin.iter
    (fun name b ->
       QTable.replace typs name 
	 (lazy (Simple (Schema_builtin.simple_type b))));
  QTable.replace typs (xsd, Utf8.mk "anyType") (lazy AnyType)
    (* Why that ? *)

(* Main parsing function *)
let schema_of_uri uri =

  let typs = QTable.create 17 in
  let elts = QTable.create 17 in
  let attrs= QTable.create 17 in
  let attr_groups = QTable.create 17 in
  let model_groups = QTable.create 17 in
  register_builtins typs;

  let attr_elems = QTable.create 17
  and attr_group_elems = QTable.create 17
  and model_group_elems = QTable.create 17
  and elts_elems = QTable.create 17 in

  let resolve k t1 t2 f qname =
    try QTable.find t1 qname
    with Not_found ->
      let node = 
	try QTable.find t2 qname
	with Not_found ->
	  error ("Can't find declaration for " ^ k ^ " " ^
			      Ns.QName.to_string qname)
      in
      let decl = f node in
      QTable.replace t1 qname decl;
      decl
  in
  let todo = ref [] in

  let imported_ns = ref [] in

  let rec parse_uri uri =
    let root = node_of_uri uri in
    let targetNamespace =  
      match _may_attr "targetNamespace" root with
	| Some ns -> Ns.Uri.mk ns
	| None -> Ns.empty
    in
    let attributeFormDefault = 
      _is_attr "attributeFormDefault" root "qualified" in
    let elementFormDefault = 
      _is_attr "elementFormDefault" root "qualified" in

    let rec parse_root uri root =

  let may_name n =
    match _may_attr "name" n with
      | Some local -> Some (targetNamespace, local)
      | None -> None in
  let get_name n = (targetNamespace, _attr "name" n) in

  let rec resolve_typ qname : Schema_types.type_definition lazy_t =
    try QTable.find typs qname
    with Not_found ->   
      error ("Cannot find type " ^ (Ns.QName.to_string qname))

  and resolve_simple_typ qname =
    match check_force (resolve_typ qname) with
      | Simple st -> st
      | _ -> error "Not a simple type"

  and resolve_elt (qname : Ns.QName.t) =
    resolve "element" elts elts_elems (parse_elt_decl true) qname
      
  and resolve_att qname =
    resolve "attribute" attrs attr_elems (parse_att_decl true) qname

  and resolve_att_group qname =
    resolve "attribute group" attr_groups attr_group_elems parse_att_group 
      qname

  and resolve_model_group qname =
    resolve "model group" model_groups model_group_elems parse_model_group_def
      qname

  (* parse an xsd:simpleType element *)
  and parse_simple_type n =
    let name = may_name n in
    match _may_elem "xsd:restriction" n with
      | Some restriction ->
	  let base = find_base_simple_type restriction in
	  let facets = parse_facets base restriction in
	  simple_restrict name base facets
      | None -> 
    match _may_elem "xsd:list" n with
      | Some list -> simple_list name (find_item_type list)
      | None -> 
    match _may_elem "xsd:union" n with
      | Some union -> simple_union name (find_member_types union)
      | None -> error "Unknown variety for simpleType"

  (* look for a simple type def: try attribute "base", try "simpleType" child,
   * fail *)
  and find_base_simple_type n : Schema_types.simple_type_definition =
    match _may_qname_attr "base" n with
      | Some v -> (resolve_simple_typ v : Schema_types.simple_type_definition)
      | None ->
	  match _may_elem "xsd:simpleType" n with
	    | Some v -> parse_simple_type v
	    | None -> error "no base simple type specified"
		
  (* look for a simple type def: try attribute "itemType", try "simpleType"
   * child, fail *)
  and find_item_type n =
    match _may_qname_attr "itemType" n with
      | Some v -> resolve_simple_typ v
      | None ->
	  match _may_elem "xsd:simpleType" n with
	    | Some v -> parse_simple_type v
	    | None -> error "no itemType specified"

  (* look for a list of simple type defs: try attribute "memberTypes", try
   * "simpleType" children, fail *)
  and find_member_types n =
    let members1 =
      match _may_attr "memberTypes" n with
	| Some v -> 
	    List.map (fun x -> resolve_simple_typ (_resolve_qname n x)) 
	      (split v)
	| None -> []
    in
    let members2 =
      let nodes = _elems "xsd:simpleType" n in
      List.map parse_simple_type nodes
    in
    match members1 @ members2 with
      | [] -> error "no member types specified"
      | members -> members


  and parse_elt_value_constraint (type_def: type_ref)  n =
    let validate_value v = 
      (* BUG: it is ok to have recursive complex_type with
	 default/fixed value *)
      match check_force type_def with
	| Simple st | Complex { ct_content = CT_simple st } ->
	    validate_simple_type st v
	| _ -> 
	    (* BUG: need to check that the value
	       belongs to the type *)
	    Schema_builtin.validate Schema_builtin.string v
    in
    default_fixed n validate_value

  (* look for a simple type def, try "simpleType" child, try "type" attribute,
   * return anySimpleType *)
  and find_simple_type n =
    match _may_elem "xsd:simpleType" n with
      | Some v -> parse_simple_type v
      | None ->
	  match _may_qname_attr "type" n with
	    | Some v -> resolve_simple_typ v
	    | None -> Schema_builtin.simple_type Schema_builtin.any_simple_type

  and parse_att_decl global n =
    let local = _attr "name" n in
    let ns = if global then targetNamespace
    else
      match _may_attr "form" n with
	| Some s when (Utf8.get_str s = "qualified") -> targetNamespace
	| None when attributeFormDefault -> targetNamespace
	| _ -> Ns.empty
    in
    let typdef = find_simple_type n in
    { attr_name = Ns.Label.mk (ns,local);
      attr_typdef = typdef;
      attr_cstr = parse_att_value_constraint typdef n }
      
  and parse_attribute_use n =
    let required = _is_attr "use" n "required" in
    let att_decl =
      match _may_qname_attr "ref" n with
	| Some v -> resolve_att v
	| None ->
	    let a = parse_att_decl false n in
	    { a with attr_cstr = None }  
	      (* forget attribute value constraint *)
    in
    let value_constr = parse_att_value_constraint att_decl.attr_typdef n in
    { attr_required = required;
      attr_decl = att_decl;
      attr_use_cstr = value_constr }
      
  and parse_attribute_uses n =
    let uses1 = (* attribute uses from "attribute" children *)
      (List.map parse_attribute_use (_elems "xsd:attribute" n)),
      (match _may_elem "xsd:anyAttribute" n with Some _ -> true | _ ->false) in

    let uses2 = (* attribute uses from "attributeGroup" children ref *)
      List.map 
	(fun n -> (parse_att_group n).ag_def)
	(_elems "xsd:attributeGroup" n)  in

    merge_attribute_uses (uses1::uses2)

  and parse_attribute_uses_deriv derivation_type base n =
(* TODO: check these rules *)
    let duses = parse_attribute_uses n in
    (* attribute uses from base type *)
    match base, derivation_type with
      | Complex { ct_attrs = uses }, `Extension -> 
	  merge_attribute_uses [duses;uses]
      | Complex { ct_attrs = uses }, `Restriction ->
	  let ( &= ) u1 u2 = 
	    (* by name equality over attribute uses *)
	    (u1.attr_decl.attr_name = u2.attr_decl.attr_name)
	  in
	  let l = 
	    List.filter
	      (fun use -> not (List.exists (fun u -> u &= use) (fst duses)))
	      (fst uses) in
	  merge_attribute_uses [duses;(l,false)]
      | _ -> duses
	  



  and get_derivation content =
    let (derivation,derivation_type) = 
      match _may_elem "xsd:restriction" content with
	| Some v -> (v, `Restriction)
	| None ->
	    match _may_elem "xsd:extension" content with
	      | Some v -> (v, `Extension)
	      | None -> error "No extension element found" in
    let base = resolve_typ (_qname_attr "base" derivation) in
    let base = check_force base in
    let uses = parse_attribute_uses_deriv derivation_type base derivation in
    (derivation,derivation_type,base,uses)
    
  and parse_complex_type_def n =
    let name = may_name n in
    let (base,derivation_type,uses,content_type) =
      match _may_elem "xsd:simpleContent" n with
	| Some c -> parse_simple_content n c
	| None ->
	    match _may_elem "xsd:complexContent" n with
	      | Some c -> parse_complex_content n c
	      | None -> parse_other_content n
    in
    Complex (complex name base derivation_type uses content_type)
  and parse_complex_type n =
    lazy (parse_complex_type_def n)
      
  and parse_simple_content n content =
(* TODO: implement this correctly *)
    let derivation,derivation_type,base,uses = get_derivation content in
    let content_type =
      match derivation_type,base with
	| `Restriction, Complex { ct_content = CT_simple base } ->
	    let base =
	      match _may_elem "xsd:simpleType" derivation with
		| Some s -> parse_simple_type s
		| None -> base in
	    CT_simple (simple_restrict None base (parse_facets base n))
	| `Extension, Complex { ct_content = CT_simple base } -> 
	    CT_simple base
	| `Extension, Simple st -> CT_simple st
	| _ -> 
	    assert false
	      (* Note: this is allowed by the spec ... *)
    in
    base,derivation_type,uses,content_type

  and parse_complex_content n content =
    let derivation,derivation_type,base,uses = get_derivation content in
    let mixed = bool_attr "mixed" content || bool_attr "mixed" n in
    let particle_node = find_particle derivation in
    let content_type =
      match derivation_type, particle_node with
	| `Restriction, None -> CT_empty
	| `Restriction, Some p_node ->
	    let particle = parse_particle p_node in
	    CT_model (particle, mixed)
	| `Extension, None ->
	    content_type_of_type base
	      (* TODO BUG HERE if base =
		 AnyType *)
	| `Extension, Some p_node ->
            let base_ct = content_type_of_type base in
	    let particle = parse_particle p_node in
	    match base_ct with
	      | CT_empty ->
		  CT_model (particle, mixed)
	      | CT_model (p, _) ->
		  let model = Sequence [p;particle] in
		  CT_model (particle_model 1 (Some 1) model, mixed)
	      | CT_simple _ -> assert false
    in
    base,derivation_type,uses,content_type

  and parse_other_content n =
    let uses = parse_attribute_uses n in
    let mixed = bool_attr "mixed" n in
    let content_type =
      match find_particle n with
	| None -> CT_empty
	| Some pnode ->
            let particle = parse_particle pnode in
            CT_model (particle, mixed)
    in
    AnyType,`Restriction,uses,content_type
      

  and parse_elt_decl global n =
    let local = _attr "name" n in
    let ns = if global then targetNamespace
    else
      match _may_attr "form" n with
	| Some s when (Utf8.get_str s = "qualified") -> targetNamespace
	| None when elementFormDefault -> targetNamespace
	| _ -> Ns.empty
    in
    let type_def = find_element_type n in
    let value_constr = parse_elt_value_constraint type_def n in
    let nillable = bool_attr "nillable" n in
    element (Atoms.V.mk (ns,local)) type_def value_constr nillable
	    
  (* look for a type definition, try "simpleType" child, try "complexType"
   * child, try "type" attribute, return anyType *)
  and find_element_type n =
    match _may_elem "xsd:simpleType" n with
      | Some n -> lazy (Simple (parse_simple_type n))
      | None ->
	  match _may_elem "xsd:complexType" n with
	    | Some n -> parse_complex_type n
	    | None ->
		match _may_qname_attr "type" n with
		  | Some v -> resolve_typ v
		  | None -> lazy AnyType
		      
  and parse_particle n =
    let min, max = parse_min_max n in
    let model mg = particle_model min max mg in
    let elt e n = particle min max (Elt e) (Atoms.atom n) (min = 0) in
    match _tag n with
      | "xsd:element" ->
	  (match _may_qname_attr "ref" n with
	     | Some ref -> elt (resolve_elt ref) (Atoms.V.mk ref)
	     | None ->
		 let decl = parse_elt_decl false n in
		 elt decl decl.elt_name)
      | "xsd:group" -> model (resolve_model_group (_qname_attr "ref" n)).mg_def
      | "xsd:all" | "xsd:sequence" | "xsd:choice" -> 
	  model (parse_model_group n)
      | "xsd:any" ->
	  let w = parse_wildcard n in
	  particle min max (Wildcard w) w.wild_first (min = 0)
      | _ -> error "Unexpected element for particle"
	  
  and parse_wildcard n =
    let c = parse_wildcard_cstr n in
    { wild_cstr = c;
      wild_process = parse_wildcard_process n;
      wild_first = first_of_wildcard_constraint c }
  and parse_wildcard_process n = match _may_attr "processContents" n with
    | Some t when Utf8.get_str t = "lax" -> `Lax
    | Some t when Utf8.get_str t = "skip" -> `Skip
    | Some t when Utf8.get_str t = "strict" -> `Strict
    | None -> `Strict
    | _ -> error "Wildcard processContents attribute not recognized"
  and parse_wildcard_cstr n = match _may_attr "namespace" n with
    | None -> WAny
    | Some ns when Utf8.get_str ns = "##any" -> WAny
    | Some ns when Utf8.get_str ns = "##other" -> WNot targetNamespace
    | Some ns -> WOne (List.map parse_wildcard_ns (split ns))
  and parse_wildcard_ns = function
    | ns when Utf8.get_str ns = "##targetNamespace" -> targetNamespace
    | ns when Utf8.get_str ns = "##local" -> Ns.empty
    | ns -> Ns.Uri.mk ns




  and parse_model_group n =
    match _tag n with
      | "xsd:all" ->
	  All (List.map parse_particle (_elems "xsd:element" n))
      | "xsd:sequence" ->
	  Sequence (List.map parse_particle (find_particles n))
      | "xsd:choice" ->
	  Choice (List.map parse_particle (find_particles n))
      | _ -> assert false
	  
  and parse_att_group n =
    match _may_qname_attr "ref" n with
      | Some v -> resolve_att_group v
      | None -> { ag_name = get_name n; ag_def = parse_attribute_uses n }
      
  and parse_model_group_def n =
    let name = get_name n in
    let model_group_node =
      match first n _may_elem ["xsd:all"; "xsd:choice"; "xsd:sequence"] with
	| Some m -> m
	| None -> error "No model group" in
    let model_group = parse_model_group model_group_node in
    { mg_name = name; mg_def = model_group }

  in

  let check_redef n table kind =
    let name = get_name n in
    if (QTable.mem table name) then
      error ("Redefinition of " ^ kind ^ " " ^ 
			  Ns.QName.to_string name)
    else name

  in
  let rec register n = function
    | "xsd:element" ->
	let name = check_redef n elts_elems "element" in
	QTable.add elts_elems name n;
	todo := (fun () -> ignore (resolve_elt name)):: !todo
    | ("xsd:simpleType" | "xsd:complexType") as s ->
	let name = check_redef n typs "type" in
	let l = if s="xsd:simpleType" then lazy (Simple (parse_simple_type n))
	else lazy (parse_complex_type_def n) in
	QTable.add typs name l
    | "xsd:attribute" ->
        let name = check_redef n attr_elems "attribute" in
	QTable.add attr_elems name n;
	todo := (fun () -> ignore (resolve_att name)):: !todo;
    | "xsd:attributeGroup" ->
        let name = check_redef n attr_group_elems "attribute group" in
	QTable.add attr_group_elems name n;
	todo := (fun () -> ignore (resolve_att_group name)):: !todo
    | "xsd:group" ->
        let name = check_redef n model_group_elems "model group" in
	QTable.add model_group_elems name n;
	todo := (fun () -> ignore (resolve_model_group name)):: !todo
    | "xsd:include" ->
	let local = _attr "schemaLocation" n in
	let uri = Url.local uri (Utf8.get_str local) in
	print_endline ("Include " ^ uri); flush stdout;
	parse_root uri (node_of_uri uri);
    | "xsd:import" ->
	let i uri = 
	  print_endline ("Import " ^ uri); flush stdout;
	  ignore (parse_uri uri) in
	(match _may_attr "schemaLocation" n with
	   | None ->
	       (match _may_attr "namespace" n with
		  | Some ns when Utf8.get_str ns = "http://www.w3.org/XML/1998/namespace" ->
		      i "http://www.w3.org/2001/xml.xsd"
		  | Some ns ->
		      print_endline ("Import with no schemaLocation but a namespace:"^(Utf8.get_str ns));
		      print_endline "Trying to use namespace as URI...";
		      i (Utf8.get_str ns)
		  | _ ->
		      print_endline "Import ignored, no schemaLocation")
	   | Some local ->
	       i (Url.local uri (Utf8.get_str local))
	)
	(* TODO: check namespace *)
    | "xsd:annotation" -> ()
    | s -> print_endline ("Ignore Schema element " ^ s);
  in
  _iter_elems root register;

(* end of parse_root *)

    in
    if List.exists (Ns.Uri.equal targetNamespace) !imported_ns then
      print_endline "(already imported)"
    else (
      imported_ns := targetNamespace :: !imported_ns;
      parse_root uri root
    );
    targetNamespace 

(* end of parse_uri *)

  in
  let ns = parse_uri uri in

  (* Second pass: compute the definitions *)
  List.iter (fun f -> f ()) !todo;
  {
    targetNamespace = ns;
    types = hashtbl_deref typs;
    attributes = hashtbl_values attrs;
    elements = hashtbl_values elts;
    attribute_groups = hashtbl_values attr_groups;
    model_groups = hashtbl_values model_groups
  }


