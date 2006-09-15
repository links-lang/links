(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

open Ident
open Schema_types
open Schema_common
open Schema_validator
open Encodings
open Typepat

let xsd = Schema_xml.xsd
let is_xsd (ns,l) local =
  (Ns.Uri.equal ns xsd) && (String.compare (Utf8.get_str l) local = 0)

let complex_memo = Hashtbl.create 213
  
(* TODO: better approx *)
let xsd_any_type = Types.any
  
let nil_type = mk_type Sequence.nil_type
  
let mk_len_regexp min max base =
  let rec repeat_regexp re = function
    | 0 -> mk_epsilon
    | n -> mk_seq re (repeat_regexp re (pred n))
  in
  let min_regexp = repeat_regexp base min in
  match max with
    | Some max ->
	let rec aux acc = function
          | 0 -> acc
          | n -> aux (mk_alt mk_epsilon (mk_seq base acc)) (pred n)
        in
        mk_seq min_regexp (aux mk_epsilon (max-min))
    | None -> mk_seq min_regexp  (mk_star base)
	
let mk_seq_derecurs base facets =
  let min,max = match facets with
    | { length = Some (v, _) } -> v, Some v
    | { minLength = Some (v, _); maxLength = None } -> v, None
    | { minLength = None; maxLength = Some (v, _) } -> 1, Some v
    | { minLength = Some (a,_); maxLength = Some (b, _) } -> a, Some b
    | _ -> 1, Some 1 in
  Sequence.repet min max base
    
let xsi_nil_type =
  let m = LabelMap.singleton xsi_nil_label (Types.cons Builtin_defs.true_type)
  in
  Types.record_fields (false,m)
    
    
    
let rec simple_type = function
  | { st_name = Some name } 
      when Schema_builtin.is name ->
      Schema_builtin.cd_type (Schema_builtin.get name)
  | { st_variety = Atomic st } ->
      (* TODO: apply facets *)
      Schema_builtin.cd_type (Schema_builtin.of_st st)
  | { st_variety = List item; st_facets = facets } ->
      mk_seq_derecurs (simple_type item) facets
  | { st_variety = Union members; st_facets = facets } ->
      let members = List.map simple_type members in
      List.fold_left (fun acc x -> Types.cup x acc) Types.empty members
	
let attr_uses (attrs,other) =
  let fields = 
    List.map 
      (fun at ->
         let r =
           match at.attr_use_cstr with
             | Some (`Fixed (_,v)) -> Types.constant (Value.inv_const v)
             | _ -> simple_type at.attr_decl.attr_typdef
         in
	 (not at.attr_required,  at.attr_decl.attr_name, r))
      attrs in
  Types.rec_of_list false fields
    
let rec regexp_of_term = function
  | Model group -> regexp_of_model_group group
  | Elt decl -> mk_elem (elt_decl decl)
  | Wildcard w -> mk_elem (wildcard w)
      
and wildcard w = 
  mk_type (Builtin_defs.any_xml_with_tag w.wild_first)
    
and regexp_of_model_group = function
  | Choice l ->
      List.fold_left
	(fun acc particle -> mk_alt acc (regexp_of_particle particle))
        mk_empty l
  | All l | Sequence l ->
      List.fold_left
        (fun acc particle -> mk_seq acc (regexp_of_particle particle))
	mk_epsilon l
	
and regexp_of_particle p =
  mk_len_regexp p.part_min p.part_max (regexp_of_term p.part_term)
    
and get_complex ct =
  try Hashtbl.find complex_memo ct.ct_uid
  with Not_found -> 
    let slot = mk_delayed () in
    let attrs = attr_uses ct.ct_attrs in
    let r = mk_prod (mk_type attrs) slot in
    Hashtbl.add complex_memo ct.ct_uid r;
    link slot (content ct.ct_content);
    r
      
and complex nil ct =
  let c = get_complex ct in
  if nil then 
    let (attrs,content) = get_ct c in
    let attrs = Types.Record.merge attrs xsi_nil_type in
    mk_or c (mk_type (Types.times (Types.cons attrs) Sequence.nil_node))
  else c
    
and content = function
  | CT_empty -> mk_type Sequence.nil_type
  | CT_simple st -> mk_type (simple_type st)
  | CT_model (particle, mixed) ->
      let regexp = regexp_of_particle particle in
      rexp_simplify ~mix:mixed regexp
	
    
and elt_decl elt =
  let atom_type = mk_type (Types.atom (Atoms.atom elt.elt_name)) in
  let content=complex_type_def elt.elt_nillable (Lazy.force elt.elt_typdef) in
  let content =
    match elt.elt_cstr with
      | Some (`Fixed (_,v)) ->
	  mk_and content (
	    mk_type (Types.times 
		     (Types.cons Types.any)
		     (Types.cons (Types.constant (Value.inv_const v)))))
      | _ -> content in
  mk_xml atom_type content

and complex_type_def nil = function
  | AnyType -> 
      mk_type (Types.times 
	       (Types.cons Types.empty_open_record)
	       (Types.cons xsd_any_type))
  | Simple st ->
      let nonnil =
	Types.times 
	  (Types.cons Types.empty_closed_record) 
	  (Types.cons (simple_type st))
      in
      let t =
	if nil then
	  Types.cup nonnil
	    (Types.times
	       (Types.cons xsi_nil_type)
	       (Types.cons Sequence.nil_type))
	else nonnil in
      mk_type t
  | Complex ct -> complex nil ct
      
let model_group g = rexp_simplify ~mix:false (regexp_of_model_group g)
  
let get_type d = internalize d; typ d

let type_def = function
  | AnyType -> xsd_any_type
  | Simple st -> simple_type st
  | Complex ct -> get_type (mk_xml (mk_type Types.any) (complex false ct))
let elt_decl x = get_type (elt_decl x)
let model_group x = get_type (model_group x.mg_def)
let attr_group ag = attr_uses ag.ag_def


let load_schema schema_name uri =
  let schema_name = schema_name ^ "." in
  let log_schema_component kind name cd_type =
    if not (Schema_builtin.is name) then begin
      Types.Print.register_global schema_name name cd_type;
      
(*      Format.fprintf Format.std_formatter "Registering schema %s: %a@." kind 
	Ns.QName.print name; *)
      
    end 
  in
  let env = ref Env.empty in
  let defs kind name cd_type v lst =
    List.iter
      (fun def ->
	 let name = name def in
	 let cd_type = cd_type def in
	 log_schema_component kind name cd_type;
	 env := Env.add (Ident.ident name) (cd_type, v def) !env
      ) lst
  in
  let schema = Schema_parser.schema_of_uri uri in
  defs "attribute group" (fun ag -> ag.ag_name) attr_group 
    (fun x -> VAttrGp x) schema.attribute_groups;
  defs "model group" (fun mg -> mg.mg_name) model_group 
    (fun x -> VModelGp x) schema.model_groups;
  defs "type" name_of_type_definition type_def 
    (fun x -> VType x) schema.types;
  defs "element" (fun e -> Atoms.V.value e.elt_name) elt_decl 
    (fun x -> VElem x) schema.elements;
  schema.targetNamespace, !env


let () = 
  Typer.load_schema := load_schema;
