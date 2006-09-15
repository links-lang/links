(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

let debug = false

open Printf

open Encodings
open Schema_pcre
open Schema_common
open Schema_types
open Value

module QTable = Hashtbl.Make(Ns.Label)

let ppf = Format.std_formatter

  (** {2 Misc} *)

let empty_record = Value.vrecord []
let foo_atom = Value.Atom (Atoms.V.mk_ascii "foo")
let foo_event = E_char_data (Utf8.mk "")

let qtable_is_empty tbl =
  try QTable.iter (fun _ _ -> raise Exit) tbl; true
  with Exit -> false

let string_of_value value =
  let buf = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buf in
  Format.fprintf fmt "%a@?" Value.print value;
  Buffer.contents buf

let foo_qname = Ns.empty, Utf8.mk ""

type context = {
  ctx_stream: event Stream.t;
  mutable ctx_mixed: bool;
  mutable ctx_current: Value.t;
}

let subctx mixed ctx = { ctx with ctx_current = Value.nil; ctx_mixed = mixed }

let push_str ctx s =
  { ctx with ctx_stream = Stream.icons (E_char_data s) ctx.ctx_stream }

let get ctx = ctx.ctx_current

let rec only_ws s i =
  (i = 0) ||
  (let i = pred i in match (String.unsafe_get s i) with
     | ' ' | '\t' | '\n' | '\r' -> only_ws s i
     | _ -> false)

let only_ws s =
  let s = Utf8.get_str s in
  only_ws s (String.length s)

let error s = raise (XSI_validation_error s)

let concat ctx v = ctx.ctx_current <- Value.concat ctx.ctx_current v
let append ctx v = ctx.ctx_current <- Value.append ctx.ctx_current v

let xml qname attrs content =
  Value.Xml (Value.Atom qname, attrs, content)


let peek ctx = 
  match Stream.peek ctx.ctx_stream with
    | None -> error "Unexpected end of stream"
    | Some e -> e

let next ctx =
  try Stream.next ctx.ctx_stream
  with Stream.Failure -> error "Unexpected end of stream"

let junk ctx =
  Stream.junk ctx.ctx_stream

let get_string ctx =
  let b = Buffer.create 67 in
  let rec aux () =
    match peek ctx with
      | E_char_data s ->
          junk ctx;
	  Buffer.add_string b (Utf8.get_str s);
	  aux ()
      | E_start_tag _ ->
	  error "XML element found in simple content"
      | _ -> ()
  in
  aux ();
  Utf8.mk (Buffer.contents b)

let rec copy_pcdata ctx =
  match peek ctx with
    | E_char_data s ->
        junk ctx;
	concat ctx (string_utf8 s);
	copy_pcdata ctx
    | _ -> ()

let rec ignore_ws ctx =
  match peek ctx with
    | E_char_data s when only_ws s -> 
	junk ctx;
	ignore_ws ctx
    | E_char_data _ -> 
        error "Unexpected char data in non-mixed content"
    | _ -> ()

let do_pcdata ctx =
  if ctx.ctx_mixed then copy_pcdata ctx else ignore_ws ctx

let expect_end_tag ctx =
  match next ctx with
    | E_end_tag -> ()
    | ev -> error (sprintf "Expected end tag, found %s" (string_of_event ev))

let is_end_tag ctx =
  match peek ctx with
    | E_end_tag -> true
    | ev -> false

let check_nil ctx =
  match peek ctx with
    | E_end_tag -> ()
    | ev -> error (sprintf "Non-empty content with xsi:nil set : %s" 
		     (string_of_event ev))

let expect_start_tag ctx tag =
  match next ctx with
    | E_start_tag t when Atoms.V.equal t tag -> ()
    | ev -> error (sprintf "Expected tag %s, found %s" 
		     (Atoms.V.to_string tag) (string_of_event ev))


let expect_any_start_tag ctx =
  match next ctx with
    | E_start_tag t -> t
    | ev -> error (sprintf "Expected start tag, found %s" 
		     (string_of_event ev))

type attrs = {
  xsi_nil: bool;
  attrs: (Ns.Label.t * Utf8.t) list
}

let get_bool v = 
  match Utf8.get_str v with
    | "true" | "1" -> true
    | "false" | "0" -> false
    | _ -> error "Invalid boolean value"

let get_attributes ctx =
  let rec aux attrs nil =
    match peek ctx with
      | E_attribute (qname,value) when Ns.Label.equal qname xsi_nil_label ->
	  junk ctx;
	  aux attrs (get_bool value)
      | E_attribute (qname, value) ->
          junk ctx;
          aux ((qname,value)::attrs) nil
      | _ -> { attrs = attrs; xsi_nil = nil }
  in
  aux [] false

let rec tries funs arg =
  match funs with
  | [] -> raise Not_found
  | f :: tl ->
      try f arg
      with XSI_validation_error _ ->
        tries tl arg

let space_RE = pcre_regexp " "
let split = pcre_split ~rex:space_RE

  (** {2 Facets validation} *)

module Schema_facets:
sig
  exception Facet_error of string
  val facets_valid: facets -> Value.t -> unit
end
=
struct

  open Big_int
  open Value

  exception Facet_error of string

    (* compute the length of a particular CDuce *)
    (* STRONG ASSUMPTION: v is a CDuce value built via "validate_simple_type"
     * function below, thus it contains no sequence of characters, but strings
     * and no Concat, but just Pair *)
  let length v =
    let rec aux acc = function
      | Pair (_, rest) -> aux (succ acc) rest
      | _ -> 0
    in
    aux 0 v

  let length_valid len value =
    if (length value != len) 
    then raise (Facet_error "length")
  let minLength_valid min_len value =
    if (length value < min_len)
    then raise (Facet_error "minLength")
  let maxLength_valid max_len value =
    if (length value > max_len) 
    then raise (Facet_error "maxLength")

  let enumeration_valid enum value =
    if not (List.exists (fun x -> Value.equal value x) enum) 
    then raise (Facet_error "enumeration")

  let maxInclusive_valid max_inc value =
    if value |>| (max_inc) then raise (Facet_error "maxInclusive")
  let maxExclusive_valid max_exc value =
    if value |>=| (max_exc) then raise (Facet_error "maxExclusive")
  let minInclusive_valid min_inc value =
    if value |<| (min_inc) then raise (Facet_error "minInclusive")
  let minExclusive_valid min_exc value =
    if value |<=| (min_exc) then raise (Facet_error "minInclusive")

    (* check facets validaty rules other than pattern and whiteSpace. "value"
    parameter should already be white space normalized and pattern valid.
    Assumption: facets set contains only facets that are applicable to the type
    of value *)
  let facets_valid facets value =
    (* TODO efficiency *)
    (* all facets are always checked, but we know that in some cases only some
     * of them can be present; e.g. if variety is union only pattern and
     * enumeration are possibles ... *)
    (match facets.length with
    | None ->
        (match facets.minLength with
        | None -> ()
        | Some (len, _) -> minLength_valid len value);
        (match facets.maxLength with
        | None -> ()
        | Some (len, _) -> maxLength_valid len value);
    | Some (len, _) -> length_valid len value);
    (match facets.enumeration with
    | None -> ()
    | Some enum -> enumeration_valid enum value);
    (match facets.maxInclusive with
    | None -> ()
    | Some (lim, _) -> maxInclusive_valid lim value);
    (match facets.maxExclusive with
    | None -> ()
    | Some (lim, _) -> maxExclusive_valid lim value);
    (match facets.minInclusive with
    | None -> ()
    | Some (lim, _) -> minInclusive_valid lim value);
    (match facets.minExclusive with
    | None -> ()
    | Some (lim, _) -> minExclusive_valid lim value);
  (*
    (match facets.totalDigits with
    | None -> ()
    | Some (dig, _) -> totalDigits_valid dig value);
    (match facets.fractionDigits with
    | None -> ()
    | Some (dig, _) -> fractionDigits_valid dig value)
  *)

end

  (** {2 Simple type validation} *)

let rec validate_simple_type def s = match def with
  | { st_name = Some name } when Schema_builtin.is name ->
      (try Schema_builtin.validate (Schema_builtin.get name) s
       with Schema_builtin.Error name ->
	 error (sprintf "%s isn't a valid %s"
		  (Utf8.to_string s) name))
  | { st_variety = Atomic st; st_facets = facets } ->
      let literal = normalize_white_space (fst facets.whiteSpace) s in
      let value = validate_simple_type st literal in
      Schema_facets.facets_valid facets value;
      value
  | { st_variety = List item; st_facets = facets } ->
      let literal = normalize_white_space (fst facets.whiteSpace) s in
      let items = List.map (validate_simple_type item) (split literal) in
      let value = Value.sequence items in
      Schema_facets.facets_valid facets value;
      value
  | { st_variety = Union members; st_facets = facets } ->
      let value = tries (List.map validate_simple_type members) s in
      Schema_facets.facets_valid facets value;
      value

  (** {2 Complex type validation} *)

let rec validate_any_type ctx =
  (* assumption: attribute events (if any) come first *)
  let attrs = get_attributes ctx in
  let attrs = List.map (fun (n,v) -> (n,Value.string_utf8 v)) attrs.attrs in

  let ctx = subctx true ctx in
  let rec aux attrs =
    copy_pcdata ctx;
    match peek ctx with
    | E_start_tag qname ->
        junk ctx;
        let (attrs, content) = validate_any_type ctx in
        expect_end_tag ctx;
	append ctx (xml qname attrs content);
        aux ()
    | E_end_tag -> ()
    | _ -> assert false
  in
  aux ();
  (Value.vrecord attrs, get ctx)

let validate_wildcard ctx w =
  let qname = expect_any_start_tag ctx in
  if not (Atoms.contains qname w.wild_first)
  then error (sprintf "Tag %s is not accepted by the wildcard" 
		(Atoms.V.to_string qname));
  let (attrs, content) = validate_any_type ctx in
  expect_end_tag ctx;
  xml qname attrs content

let check_fixed fixed value =
  if not (Value.equal fixed value) then
    error (sprintf "Expected fixed value: %s; found %s"
      (string_of_value fixed) (string_of_value value))



let next_tag ctx =
  match peek ctx with
    | E_start_tag qname -> qname
    | _ -> raise Not_found

let validate_attribute_uses attrs (attr_uses,anyattr) =
  let tbl = QTable.create 11 in
  List.iter
    (fun use -> QTable.add tbl use.attr_decl.attr_name use)
    attr_uses;
  let attribs = ref [] in
  List.iter
    (fun (qname, value) ->
       let value = 
	 try
	   let a = QTable.find tbl qname in
	   let value = validate_simple_type a.attr_decl.attr_typdef value in
	   (match a.attr_use_cstr with  (* check fixed constraint *)
              | Some (`Fixed (_,v)) -> check_fixed v value
              | _ -> ());
	   QTable.remove tbl qname;
	   value
	 with Not_found ->
	   if anyattr then Value.string_utf8 value
	   else error 
	     (sprintf "Unexpected attribute: %s" (Ns.Label.string_of_attr qname))
       in
       attribs := (qname, value) :: !attribs
    ) attrs.attrs;
  if attrs.xsi_nil then
    attribs := (xsi_nil_label, Value.vtrue) :: !attribs;
  QTable.iter
    (fun qname at ->
       if at.attr_required then  (* check for missing required attributes *)
         error (sprintf "Required attribute %s is missing"
		  (Ns.Label.string_of_attr qname))
       else  (* add default values *)
         match at.attr_use_cstr with
           | Some (`Default (_,v)) -> attribs := (qname, v) :: !attribs
           | _ -> ())
    tbl;
  Value.vrecord !attribs

let rec validate_element ctx elt =
  expect_start_tag ctx elt.elt_name;
  let attrs = get_attributes ctx in
  if (attrs.xsi_nil && not elt.elt_nillable) then
    error "xsi:nil attribute on non-nillable element";
  let is_empty = is_end_tag ctx in
  let ctx =
    match is_empty, elt.elt_cstr with
      | true, Some (`Default (v,_) | `Fixed (v,_)) -> push_str ctx v
      | _ -> ctx in
  let (attrs, content) = validate_type ctx attrs (Lazy.force elt.elt_typdef) in
  (match is_empty, elt.elt_cstr with
      | false, Some (`Fixed (_,v)) -> check_fixed v content
      | _ -> ());
  expect_end_tag ctx;
  xml elt.elt_name attrs content

and validate_type ctx attrs = function
  | AnyType -> validate_any_type ctx
  | Simple st_def ->
      if (List.length attrs.attrs > 0) then
	error "attribute on element with simple content";
      if attrs.xsi_nil then (check_nil ctx; 
			     Value.vrecord [xsi_nil_label,Value.vtrue],
			     Value.nil)
      else (empty_record, validate_simple_type st_def (get_string ctx))
  | Complex ct_def -> validate_complex_type ctx attrs ct_def

and validate_complex_type ctx attrs ct =
  let content =
    if attrs.xsi_nil then (check_nil ctx; Value.nil)
    else validate_content_type ctx ct.ct_content in
  let attrs = validate_attribute_uses attrs ct.ct_attrs in
  (attrs, content)

and validate_content_type ctx content_type =
  match content_type with
  | CT_empty -> 
      Value.nil
  | CT_simple st_def -> 
      validate_simple_type st_def (get_string ctx)
  | CT_model (particle, mixed) ->
      let ctx = subctx mixed ctx in
      validate_particle ctx particle;
      get ctx

and validate_particle ctx particle =
  let rec validate_once ~cont_ok ~cont_failure =
    do_pcdata ctx;
    match peek ctx with
    | E_start_tag qname 
	when Atoms.contains qname particle.part_first ->
	validate_term ctx particle.part_term;
        cont_ok ()
    | ev -> 
	cont_failure ev
  in
  let rec required = function
    | 0 -> ()    | n ->
        validate_once
          ~cont_ok:(fun () -> required (pred n))
          ~cont_failure:(fun event ->
			   if particle.part_nullable then ()
			   else
			     error (sprintf "Unexpected content: %s"
				      (string_of_event event)))
  in
  let rec optional = function
    | None ->
        validate_once
          ~cont_ok:(fun () -> optional None)
          ~cont_failure:(fun _ -> ())
    | Some 0 -> ()
    | Some n ->
        validate_once
          ~cont_ok:(fun () -> optional (Some (pred n)))
          ~cont_failure:(fun _ -> ())
  in
  required particle.part_min;
  optional
    (match particle.part_max with 
       | None -> None 
       | Some v -> Some (v - particle.part_min));
  do_pcdata ctx

and validate_term ctx term =
  match term with
  | Elt elt -> append ctx (validate_element ctx elt)
  | Model mg -> validate_model_group ctx mg
  | Wildcard w -> append ctx (validate_wildcard ctx w)

and validate_choice ctx particles =
(* TODO: Handle case when one of the choices is nullable *)
  let tbl = Atoms.mk_map
    (List.map (fun p -> p.part_first, p) particles) in
  do_pcdata ctx;
  try
    (match peek ctx with
       | E_start_tag q -> validate_particle ctx (Atoms.get_map q tbl)
       | _ -> raise Not_found)
  with Not_found ->
    error (sprintf "Cannot choose branch of choice group")

and validate_all_group ctx particles =
  let slots = List.map (fun p -> (p, ref None)) particles in
  let tbl = Atoms.mk_map
    (List.map (fun (p,slot) -> p.part_first, (p,slot)) slots) in
  
  let rec aux () =
    match peek ctx with
      | E_start_tag q ->
	  let p,slot = Atoms.get_map q tbl in
	  (match !slot with
	    | Some x -> ()
	    | None -> 
		let ctx = subctx ctx.ctx_mixed ctx in
		validate_particle ctx p;
		slot := Some (get ctx); aux ())
      | _ -> ()
  in
  do_pcdata ctx;
  aux ();
  List.iter
    (fun (p,slot) ->
       match !slot with
	 | Some x -> concat ctx x
	 | None when p.part_nullable -> ()
	 | None -> error "One particle of the all group is missing"
    ) slots


and validate_model_group ctx model_group =
  match model_group with
  | All particles -> validate_all_group ctx particles
  | Choice particles -> validate_choice ctx particles
  | Sequence particles -> List.iter (validate_particle ctx) particles


let ctx stream =
  { ctx_stream = stream;
    ctx_mixed = false;
    ctx_current = Value.Absent }

let validate_element decl value =
  let ctx = ctx (stream_of_value value) in
  validate_element ctx decl

let get_str v = 
  if not (is_str v) then
    error
      "Only string values could be validate against simple types";
  fst (get_string_utf8 v)

let validate_type def value =
  match def with
  | AnyType -> value  (* shortcut *)
  | Simple st_def -> validate_simple_type st_def (get_str value)
  | Complex ct_def ->
      let ctx = ctx (stream_of_value value) in
      let start_tag = expect_any_start_tag ctx in
      let attrs = get_attributes ctx in
      let (attrs, content) = validate_complex_type ctx attrs ct_def in
      expect_end_tag ctx;
      Value.Xml (Value.Atom start_tag, attrs, content)

(*
let validate_attribute decl value =
  assert false; (* TODO see the .mli *)
  (match value with
  | Record _ -> ()
  | _ ->
      error
        "Only record values could be validated against attributes");
  let (name, st_def, constr) = decl in
  let qname = (schema.targetNamespace, name) in
  let fields = Value.get_fields value in
  let found = ref false in
  let rec aux = function
    | [] -> []
    | (qname', value) :: rest when qname' = qname ->
        found := true;
        (qname', validate_simple_type st_def value) :: aux rest
    | field :: rest -> field :: aux rest

  in
  let fields = aux (Value.get_fields value) in
  let fields =
    if not !found then
      match constr with
      | Some (`Default v) -> (qname, v) :: fields
      | _ ->
          error (sprintf
            "Attribute %s was not found and no default value was provided"
            (Ns.QName.to_string qname))
    else
      fields
  in
  Value.vrecord fields
*)

let validate_attribute_group { ag_def = attr_uses } value =
  let stream =
    match value with
    | Record _ ->
        Stream.of_list
          ((List.map
            (fun (qname, v) ->
              E_attribute (qname, fst (Value.get_string_utf8 v)))
            (Value.get_fields value)) @
            [ foo_event ])
    | _ ->
        error
          "Only record values could be validated against attribute groups"
  in
  let ctx = ctx stream in
  let attrs = get_attributes ctx in
  validate_attribute_uses attrs attr_uses


let validate_model_group { mg_def = mg } value =
  if not (Value.is_seq value) then
    error
      "Only sequence values could be validated against model groups";
  let stream = stream_of_value (Value.Xml (foo_atom, empty_record, value)) in
  Stream.junk stream;
  let ctx = ctx stream in
  validate_model_group ctx mg;
  get ctx



type t =
  | VAttrGp of attribute_group_definition
  | VModelGp of model_group_definition
  | VType of type_definition
  | VElem of element_declaration

let run s v =
  match s with
    | VAttrGp x -> validate_attribute_group x v
    | VModelGp x -> validate_model_group x v
    | VType x -> validate_type x v
    | VElem x -> validate_element x v
