open Utility

module A = Algebra

type offset = int
type cs = csentry list
and csentry =
  | Offset of offset * A.column_type
  | Mapping of string * cs

(* TODO: use deriving Show *)
let rec print cs =
  mapstrcat " "
    (function
       | Offset (i, typ) ->
	   "Off " ^ (string_of_int i) ^ "[" ^ (A.string_of_column_type typ) ^ "]"
       | Mapping (name, cs) ->
	   "M " ^ name ^ " -> {" ^ (print cs) ^ "}")
    cs

let rec out_cs out cs =
  let attr_list xml_attributes = 
    List.map 
      (fun (name, value) -> ("", name), value) 
      xml_attributes
  in
  let tag_attr name attributes = ("", name), (attr_list attributes) in
  let prop1 n = tag_attr "property" [("name", n)] in
  let prop2 n v = tag_attr "property" [("name", n); ("value", v)] in
  let csentry = function
    | Offset (i, typ) ->
	out (`El_start (prop2 "offset" (string_of_int i)));
	out (`El_start (prop2 "type" (A.string_of_column_type typ)));
	out `El_end;
	out `El_end
    | Mapping (name, cs) ->
	out (`El_start (prop2 "mapping" name));
	out_cs out cs;
	out `El_end;
  in
    out (`El_start (prop1 "cs"));
    List.iter csentry cs;
    out `El_end
      
(* return all columns *)	
let rec leafs cs =
  List.rev
    (List.fold_left
       (fun leaf_list cs_entry ->
	  match cs_entry with
	    | Offset (o, _) -> o :: leaf_list
	    | Mapping (_, cs) -> (List.rev (leafs cs)) @ leaf_list)
       []
       cs)

let cardinality = List.length

(* increase all column names by i *)
let rec shift cs i =
  List.map
    (function
       | Offset (o, typ) -> Offset ((o + i), typ)
       | Mapping (key, cs) -> Mapping (key, (shift cs i)))
    cs

(* append to cs components *)
let append cs1 cs2 =
  cs1 @ (shift cs2 (cardinality cs1))

(* fuse two cs's by choosing the larger one *)
let fuse cs1 cs2 =
  if (List.length cs1) > (List.length cs2) then
    cs1
  else
    cs2

(* true iff cs has exactly one flat column *)
let is_operand cs =
  if List.length cs <> 1 then
    false
  else
    match (List.hd cs) with
      | Offset _ -> true
      | _ -> false

(* look up the column corresponding to a record field *)
let lookup_record_field cs field =
  let rec loop = function
    | (Offset _) :: tl ->
	loop tl
    | (Mapping (key, cs)) :: tl ->
	if key = field then
	  cs
	else
	  loop tl
    | [] ->
	failwith "Cs.get_mapping: unknown field name"
  in
    loop cs

(* remove all mappings with keys in fields *)
let filter_record_fields cs fields =
  List.filter
    (function 
       | Mapping (key, _) when StringSet.mem key fields -> false 
       | _ -> true)
    cs

(* return all top-level record fields *)
let record_fields cs =
  List.fold_left
    (fun l c ->
       match c with
	 | Mapping (key, _) -> key :: l
	 | Offset _ -> l)
    []
    cs
