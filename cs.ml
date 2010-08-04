(*pp deriving *)
open Utility

module A = Algebra

type column_type = [ A.pf_type | `Surrogate | `Unit | `Tag ] deriving (Show)

let is_primitive_col = function
  | `Surrogate -> false
  | `Tag -> false
  | _ -> true

(* FIXME: there should be only one definition of implementation_type. at the moment 
   there are three (Query2.Annotate, Cs, Heapresult) *)
type implementation_type = [`Atom | `List] deriving (Show)

(* the number of a column starting with 1 *)
type offset = int deriving (Show)

type column = offset * column_type deriving (Show)

(* the type of the cs component which describes how the represented 
   values (primitive values, records, lists, tags) are mapped onto 
   the flat columns *)
type cs = csentry list
and csentry = 
    [ `Column of column 
    | `Tag of column * column
    | `Mapping of string * cs ] 
      deriving (Show)

let show = Show.show show_cs

(* return all columns together with their column type *)
let rec leafs cs =
    List.fold_right
       (fun cs_entry leaf_list ->
	  match cs_entry with
	    | `Column _
	    | `Tag _ -> cs_entry :: leaf_list
	    | `Mapping (_, cs) -> (leafs cs) @ leaf_list)
      cs
      []
    
let offset_of_csentry = function
  | `Column (off, _) -> [off]
  | `Tag ((toff, _), (roff, _)) -> [toff; roff]
  | `Mapping _ -> assert false

let is_atomic = function `Column (_, t) when is_primitive_col t -> true | _ -> false
let is_variant = function `Tag _ -> true | _ -> false
let is_record = function `Mapping _ -> true | _ -> false
let is_boxed_list = function `Column (_, `Surrogate) -> true | _ -> false

(* return all columns *)	
let rec offsets = List.flatten -<- (List.map offset_of_csentry) -<- leafs

let cardinality (cs : cs) = List.length (offsets cs)

(* increase all column names by i *)
let rec shift cs i =
  List.map
    (function
       | `Column (o, typ) -> `Column ((o + i), typ)
       | `Tag ((tago, tagt), (refo, reft)) -> `Tag ((tago + i, tagt), (refo + i, reft))
       | `Mapping (key, cs) -> `Mapping (key, (shift cs i)))
    cs

(* append two cs components *)
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
      | `Column _ -> true
      | _ -> false

(* look up the sub-cs corresponding to a record field *)
let lookup_record_field cs field =
  let rec loop = function
    | (`Column _) :: _ -> assert false
    | (`Tag _) :: _ -> assert false
    | (`Mapping (key, cs)) :: _ when key = field -> cs
    | (`Mapping _) :: tl -> loop tl
    | [] ->
	failwith "Cs.get_mapping: unknown field name"
  in
    loop cs

(* remove all mappings with keys in fields *)
let filter_record_fields cs fields =
  List.filter
    (function 
       | `Mapping (key, _) when StringSet.mem key fields -> false 
       | _ -> true)
    cs

(* return all top-level record fields *)
let record_fields cs =
  List.fold_left
    (fun l c ->
       match c with
	 | `Mapping (key, _) -> key :: l
	 | _ -> assert false)
    []
    cs

type field_name = string
type atom_type =
    [ `Primitive of column_type
	(* FIXME is additional information needed from type `Tag? *)
    | `Tag 
    | `Record of field_name list ]

let atom_type = function
  | [(`Column (_, t))] -> `Primitive t
  | [`Tag _] -> `Tag
  | [] -> failwith "Cs.atom_type: empty cs" 
  | cs_entries ->
      `Record (
	List.map
	  (function 
	     | `Mapping (fieldname, _) -> fieldname
	     | `Column _ -> failwith "Cs.atom_type: toplevel offset in record cs"
	     | `Tag _ -> failwith "Cs.atom_type: toplevel tag in record cs")
	  cs_entries)
  | _ -> assert false
      
let rec sort_record_columns = function
  | [(`Column _) as col] -> [col]
  | [(`Tag _) as tag] -> [tag]
  | [] -> failwith "Cs.sort_record_columns: empty cs"
  | cs_entries ->
      let cmp m1 m2 = 
	match (m1, m2) with
	  | `Mapping (field1, _), `Mapping (field2, _) ->
	      compare field1 field2
	  | _ -> 
	      failwith "Cs.sort_record_columns: multiple flat offsets"
      in
      let cs_entries = List.sort cmp cs_entries in
	List.map 
	  (function 
	     | `Mapping (field, cs) -> `Mapping (field, (sort_record_columns cs))
	     | _ -> failwith "Cs.sort_record_columns: multiple flat offsets")
	  cs_entries

(* if cs1 and cs2 differ in length, use only the larger one. *)
let longer_cs cs1 cs2 = 
  match cardinality cs1, cardinality cs2 with
    | a, b when a > b -> (cs1, cs1)
    | a, b when a < b -> (cs2, cs2)
    | _, _ -> (cs1, cs2)

(* replace the column numbers used in cs with the numbers given by new_cols (in-order) *)
let map_cols new_cols cs =
  let rec map_cols_1 new_cols cs = 
    match cs with
      | `Column (_, t) :: cs ->
	  (match new_cols with
	     | c' :: cols -> 
		 let cols_rest, cs' = map_cols_1 cols cs in
		   cols_rest, (`Column (c', t) :: cs')
	     | _ -> assert false)
      | `Tag (_, _) :: cs ->
	  (match new_cols with
	     | tagcol' :: refcol' :: cols ->
		 let cols_rest, cs' = map_cols_1 cols cs in
		   cols_rest, ((`Tag ((tagcol', `Tag), (refcol', `Surrogate))) :: cs')
	     | _ -> assert false)
      | `Mapping (field, nested_cs) :: cs ->
	  let cols_rest, nested_cs' = map_cols_1 new_cols nested_cs in
	  let cols_rest, cs' = map_cols_1 cols_rest cs in
	    cols_rest, (`Mapping (field, nested_cs') :: cs')
      | [] ->
	  (new_cols, cs)
  in
    snd (map_cols_1 new_cols cs)
