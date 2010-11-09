(*pp deriving *)
open Utility

module A = Algebra

type column_type = [ A.pf_type | `Surrogate | `Unit | `Tag | `EmptyListLit ] deriving (Show)

let column_type_of_constant = function
  | `Bool _ -> `BoolType
  | `Int _ -> `IntType
  | `String _ -> `StrType
  | `Float _ -> `FloatType
  | `Char _ -> `CharType

let column_type_of_pf_type : Algebra.pf_type -> column_type = function
  | `BoolType -> `BoolType
  | `IntType -> `IntType
  | `StrType -> `StrType
  | `FloatType -> `FloatType
  | `CharType -> `CharType
  | `NatType -> `NatType

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

type field_name = string deriving (Show)

(* the type of the cs component which describes how the represented 
   values (primitive values, records, lists, tags) are mapped onto 
   the flat columns *)
type t = 
  | Column of column
  | Tag of column * column
  | Mapping of (field_name * t) list
      deriving (Show)

let show = Show.show show_t

(* return all columns together with their column type *)
let rec leafs = function
  | Column _ as c -> [c]
  | Tag _ as c-> [c]
  | Mapping fields -> List.flatten (List.map (leafs -<- snd) fields)
    
let offsets_of_cs = function
  | Column (off, _) -> [off]
  | Tag ((toff, _), (roff, _)) -> [toff; roff]
  | Mapping _ -> assert false

let is_atomic = function Column (_, t) when is_primitive_col t -> true | _ -> false
let is_variant = function Tag _ -> true | _ -> false
let is_record = function Mapping _ -> true | _ -> false
let is_boxed_list = function Column (_, `Surrogate) -> true | _ -> false
let is_empty_list_lit = function Column (_, `EmptyListLit) -> true | _ -> false

(* return all columns *)	
let rec offsets = List.flatten -<- (List.map offsets_of_cs) -<- leafs

let cardinality (cs : t) = List.length (offsets cs)

let rec shift i cs =
  match cs with
    | Column (o, typ) -> Column ((o + i), typ)
    | Tag ((tago, tagt), (refo, reft)) -> Tag ((tago + i, tagt), (refo + i, reft))
    | Mapping fields -> Mapping (alistmap (shift i) fields)

(* append two mappings *)
let append_mappings cs1 cs2 =
  match cs1, cs2 with
    | Mapping fields1, Mapping fields2 ->
      Mapping (fields1 @ (alistmap (shift (cardinality cs1)) fields2))
    | _ -> assert false

(* fuse two cs's by choosing the larger one *)
let choose_nonempty cs1 cs2 = 
  let empty = function
    | Column (_, `EmptyListLit) -> true
    | _ -> false
  in
  match (empty cs1), (empty cs2) with
    | true, true 
    | false, false -> cs1
    | true, false -> cs2
    | false, true -> cs1

let lookup_record_field cs field =
  match cs with
    | Mapping fields -> 
      begin
	try 
	  List.assoc field fields
	with Not_found -> failwith "Cs.get_mapping: unknown field name"
      end
    | _ ->
      assert false

(* remove all mappings with keys in fields *)
let filter_record_fields cs to_remove =
  match cs with
    | Mapping fields -> Mapping (List.filter (function (f, _) -> not (StringSet.mem f to_remove)) fields)
    | _ -> assert false

let rec sort_record_columns = function
  | Column _ as c -> c
  | Tag _ as t -> t
  | Mapping fields ->
    let cmp a b = compare (fst a) (fst b) in
    let fields_sorted = List.sort cmp fields in
    Mapping (alistmap sort_record_columns fields_sorted)

(* replace the column numbers used in cs with the numbers given by new_cols (in-order) *)
let map_cols new_cols cs =
  let rec map_cols_1 new_cols cs =
    match cs with
      | Column (_, t) ->
	begin
	  match new_cols with
	    | c' :: cols -> cols, Column (c', t)
	    | _ -> assert false
	end
      | Tag (_, _) ->
	begin
	  match new_cols with
	    | tagcol' :: refcol' :: cols -> cols, Tag ((tagcol', `Tag), (refcol', `Surrogate))
	    | _ -> assert false
	end
      | Mapping fields ->
	let aux (new_fields, new_cols) (field, cs) = 
	  let cols_rest, cs' = map_cols_1 new_cols cs in
	  ((field, cs') :: new_fields), cols_rest
	in
	let new_fields, cols_rest = List.fold_left aux ([], new_cols) fields in
	cols_rest, Mapping (List.rev new_fields)
  in
  snd (map_cols_1 new_cols cs)

	    
