(* Representations of types *)

open Pickler
open Utility

open Type_basis

type type_var_set = Type_basis.type_var_set
type primitive = Type_basis.primitive

(* Types for datatypes *)
type datatype = (datatype, row) type_basis
and field_spec = datatype field_spec_basis
and field_spec_map = datatype field_spec_map_basis
and row_var = row row_var_basis
and row = (datatype, row_var) row_basis
    deriving (Show, Pickle)
			  
type type_variable = Type_basis.type_variable
type quantifier = Type_basis.quantifier

type assumption = datatype assumption_basis
    deriving (Show, Pickle)
type environment = datatype environment_basis
    deriving (Show, Pickle)

let (-->) x y = `Function (x,y)

(* Caveat: Map.fold behaves differently between Ocaml 3.08.3 and 3.08.4,
   so we need to reverse the result generated.
*)
let map_fold_increasing = ocaml_version_atleast [3; 8; 4]

let split_fields : 'typ field_spec_map_basis -> (string * 'typ) list * string list =
  fun field_env ->
    let present, absent =
      StringMap.fold
	(fun label -> function
	   | `Present t -> (fun (present_fields, absent_fields) -> (label, t) :: present_fields, absent_fields)
	   | `Absent -> (fun (present_fields, absent_fields) -> present_fields, label :: absent_fields)) field_env ([], [])
    in
      if map_fold_increasing then
        List.rev present, List.rev absent 
      else 
        present, absent
	
let get_present_fields field_env = fst (split_fields field_env)
let get_absent_fields field_env = snd (split_fields field_env)

let string_type = `List (`Primitive `Char)
let xml = `List (`Primitive `XMLitem)

(* Type printers *)
let string_of_primitive : primitive -> string = function
  | `Bool -> "Bool"  | `Int -> "Int"  | `Char -> "Char"  | `Float   -> "Float"  | `XMLitem -> "XMLitem" | `Abstract s -> s

exception Not_tuple

let rec string_of_datatype' vars : datatype -> string =
  let is_tuple (field_env, _) =
    let present_fields, absent_fields = split_fields field_env in
      match absent_fields with
	| [] ->
	    (* 0/1-tuples are displayed as records *)
	    List.length present_fields > 1 &&
	      (* check that the labels are numbers 1..n *)
	      (List.fold_left
		 (fun index (label, _) ->
		    if index > 0 && (String.compare (string_of_int index) label)=0 then
		      index+1
		    else
		      0) 1 present_fields) <> 0
	| _ -> false in
  let string_of_tuple (field_env, row_var) =
    match row_var with
      | `RecRowVar _ -> assert(false)
      | `RowVar row_var ->
	  let present_fields = get_present_fields field_env in
	  let row_var_string = match row_var with
	    |	Some var -> [string_of_int var]
	    | None -> [] in
	  let strings = (List.map (fun (_, t) -> string_of_datatype' vars t) present_fields) @ row_var_string in
	    "(" ^ String.concat ", " strings ^ ")" in
    function
      | `Not_typed       -> "not typed"
      | `Primitive p     -> string_of_primitive p
      | `TypeVar var      -> IntMap.find var vars
      | `Function (`Record _ as f,t) -> string_of_datatype' vars f ^ " -> " ^ string_of_datatype' vars t
      | `Function (f,t)  -> "(" ^ string_of_datatype' vars f ^ ") -> " ^ string_of_datatype' vars t
      | `Record row      -> (if is_tuple row then string_of_tuple row
			     else
			       "(" ^ string_of_row' "," vars row ^ ")")
      | `Variant row    -> "[|" ^ string_of_row' " | " vars row ^ "|]"
      | `Recursive (var, body) ->
	  "mu " ^ IntMap.find var vars ^ " . " ^ string_of_datatype' vars body
      | `DB             ->                   "Database"
      | `List (`Primitive `Char) -> "String"
      | `List (`Primitive `XMLitem) -> "XML"
      | `List (elems)           ->  "["^ string_of_datatype' vars elems ^"]"
      | `Mailbox (msg)           ->  "Mailbox ("^ string_of_datatype' vars msg ^")"
and string_of_row' sep vars (field_env, row_var) =
  let present_fields, absent_fields = split_fields field_env in
  let present_strings = List.map (fun (label, t) -> label ^ ":" ^ string_of_datatype' vars t) present_fields in
  let absent_strings = List.map (fun label -> label ^ " -") absent_fields in
  let row_var_string = match row_var with
      |	`RowVar (Some var) -> [string_of_int var]
      | `RowVar None -> []
      | `RecRowVar (var, row) -> 
	  ["(mu " ^ string_of_int var ^ " . " ^ string_of_row' sep vars row ^ ")"] in
    String.concat sep (present_strings @ absent_strings @ row_var_string)
(*
  String.concat sep (map (function
			    | `Row_variable var -> string_of_int var
			    | `Field_present (label, datatype) -> label ^":"^ string_of_datatype' vars datatype
			    | `Field_absent label -> label ^ " -")
		       fields)
*)

(*let string_of_datatype = string_of_datatype' []
and string_of_row = string_of_row' []*)

(* Making readable names for printing type variables.  (This can't be
   done at generation time because we don't have the whole type then.) *)
(*
let letters =
  ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m"; 
   "n"; "o"; "p"; "q"; (*"r";*) "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z"]

let rec next_name = function
  | suffix, [] -> next_name (suffix + 1, letters)
  | 0, name :: names -> name, names, 0
  | suffix, name :: names -> (name ^ string_of_int suffix), names, suffix

let make_names items =
  let rec aux (suffix, names) = function
    | item :: items ->
	let name, names, suffix = next_name (suffix, names) in
          (item, name) :: aux (suffix, names) items
    | [] -> []
  in aux (0, letters) items       
*)

let make_names vars =
  let first_letter = int_of_char 'a' in
  let last_letter = int_of_char 'z' in
  let num_letters = last_letter - first_letter + 1 in
    
  let string_of_ascii n = Char.escaped (char_of_int n) in

  let rec num_to_letters n =
    let letter = string_of_ascii (first_letter + (n mod num_letters)) in
      letter ^
	(if n >= num_letters then (num_to_letters (n / num_letters))
	 else "")
  in
    
  let (_, name_map) = 
    IntSet.fold (fun var (n, name_map) -> (n+1, IntMap.add var (num_to_letters n) name_map)) vars (0, IntMap.empty)
  in
    name_map

(* [TODO]
      change the return type to be IntSet.t
*)
let rec type_vars : datatype -> int list = fun datatype ->
  let rec aux = function
    | `Not_typed               -> []
    | `Primitive _             -> []
    | `TypeVar var             -> [var]
    | `Function (from, into)   -> aux from @ aux into
    | `Record row              -> row_type_vars row
    | `Variant row             -> row_type_vars row
    | `Recursive (var, body)   -> List.filter ((<>) var) (aux body)
    | `List (datatype)             -> aux datatype
    | `Mailbox (datatype)          -> aux datatype
    | `DB                      -> []
  in unduplicate (=) (aux datatype)
and row_type_vars (field_env, row_var) =
  let field_type_vars =
    List.concat (List.map (fun (_, t) -> type_vars t) (get_present_fields field_env)) in

  let row_var =
      match row_var with
	| `RowVar (Some var) -> [var]
	| `RowVar None -> []
	| `RecRowVar (var, row) -> List.filter ((<>) var) (row_type_vars row)
  in
    field_type_vars @ row_var

let rec free_bound_type_vars : datatype -> IntSet.t = function
  | `Not_typed               -> IntSet.empty
  | `Primitive _             -> IntSet.empty
  | `TypeVar var             -> IntSet.singleton var
  | `Function (from, into)   -> IntSet.union (free_bound_type_vars from) (free_bound_type_vars into)
  | `Record row              -> free_bound_row_type_vars row
  | `Variant row             -> free_bound_row_type_vars row
  | `Recursive (var, body)   -> IntSet.add var (free_bound_type_vars body)
  | `List (datatype)             -> free_bound_type_vars datatype
  | `Mailbox (datatype)          -> free_bound_type_vars datatype
  | `DB                      -> IntSet.empty

and free_bound_row_type_vars (field_env, row_var) =
  let field_type_vars = 
    List.fold_right IntSet.union
      (List.map (fun (_, t) -> free_bound_type_vars t) (get_present_fields field_env))
      IntSet.empty
  in
  let row_var = match row_var with
    | `RowVar (Some var) -> IntSet.singleton var
    | `RowVar None -> IntSet.empty
    | `RecRowVar (var, row) -> IntSet.add var (free_bound_row_type_vars row)
  in
    IntSet.union field_type_vars row_var

(* string conversions *)
let string_of_datatype (datatype : datatype) = 
  string_of_datatype' (make_names (free_bound_type_vars datatype)) datatype

let string_of_datatype_raw datatype = 
  string_of_datatype' (IntSet.fold
		     (fun var name_map -> IntMap.add var (string_of_int var) name_map)
		     (free_bound_type_vars datatype) IntMap.empty) datatype

let string_of_row row = 
  string_of_row' "," (make_names (free_bound_row_type_vars row)) row

let string_of_quantifier = function
  | `TypeVar var -> string_of_int var
  | `RowVar var -> "'" ^ string_of_int var
let string_of_assumption = function
  | [], datatype -> string_of_datatype datatype
  | assums, datatype -> "forall " ^ (String.concat ", " (List.map string_of_quantifier assums)) ^" . "^ string_of_datatype datatype
let string_of_environment env =
  "{ " ^ (String.concat " ; " (List.map (fun (f, s) -> f ^" : " ^ string_of_assumption s) env)) ^" }"

(* serialisation *) 
let (serialise_primitive : primitive serialiser), 
    (deserialise_primitive : primitive deserialiser)
  = enumeration_serialisers [`Bool, 'b';  `Int, 'i';  `Char, 'c';  `Float, 'f';  `XMLitem, 'x']

let rec serialise_datatype : datatype serialiser = 
  function
    | `Not_typed       -> serialise0 'a' () ()
    | `Primitive v     -> serialise1 'b' (serialise_primitive) v
    | `TypeVar v       -> serialise1 'c' (serialise_oint) v
    | `Function v      -> serialise2 'd' (serialise_datatype, serialise_datatype) v
    | `Record v        -> serialise_row 'e' v
    | `Variant v       -> serialise_row 'f' v
    | `Recursive v     -> serialise2 'g' (serialise_oint, serialise_datatype) v
    | `List v          -> serialise1 'h' (serialise_datatype) v
    | `Mailbox v       -> serialise1 'm' (serialise_datatype) v
    | `DB              -> serialise0 'i' () ()
and serialise_field_spec : field_spec serialiser =
  function
    | `Absent -> serialise0 'a' () ()
    | `Present v -> serialise1 'b' (serialise_datatype) v
and serialise_row_var : row_var serialiser = 
  function
    | `RowVar v        -> serialise1 'a' (serialise_option (serialise_oint)) v
    | `RecRowVar v     -> serialise2 'b' (serialise_oint, serialise_row 'a') v
and serialise_row : char -> row serialiser = fun t -> serialise2 t
  ((serialise_list
     (serialise2 'a' (serialise_string, serialise_field_spec))) -<- assoc_list_of_string_map,
   serialise_row_var)
  

and deserialise_datatype : datatype deserialiser =
  fun s ->
    let t, obj, rest = extract_object s in
    let r = 
      (match t with
         | 'a'        -> (deserialise0 () obj); `Not_typed
         | 'b'        -> `Primitive (deserialise1 (deserialise_primitive) obj)
         | 'c'        -> `TypeVar (deserialise1 (deserialise_oint) obj)
         | 'd'        -> `Function (deserialise2 (deserialise_datatype, deserialise_datatype) obj)
         | 'e'        -> `Record (fst (deserialise_row obj))
         | 'f'        -> `Variant (fst (deserialise_row obj))
	 | 'g'        -> `Recursive (deserialise2 (deserialise_oint, deserialise_datatype) obj)
         | 'h'        -> `List (deserialise1 (deserialise_datatype) obj)
         | 'm'        -> `Mailbox (deserialise1 (deserialise_datatype) obj)
         | 'i'        -> (deserialise0 () obj); `DB
         | _          -> failwith ("Unexpected character deserialising datatype : " ^ String.make 1 t))
    in r, rest
and deserialise_field_spec : field_spec deserialiser =
  fun s ->
    let t, obj, rest = extract_object s in
    let r =
      (match t with
	 | 'a' -> `Absent
	 | 'b' -> `Present (fst (deserialise_datatype obj))
         | c   -> invalid_header "field_spec" c)
    in
      r, rest
and deserialise_row_var : row_var deserialiser =
  fun s ->
    let t, obj, rest = extract_object s in
    let r =
      (match t with
	 | 'a' -> `RowVar (deserialise1 (deserialise_option (deserialise_oint)) obj)
	 | 'b' -> `RecRowVar (deserialise2 (deserialise_oint, deserialise_row) obj)
         | c   -> invalid_header "row_var" c)
    in
      r, rest
and deserialise_row : row deserialiser =
  fun s ->
    let _, obj, rest = extract_object s in
    let r =
      deserialise2
	((fun (r, rest) -> string_map_of_assoc_list r, rest) -<-
	   (deserialise_list (deserialiser2 (deserialise_string, deserialise_field_spec)))
	,deserialise_row_var) obj
    in
      r, rest

let serialise_quantifier : quantifier serialiser
    = function
      | `TypeVar i -> serialise1 't' serialise_oint i
      | `RowVar i -> serialise1 'r' serialise_oint i

let deserialise_quantifier : quantifier deserialiser
    = fun s -> let t, obj, rest = extract_object s in
      match t with 
      | 't' -> `TypeVar (deserialise1 deserialise_oint obj), rest
      | 'r' -> `RowVar (deserialise1 deserialise_oint obj), rest
      | c   -> invalid_header "quantifier" c

let serialise_assumption : assumption serialiser 
    = serialise2 'a' (serialise_list (serialise_quantifier), serialise_datatype)

let deserialise_assumption : assumption deserialiser
    = fun s ->
      let t, obj, rest = extract_object s in
      match t with
          | 'a'  -> deserialise2 (deserialise_list (deserialise_quantifier), deserialise_datatype) obj, rest
          | _          -> failwith ("Unexpected character deserialising assumption : " ^ String.make 1 t)

let serialise_environment : environment serialiser =
  serialise1 'a' (serialise_list (serialise2 'a' (serialise_string, serialise_assumption)))

let deserialise_environment : environment deserialiser =
  let deserialise_binding : (string * assumption) deserialiser = 
    fun s ->
      let t, obj, rest = extract_object s in
        match t with
          | 'a' -> deserialise2 (deserialise_string, deserialise_assumption) obj, rest
          | _          -> failwith ("Unexpected character deserialising binding : " ^ String.make 1 t)
  in
    fun s ->
      let t, obj, rest = extract_object s in
        match t with
          | 'a' -> deserialise1 (deserialise_list (deserialise_binding)) obj, rest
          | _          -> failwith ("Unexpected character deserialising environment : " ^ String.make 1 t)

module BasicTypeOps :
  (Type_basis.BASICTYPEOPS
   with type typ = datatype
   and type row_var' = row_var) =
struct
  type typ = datatype
  type row_var' = row_var

  type field_spec = typ field_spec_basis
  type field_spec_map = typ field_spec_map_basis
  type row = (typ, row_var') row_basis

  let make_type_variable var = `TypeVar var
  let make_row_variable var = `RowVar (Some var)

  let empty_field_env = StringMap.empty
  let closed_row_var = `RowVar None

  let is_closed_row = function
    | (_, `RowVar (Some _)) -> false
    | (_, `RowVar None) -> true
    | (_, `RecRowVar _) -> true

  let get_row_var (_, row_var) =
    let rec get_row_var' rec_vars = function
      | `RowVar None -> None
      | `RowVar (Some var) -> Some var
      | `RecRowVar (var, (_, row_var')) ->
	  if IntSet.mem var rec_vars then
	    None
	  else
	    get_row_var' (IntSet.add var rec_vars) row_var'
    in
      get_row_var' IntSet.empty row_var
end

module TypeOps :
  (Type_basis.TYPEOPS
   with type typ = datatype
   and type row_var = row_var) = TypeOpsGen(BasicTypeOps)

let unit_type = `Record (TypeOps.make_empty_closed_row ())

(* fresh type_variable * type *)
let fresh_type () =
  let var = fresh_raw_variable () in
    `TypeVar var, `TypeVar var

(* fresh type_variable * row *)
let fresh_row () =
  let var = fresh_raw_variable () in
    `RowVar var, TypeOps.make_empty_open_row_with_var var


(* rewriting for types *)
let perhaps_process_children (f : datatype -> datatype option) :  datatype -> datatype option =
  let rewrite_row (fields, r) = 
    match (StringMap.fold
             (fun name field (changed, row) ->
                match field with
                  | `Present k -> (match f k with
                                     | Some k -> true,    TypeOps.set_field (name, `Present k) row
                                     | None   -> changed, TypeOps.set_field (name, `Present k) row)
                  | `Absent    -> changed, TypeOps.set_field (name, `Absent) row)
             fields
             (false, (BasicTypeOps.empty_field_env, r))) with
      | true, row -> Some row
      | false, _  -> None in
    function
        (* no children *)
      | `Not_typed
      | `Primitive _
      | `DB
      | `TypeVar  _ -> None
          (* one child *)
      | `Recursive (v, k) -> (match f k with 
                                | Some k -> Some (`Recursive (v, k))
                                | None   -> None)
      | `List k ->          (match f k with 
                               | Some k -> Some (`List k)
                               | None   -> None)
      | `Mailbox k ->       (match f k with 
                               | Some k -> Some (`Mailbox k)
                               | None   -> None)
          (* two children *)
      | `Function (j, k) -> (match f j, f k with
                               | None,   None   -> None
                               | Some j, None   -> Some (`Function (j, k))
                               | None,   Some k -> Some (`Function (j, k))
                               | Some j, Some k -> Some (`Function (j, k)))
          (* n children *)
      | `Record row  -> (match rewrite_row row with 
                           | Some row -> Some (`Record row)
                           | None ->     None)
      | `Variant row -> (match rewrite_row row with 
                           | Some row -> Some (`Variant row)
                           | None ->     None)
