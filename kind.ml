(* Representations of types *)

open List

open Pickle
open Utility

type type_var_set = IntSet.t


(* Types for kinds *)

type primitive = [ `Bool | `Int | `Char | `Float | `XMLitem ]

type collection_type = [`Set | `Bag | `List | `CtypeVar of int]

type ('typ, 'row, 'ctype) type_basis = [
  | `Not_typed
  | `Primitive of primitive
  | `TypeVar of int
  | `Function of ('typ * 'typ)
  | `Record of 'row
  | `Variant of 'row
  | `Recursive of (int * 'typ)
  | `Collection of ('ctype * 'typ)
  | `DB ]

type 'typ field_spec_basis = [ `Present of 'typ | `Absent ]
type 'typ field_spec_map_basis = ('typ field_spec_basis) StringMap.t
type ('typ, 'row_var) row_basis = 'typ field_spec_map_basis * 'row_var 
type 'row row_var_basis =
    [ `RowVar of int option 
    | `RecRowVar of int * 'row ]

type kind = (kind, row, collection_type) type_basis
and field_spec = kind field_spec_basis
and field_spec_map = kind field_spec_map_basis
and row_var = row row_var_basis
and row = (kind, row_var) row_basis

			  
type equivalence =   Var_equiv of (int * kind) 
                   | Row_equiv of (int * row)
                   | Colltype_equiv of (int * collection_type)
type substitution = (equivalence list)


type type_variable = [`TypeVar of int | `RowVar of int | `CtypeVar of int]
type quantifier = type_variable

type 'typ assumption_basis = ((quantifier list) * 'typ)
type 'typ environment_basis = ((string * 'typ assumption_basis) list)

type assumption = kind assumption_basis
type environment = kind environment_basis

let (-->) x y = `Function (x,y)

let get_equivalence_variable = function
  | Var_equiv (var, _) -> var
  | Row_equiv (var, _) -> var
  | Colltype_equiv (var, _) -> var

(* remove a variable from a type substitution *)
let restrict_substitution substs var = List.filter (fun equiv -> (get_equivalence_variable equiv) <> var) substs


(* lookup the type of type variable from a type substitution *)
(*
let lookup_substitution_kind var = function
  | (Var_equiv (id, kind)) :: subst when id=var -> Some kind
  | equiv :: subst -> lookup_substitution_kind var subst
  | [] -> None
*)

(*
let is_rowvar = function
  | `Row_variable _ -> true
  | _              -> false
and fields_present : field list -> (string * kind) list = fold_left 
  (fun labels -> function 
     | `Row_variable _ | `Field_absent _ -> labels
     | `Field_present s -> s :: labels) []
*)

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

let string = `Collection (`List, `Primitive `Char)
let xml = `Collection (`List, `Primitive `XMLitem)

(* Type printers *)
let coll_name : collection_type -> string = function
  | `Set          -> "Set"
  | `Bag          -> "Bag"
  | `List         -> "List"
  | `CtypeVar id  -> "{"^ string_of_int id ^"}"
and coll_prefix var_names : collection_type -> string = function
  | `Set          -> "Set"
  | `Bag          -> "Bag"
  | `List         -> ""
  | `CtypeVar id  -> "{"^ IntMap.find id var_names ^"}"
and string_of_primitive : primitive -> string = function
  | `Bool -> "Bool"  | `Int -> "Int"  | `Char -> "Char"  | `Float   -> "Float"  | `XMLitem -> "XMLitem"

exception Not_tuple

let rec string_of_kind' vars : kind -> string =
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
	  let strings = (List.map (fun (_, t) -> string_of_kind' vars t) present_fields) @ row_var_string in
	    "(" ^ String.concat ", " strings ^ ")" in
    function
      | `Not_typed       -> "not typed"
      | `Primitive p     -> string_of_primitive p
      | `TypeVar id      -> IntMap.find id vars
      | `Function (`Record _ as f,t) -> string_of_kind' vars f ^ " -> " ^ string_of_kind' vars t
      | `Function (f,t)  -> "(" ^ string_of_kind' vars f ^ ") -> " ^ string_of_kind' vars t
      | `Record row      -> (if is_tuple row then string_of_tuple row
			     else
			       "(" ^ string_of_row' "," vars row ^ ")")
      | `Variant row    -> "[" ^ string_of_row' " | " vars row ^ "]"
      | `Recursive (id, body) ->
	  "mu " ^ IntMap.find id vars ^ " . " ^ string_of_kind' vars body
      | `DB             ->                   "Database"
      | `Collection (`List, `Primitive `Char) -> "String"
      | `Collection (`List, `Primitive `XMLitem) -> "XML"
      | `Collection (coll_type, elems)           ->  coll_prefix vars coll_type ^"["^ string_of_kind' vars elems ^"]"
and string_of_row' sep vars (field_env, row_var) =
  let present_fields, absent_fields = split_fields field_env in
  let present_strings = List.map (fun (label, t) -> label ^ ":" ^ string_of_kind' vars t) present_fields in
  let absent_strings = List.map (fun label -> label ^ " -") absent_fields in
  let row_var_string = match row_var with
      |	`RowVar (Some var) -> [string_of_int var]
      | `RowVar None -> []
      | `RecRowVar _ -> ["recrowvar"] in
    String.concat sep (present_strings @ absent_strings @ row_var_string)
(*
  String.concat sep (map (function
			    | `Row_variable id -> string_of_int id
			    | `Field_present (label, kind) -> label ^":"^ string_of_kind' vars kind
			    | `Field_absent label -> label ^ " -")
		       fields)
*)

(*let string_of_kind = string_of_kind' []
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

let rec type_vars items = let rec aux = function
  | `Not_typed               -> []
  | `Primitive _             -> []
  | `TypeVar var             -> [var]
  | `Function (from, into)   -> aux from @ aux into
  | `Record row              -> row_type_vars row
  | `Variant row             -> row_type_vars row
  | `Recursive (id, body)    -> List.filter ((<>) id) (aux body)
  | `Collection (`CtypeVar id, kind)    -> id :: aux kind
  | `Collection (_, kind)    -> aux kind
  | `DB                      -> []
in unduplicate (=) (aux items)
and row_type_vars (field_env, `RowVar row_var) =
  let field_type_vars = List.concat (List.map (fun (_, t) -> type_vars t) (get_present_fields field_env)) in
  let row_var = match row_var with
    | Some var -> [var]
    | None -> []
  in
    field_type_vars @ row_var

let rec free_bound_type_vars = function
  | `Not_typed               -> IntSet.empty
  | `Primitive _             -> IntSet.empty
  | `TypeVar var             -> IntSet.singleton var
  | `Function (from, into)   -> IntSet.union (free_bound_type_vars from) (free_bound_type_vars into)
  | `Record row              -> free_bound_row_type_vars row
  | `Variant row             -> free_bound_row_type_vars row
  | `Recursive (id, body)    -> IntSet.add id (free_bound_type_vars body)
  | `Collection (`CtypeVar var, kind) -> IntSet.add var (free_bound_type_vars kind)
  | `Collection (_, kind)    -> free_bound_type_vars kind
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


let string_of_kind kind = 
  string_of_kind' (make_names (free_bound_type_vars kind)) kind

let string_of_kind_raw kind = 
  string_of_kind' (IntSet.fold
		     (fun id name_map -> IntMap.add id (string_of_int id) name_map)
		     (free_bound_type_vars kind) IntMap.empty) kind

let string_of_row row = 
  string_of_row' "," (make_names (free_bound_row_type_vars row)) row
(*  string_of_row' "," (make_names (List.concat (map field_type_vars row))) row *)

(*
let string_of_equivalence = function
  | Var_equiv (id, kind)       -> "Var_equiv '"^ string_of_int id ^" => "^ string_of_kind kind
  | Row_equiv (id, fields)     -> "Row_equiv '"^ string_of_int id ^ " => { "^ string_of_row fields ^" }"
  | Colltype_equiv (id, ctype) -> "Colltype_equiv '"^ string_of_int id ^" => "^ coll_name ctype
let string_of_substitution = fun substs ->
  "{ "^ String.concat " , " (map string_of_equivalence substs) ^" }"
*)

let string_of_quantifier = function
  | `TypeVar id -> string_of_int id
  | `RowVar id -> "'" ^ string_of_int id
  | `CtypeVar id -> "`" ^ string_of_int id
let string_of_assumption = function
  | [], kind -> string_of_kind kind
  | assums, kind -> "forall " ^ (String.concat ", " (map string_of_quantifier assums)) ^" . "^ string_of_kind kind
let string_of_environment env =
  "{ " ^ (String.concat " ; " (map (fun (f, s) -> f ^" : " ^ string_of_assumption s) env)) ^" }"

(* serialisation *) 
let serialise_colltype : (collection_type serialiser) = function
  | `Set -> "s" | `Bag -> "b" | `List -> "l" | `CtypeVar _ -> "c"
and deserialise_colltype : (collection_type deserialiser)
    = fun s -> (assoc (String.sub s 0 1) ["s", `Set; "b", `Bag; "l", `List; "c", `CtypeVar (-1)],
                String.sub s 1 (String.length s - 1))

let (serialise_primitive : primitive serialiser), 
    (deserialise_primitive : primitive deserialiser)
  = enumeration_serialisers [`Bool, 'b';  `Int, 'i';  `Char, 'c';  `Float, 'f';  `XMLitem, 'x']

let rec serialise_kind : kind serialiser = 
  function
    | `Not_typed       -> serialise0 'a' () ()
    | `Primitive v     -> serialise1 'b' (serialise_primitive) v
    | `TypeVar v       -> serialise1 'c' (serialise_oint) v
    | `Function v      -> serialise2 'd' (serialise_kind, serialise_kind) v
    | `Record v        -> failwith "serialise_kind: not implemented serialisation for rows yet"
    | `Variant v       -> failwith "serialise_kind: not implemented serialisation for rows yet"
    | `Recursive v     -> serialise2 'g' (serialise_oint, serialise_kind) v
    | `Collection v    -> serialise2 'h' (serialise_colltype, serialise_kind) v
    | `DB              -> serialise0 'i' () ()
(*
and serialise_row : row serialiser = 
  function
    | (`RowVar row_var, field_env) ->
	let present_fields, absent_fields = split_fields field_env in
	let 
*)
and deserialise_kind : kind deserialiser =
  fun s ->
    let t, obj, rest = extract_object s in
    let r = 
      (match t with
         | 'a'        -> (deserialise0 () obj); `Not_typed
         | 'b'        -> `Primitive (deserialise1 (deserialise_primitive) obj)
         | 'c'        -> `TypeVar (deserialise1 (deserialise_oint) obj)
         | 'd'        -> `Function (deserialise2 (deserialise_kind, deserialise_kind) obj)
(*
         | 'e'        -> `Record (deserialise1 (deserialise_list (deserialise_field)) obj)
         | 'f'        -> `Variant (deserialise1 (deserialise_list (deserialise_field)) obj)
*)
         | 'e'        -> failwith "deserialise_kind: not implemented deserialisation for rows yet"
         | 'f'        -> failwith "deserialise_kind: not implemented deserialisation for rows yet"
	 | 'g'        -> `Recursive (deserialise2 (deserialise_oint, deserialise_kind) obj)
         | 'h'        -> `Collection (deserialise2 (deserialise_colltype, deserialise_kind) obj)
         | 'i'        -> (deserialise0 () obj); `DB
         | _          -> failwith ("Unexpected character deserialising kind : " ^ String.make 1 t))
    in r, rest
(*
and deserialise_field : field deserialiser =
  fun s ->
    let t, obj, rest = extract_object s in
    let r = 
      (match t with
         | 'a'        -> `Row_variable (deserialise1 (deserialise_oint) obj)
         | 'b'        -> `Field_present (deserialise2 (deserialise_string, deserialise_kind) obj)
         | 'c'        -> `Field_absent (deserialise1 (deserialise_string) obj)
         | _          -> failwith ("Unexpected character deserialising field : " ^ String.make 1 t))
    in r, rest
*)

let serialise_quantifier : quantifier serialiser
    = function
      | `TypeVar i -> serialise1 't' serialise_oint i
      | `RowVar i -> serialise1 'r' serialise_oint i
      | `CtypeVar i -> serialise1 'c' serialise_oint i

let deserialise_quantifier : quantifier deserialiser
    = fun s -> let t, obj, rest = extract_object s in
      match t with 
      | 't' -> `TypeVar (deserialise1 deserialise_oint obj), rest
      | 'r' -> `RowVar (deserialise1 deserialise_oint obj), rest
      | 'c' -> `CtypeVar (deserialise1 deserialise_oint obj), rest

let serialise_assumption : assumption serialiser 
    = serialise2 'a' (serialise_list (serialise_quantifier), serialise_kind)

let deserialise_assumption : assumption deserialiser
    = fun s ->
      let t, obj, rest = extract_object s in
      match t with
          | 'a'  -> deserialise2 (deserialise_list (deserialise_quantifier), deserialise_kind) obj, rest
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

(* Generation of fresh type variables *)
let type_variable_counter = ref 0

let new_raw_variable : unit -> int =
  function () -> 
    incr type_variable_counter; !type_variable_counter

module type TYPEOPS =
sig
  type typ
  type row_var
  type collection_type

  type field_spec = typ field_spec_basis
  type field_spec_map = typ field_spec_map_basis
  type row = (typ, row_var) row_basis

  val make_type_var : int -> typ
  val make_row_var : int -> row_var
  val make_collection_var : int -> collection_type

  (* fresh type variable generation *)
  val new_type_variable : unit -> typ
  val new_row_variable : unit -> row_var
  val new_collection_variable : unit -> collection_type

  (* empty row constructors *)
  val make_empty_closed_row : unit -> row
  val make_empty_open_row : unit -> row
  val make_empty_open_row_with_var : int -> row

  (* singleton row constructors *)
  val make_singleton_closed_row : (string * field_spec) -> row
  val make_singleton_open_row : (string * field_spec) -> row
  val make_singleton_open_row_with_var : (string * field_spec) -> int -> row

  (* empty record constructors *)
(*
  val make_unit : unit -> typ
  val make_empty_record_with_row_var : int -> typ
*)

  (* row predicates *)
  val is_closed_row : row -> bool
  val is_absent_from_row : string -> row -> bool

  (* row update *)
  val set_field : (string * field_spec) -> row -> row

  (* constants *)
  val empty_field_env : typ field_spec_map_basis
  val closed_row_var : row_var
end

module type BASICTYPEOPS =
sig
  type typ
  type row_var'
  type collection_type'
 
  type field_spec = typ field_spec_basis
  type field_spec_map = typ field_spec_map_basis
  type row = (typ, row_var') row_basis

  val make_type_var : int -> typ
  val make_row_var : int -> row_var'
  val make_collection_var : int -> collection_type'

  val empty_field_env : typ field_spec_map_basis
  val closed_row_var : row_var'

  val is_closed_row : row -> bool
end

module BasicTypeOps :
  (BASICTYPEOPS with type typ = kind
		and type row_var' = row_var
		and type collection_type' = collection_type) =
struct
  type typ = kind
  type row_var' = row_var
  type collection_type' = collection_type

  type field_spec = typ field_spec_basis
  type field_spec_map = typ field_spec_map_basis
  type row = (typ, row_var') row_basis

  let make_type_var var = `TypeVar var
  let make_row_var var = `RowVar (Some var)
  let make_collection_var var = `CtypeVar var

  let empty_field_env = StringMap.empty
  let closed_row_var = `RowVar None

  let is_closed_row = function
    | (_, `RowVar (Some _)) -> false
    | (_, `RowVar None) -> true
    | (_, `RecRowVar _) -> true
end

module TypeOpsGen(BasicOps: BASICTYPEOPS) :
  (TYPEOPS
   with type typ = BasicOps.typ 
   and type row_var = BasicOps.row_var'
   and type collection_type = BasicOps.collection_type'
) =
struct
  type typ = BasicOps.typ
  type row_var = BasicOps.row_var'
  type collection_type = BasicOps.collection_type'

  type field_spec = BasicOps.field_spec
  type field_spec_map = BasicOps.field_spec_map
  type row = BasicOps.row


  let make_type_var = BasicOps.make_type_var
  let make_row_var = BasicOps.make_row_var
  let make_collection_var = BasicOps.make_collection_var

  let is_closed_row = BasicOps.is_closed_row

  let new_type_variable = make_type_var @@ new_raw_variable
  let new_row_variable = make_row_var @@ new_raw_variable
  let new_collection_variable = make_collection_var @@ new_raw_variable

  let empty_field_env = BasicOps.empty_field_env
  let closed_row_var = BasicOps.closed_row_var

  let make_empty_closed_row () = empty_field_env, closed_row_var
  let make_empty_open_row () = empty_field_env, new_row_variable ()
  let make_empty_open_row_with_var var = empty_field_env, make_row_var var

  let make_singleton_closed_row (label, field_spec) =
    StringMap.add label field_spec empty_field_env, closed_row_var
  let make_singleton_open_row (label, field_spec) =
    StringMap.add label field_spec empty_field_env, new_row_variable ()
  let make_singleton_open_row_with_var (label, field_spec) var =
    StringMap.add label field_spec empty_field_env, make_row_var var

  let is_absent_from_row label =
    function
      | (field_env, row_var) as row ->
	  if StringMap.mem label field_env then
	    StringMap.find label field_env = `Absent
	  else
	    is_closed_row row

  let set_field (label, f) ((field_env, row_var) as row) =
    StringMap.add label f field_env, row_var
end

module TypeOps :
  (TYPEOPS with type typ = kind
	   and type row_var = row_var
	   and type collection_type = collection_type) = TypeOpsGen(BasicTypeOps)

let make_unit () = `Record (TypeOps.make_empty_closed_row ())
let make_empty_record_with_row_var var = `Record (TypeOps.make_empty_open_row_with_var var)

(* From library.ml; there's probably another name for these *)
let fresh_type_variable () = `TypeVar (new_raw_variable ())
let fresh_row_variable () = `RowVar (new_raw_variable ())
let fresh_collection_variable () = `CtypeVar (new_raw_variable ())

(* Functions on environments *)
let lookup : string -> 'typ environment_basis -> 'typ assumption_basis = assoc
