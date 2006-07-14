(* Representations of types *)

open Debug
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

(* whether to display mailbox annotations on arrow types
   [NOTE]
      unused mailbox parameters are never shown
 *)
let show_mailbox_annotations = Settings.add_bool("show_mailbox_annotations", true, true)

(* pretty-print type vars as raw numbers rather than letters *)
let show_raw_type_vars = Settings.add_bool("show_raw_type_vars", false, true)

(*
  [HACK]
  used to temporarily disable mailbox typing for two-pass type-checking
*)
let use_mailbox_typing = ref true

(* return true if mailbox typing is currently switched on *)
let using_mailbox_typing () = !use_mailbox_typing

(* call f()
   with mailbox typing switched on or off according to
   the value of b
   (guarantees that the state of mailbox typing is
   restored afterwards)
*)
let with_mailbox_typing b f =
  let oldb = using_mailbox_typing ()
  in
    try
      use_mailbox_typing := b;
      let result = f() in
	use_mailbox_typing := oldb;
	result
    with e ->
      use_mailbox_typing := oldb;
      raise e

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
  | `Bool -> "Bool"  | `Int -> "Int"  | `Char -> "Char"  | `Float   -> "Float"  
  | `XMLitem -> "XMLitem" | `Abstract s -> s

exception Not_tuple

let rec string_of_datatype' : string IntMap.t -> datatype -> string = fun vars datatype ->
  let sd = string_of_datatype' vars in

  let string_of_mailbox_arrow mailbox_type =
    begin
      if Settings.get_value(show_mailbox_annotations) then
	"-{" ^ sd mailbox_type ^ "}->"
      else
	"->"
    end in

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
	    | Some var -> Some (IntMap.find var vars)
	    | None -> None in
	  let strings = (List.map (fun (_, t) -> string_of_datatype' vars t) present_fields) in
	    "(" ^ String.concat ", " strings ^
	      (match row_var_string with
		 | Some s -> " | "^s
		 | None -> "") ^ ")"
  in
    match datatype with
      | `Not_typed       -> "not typed"
      | `Primitive p     -> string_of_primitive p
      | `TypeVar var      -> IntMap.find var vars
      | `Function (mailbox_type, t) when using_mailbox_typing () ->
	  let arrow =
	    match mailbox_type with
	      | `Mailbox t ->
		  string_of_mailbox_arrow (t)
	      | _ ->
		  "->"
	  in
	    begin
	      match t with
		| `Function (`Record _ as f, t) ->
		    string_of_datatype' vars f ^ " " ^arrow ^
		      " " ^ string_of_datatype' vars t
		| `Function (f, t) ->
		    "(" ^ string_of_datatype' vars f ^ ") "^ arrow ^
		      " " ^ string_of_datatype' vars t
		| _ -> (*assert(false)*)
		    begin
		      let f = mailbox_type in
			debug ("non-mailbox function in mailbox type: pretending it didn't happen!");
			match mailbox_type with
			  | `Record _ ->
			      string_of_datatype' vars f ^ " -> " ^ string_of_datatype' vars t
			  | _ ->
			      "(" ^ string_of_datatype' vars f ^ ") -> " ^ string_of_datatype' vars t	
		   end
	   end
     | `Function (f, t) ->
	 begin
	   match f with
	     | `Record _ ->
		 string_of_datatype' vars f ^ " -> " ^ string_of_datatype' vars t
	     | _ ->
		 "(" ^ string_of_datatype' vars f ^ ") -> " ^ string_of_datatype' vars t	
	 end
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
  let row_var_string = string_of_row_var' sep vars row_var in
    (String.concat sep (present_strings @ absent_strings)) ^
      (match row_var_string with
	 | None -> ""
	 | Some s -> "|"^s)
and string_of_row_var' sep vars row_var =
   match row_var with
      | `RowVar None -> None
      |	`RowVar (Some var) -> Some (IntMap.find var vars)
      | `RecRowVar (var, row) -> 
	  Some ("(mu " ^ IntMap.find var vars ^ " . " ^ string_of_row' sep vars row ^ ")")

let make_names vars =
  if Settings.get_value show_raw_type_vars then
    IntSet.fold (fun var (name_map) -> IntMap.add var (string_of_int var) name_map) vars IntMap.empty
  else
    begin
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
    end
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
(*
  [HACK]
    uncommenting this prevents unused mailbox variables from being counted
*)
(*
  | `Function (mailbox_type, `Function (from, into)) when using_mailbox_typing() ->
      let mailbox_type_vars =
	match mailbox_type with
	  | `TypeVar _ -> IntSet.empty
	  | _ -> free_bound_type_vars mailbox_type
      in
	IntSet.union mailbox_type_vars (IntSet.union (free_bound_type_vars from) (free_bound_type_vars into))
*)
  | `Function (from, into)   -> IntSet.union (free_bound_type_vars from) (free_bound_type_vars into)
  | `Record row              -> free_bound_row_type_vars row
  | `Variant row             -> free_bound_row_type_vars row
  | `Recursive (var, body)   -> IntSet.add var (free_bound_type_vars body)
  | `List (datatype)         -> free_bound_type_vars datatype
  | `Mailbox (datatype)      -> free_bound_type_vars datatype
  | `DB                      -> IntSet.empty
and free_bound_row_type_vars (field_env, row_var) =
  let field_type_vars = 
    List.fold_right IntSet.union
      (List.map (fun (_, t) -> free_bound_type_vars t) (get_present_fields field_env))
      IntSet.empty in
  let row_var = free_bound_row_var_vars row_var in
    IntSet.union field_type_vars row_var  
and free_bound_row_var_vars row_var = 
  match row_var with
    | `RowVar (Some var) -> IntSet.singleton var
    | `RowVar None -> IntSet.empty
    | `RecRowVar (var, row) -> IntSet.add var (free_bound_row_type_vars row)

(* [TODO]
    - make sure TypeVars and RowVars with clashing names are
    treated correctly
    - perhaps this function should be in sugar.ml
*)
(*
  freshen the free type variables in a type

  freshen_free_type_vars var_map datatype
     - var_map contains an initial mapping from free variables to new names
     - on exit var_map contain mappings from any additional free variables
     in datatype to their new names
*)
let freshen_free_type_vars : (int IntMap.t) ref -> datatype -> datatype = fun var_map datatype ->
  let rec freshen_datatype : IntSet.t -> datatype -> datatype = fun bound_vars t ->
    let ftv = freshen_datatype bound_vars in
    let rftv = freshen_row bound_vars in
      match t with
	| `Not_typed               
	| `Primitive _ -> t
	| `TypeVar var ->
	    if IntSet.mem var bound_vars then
	      t
	    else if IntMap.mem var !var_map then
	      `TypeVar (IntMap.find var !var_map)
	    else
	      let fresh_var = fresh_raw_variable () in
		var_map := IntMap.add var fresh_var !var_map;
		`TypeVar fresh_var
	| `Function (from, into) ->
	    `Function (ftv from, ftv into)
	| `Record row              -> `Record (rftv row)
	| `Variant row             -> `Variant (rftv row)
	| `Recursive (var, body)   -> `Recursive (var, freshen_datatype (IntSet.add var bound_vars) body)
	| `List (datatype)         -> `List (ftv datatype)
	| `Mailbox (datatype)      -> `Mailbox (ftv datatype)
	| `DB                      -> `DB
  and freshen_row bound_vars (field_env, row_var) =
    let field_env =
      StringMap.map (function
		       | `Absent -> `Absent
		       | `Present t -> `Present (freshen_datatype bound_vars t)) field_env in
    let row_var = freshen_row_var bound_vars row_var in
      (field_env, row_var)
  and freshen_row_var bound_vars row_var =
    match row_var with
      | `RowVar None -> row_var
      | `RowVar (Some var) ->
	  if IntSet.mem var bound_vars then
	    row_var
	  else if IntMap.mem var !var_map then
	    `RowVar (Some (IntMap.find var !var_map))
	  else
	    let fresh_var = fresh_raw_variable () in
	      var_map := IntMap.add var fresh_var !var_map;
	      `RowVar (Some fresh_var)
      | `RecRowVar (var, row) -> `RecRowVar (var, freshen_row (IntSet.add var bound_vars) row)
  in
    freshen_datatype IntSet.empty datatype

(* string conversions *)
let string_of_datatype (datatype : datatype) = 
  string_of_datatype' (make_names (free_bound_type_vars datatype)) datatype

let string_of_datatype_raw datatype = 
  string_of_datatype' (IntSet.fold
		     (fun var name_map -> IntMap.add var (string_of_int var) name_map)
		     (free_bound_type_vars datatype) IntMap.empty) datatype

let string_of_row row = 
  string_of_row' "," (make_names (free_bound_row_type_vars row)) row

let string_of_row_var row_var =
  match string_of_row_var' "," (make_names (free_bound_row_var_vars row_var)) row_var with
    | None -> ""
    | Some s -> s

let string_of_quantifier = function
  | `TypeVar var -> string_of_int var
  | `RowVar var -> "'" ^ string_of_int var
let string_of_assumption = function
  | [], datatype -> string_of_datatype datatype
  | assums, datatype -> "forall " ^ (String.concat ", " (List.map string_of_quantifier assums)) ^" . "^ string_of_datatype datatype
let string_of_environment env =
  "{ " ^ (String.concat " ; " (List.map (fun (f, s) -> f ^" : " ^ string_of_assumption s) env)) ^" }"

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

let tuplify types =
  let ns = List.map string_of_int (Utility.fromTo 1 (1 + List.length types)) in
    `Record 
      (List.fold_right
         (TypeOps.set_field -<- (fun (f,t) -> (f,`Present t)))
         (List.combine ns types)
         (TypeOps.make_empty_closed_row ()))

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
