open Utility

type type_var_set = Type_basis.type_var_set

type inference_type = [
  | (inference_type, inference_row, inference_collection_type) Type_basis.type_basis
  | `MetaTypeVar of inference_type Unionfind.point ]
and inference_collection_type = [
  | Kind.collection_type
  | `MetaCollectionVar of inference_collection_type Unionfind.point ]
and inference_field_spec = inference_type Type_basis.field_spec_basis
and inference_field_spec_map = inference_field_spec StringMap.t
and inference_row_var = [
  | inference_row Type_basis.row_var_basis
  | `MetaRowVar of inference_row Unionfind.point ]
and inference_row = (inference_type, inference_row_var) Type_basis.row_basis

type type_variable = Type_basis.type_variable
type quantifier = Type_basis.quantifier

let inference_string_type = `Collection (`List, `Primitive `Char)

type inference_expression = (Syntax.position * inference_type * string option (* label *)) Syntax.expression'

(* [TODO]
      change the return type of these functions to be IntSet.t
*)
let
    free_type_vars, free_row_type_vars =
  let rec free_type_vars' : type_var_set -> inference_type -> int list = fun rec_vars ->
    function
      | `Not_typed               -> []
      | `Primitive _             -> []
      | `TypeVar var             ->
	  if IntSet.mem var rec_vars then
	    []
	  else
	    [var]
      | `Function (from, into)   -> free_type_vars' rec_vars from @ free_type_vars' rec_vars into
      | `Record row              -> free_row_type_vars' rec_vars row
      | `Variant row             -> free_row_type_vars' rec_vars row
      | `Recursive (var, body)    ->
	  if IntSet.mem var rec_vars then
	    []
	  else
	    free_type_vars' (IntSet.add var rec_vars) body
      | `Collection (`CollectionTypeVar var, kind)             -> var :: free_type_vars' rec_vars kind
      | `Collection (`MetaCollectionVar point, kind) -> free_type_vars' rec_vars (`Collection (Unionfind.find point, kind))
      | `Collection (_, kind)    -> free_type_vars' rec_vars kind
      | `DB                      -> []
      | `MetaTypeVar point       -> free_type_vars' rec_vars (Unionfind.find point)
  and free_row_type_vars' : type_var_set -> inference_row -> int list = 
    fun rec_vars (field_env, row_var) ->
      let field_vars = List.concat (List.map (fun (_, t) -> free_type_vars' rec_vars t) (Kind.get_present_fields field_env)) in
      let row_vars =
        match row_var with
	  | `RowVar (Some var) -> [var]
	  | `RowVar None -> []
	  | `RecRowVar (var, row) -> if IntSet.mem var rec_vars then
	      []
	    else
              (free_row_type_vars' (IntSet.add var rec_vars) row)
	  | `MetaRowVar point -> free_row_type_vars' rec_vars (Unionfind.find point)
      in
        field_vars @ row_vars
  in
    ((fun t -> Utility.unduplicate (=) (free_type_vars' IntSet.empty t)),
     (fun t -> Utility.unduplicate (=) (free_row_type_vars' IntSet.empty t)))

type inference_assumption = inference_type Type_basis.assumption_basis
type inference_environment = inference_type Type_basis.environment_basis



module BasicInferenceTypeOps :
  (Type_basis.BASICTYPEOPS
   with type typ = inference_type
   and type row_var' = inference_row_var
   and type collection_type' = inference_collection_type) =
struct
  type typ = inference_type
  type row_var' = inference_row_var
  type collection_type' = inference_collection_type

  type field_spec = typ Type_basis.field_spec_basis
  type field_spec_map = typ Type_basis.field_spec_map_basis
  type row = (typ, row_var') Type_basis.row_basis

  let empty_field_env = StringMap.empty
  let closed_row_var = `RowVar None

  let make_type_variable var = `MetaTypeVar (Unionfind.fresh (`TypeVar var))
  let make_row_variable var = `MetaRowVar (Unionfind.fresh (empty_field_env, `RowVar (Some var)))
  let make_collection_variable var = `MetaCollectionVar (Unionfind.fresh (`CollectionTypeVar var))

  let is_closed_row =
    let rec is_closed_row' rec_vars =
      function
	| (_, `RowVar (Some var)) -> IntSet.mem var rec_vars
	| (_, `RowVar None) -> true
	| (_, `RecRowVar (var, row)) ->
	     ((IntSet.mem var rec_vars) or (is_closed_row' (IntSet.add var rec_vars) row))
	| (_, `MetaRowVar point) ->
	    is_closed_row' rec_vars (Unionfind.find point)
    in
      is_closed_row' IntSet.empty
end

let field_env_union : (inference_field_spec_map * inference_field_spec_map) -> inference_field_spec_map =
  fun (env1, env2) ->
    StringMap.fold (fun label field_spec env' ->
		      StringMap.add label field_spec env') env1 env2

let contains_present_fields field_env =
  StringMap.fold
    (fun label field_spec present ->
       match field_spec with
	 | `Present _ -> true
	 | `Absent -> present
    ) field_env false

let is_flattened_row : inference_row -> bool =
  let rec is_flattened = fun rec_vars -> function
    | (field_env, `MetaRowVar point) ->
	(match Unionfind.find point with 
	   | (field_env', `RowVar None) -> false
	   | (field_env', `RowVar (Some _)) ->
	       assert(not (contains_present_fields field_env')); true
	   | (field_env', `RecRowVar (var, rec_row)) ->
	       assert(not (contains_present_fields field_env'));
	       if IntSet.mem var rec_vars then true
	       else is_flattened (IntSet.add var rec_vars) rec_row
	   | (field_env', `MetaRowVar _) -> false)
    | (field_env, `RowVar (Some _ )) -> assert(false)
    | (field_env, `RowVar None) -> true
    | (field_env, `RecRowVar (var, rec_row)) ->
	assert(false)
  in
    is_flattened IntSet.empty

(* 
 convert a row to the form (field_env, row_var)
 where row_var is of the form:
    `RowVar None
  | `MetaRowVar (`RowVar (Some var))
  | `MetaRowVar (`RecRowVar (var, rec_row))
 *)
let flatten_row : inference_row -> inference_row =
  let rec flatten_row' : (inference_row Unionfind.point) IntMap.t -> inference_row -> inference_row =
    fun rec_env row ->
      let row' =
	match row with
	  | (field_env, `MetaRowVar point) ->
	      let row' = Unionfind.find point in
		(match row' with
		   | (field_env', `RowVar None) ->
		       field_env_union (field_env, field_env'), `RowVar None
		   | (field_env', `RowVar (Some var)) ->
		       assert(not (contains_present_fields field_env'));
		       if IntMap.mem var rec_env then
			 (field_env, `MetaRowVar (IntMap.find var rec_env))
		       else
		         row
		   | (field_env', `RecRowVar (var, rec_row)) ->
		       assert(not (contains_present_fields field_env'));
		       if IntMap.mem var rec_env then
			 field_env, `MetaRowVar (IntMap.find var rec_env)
		       else
			 (let point' = Unionfind.fresh (field_env', `RecRowVar (var, (StringMap.empty, `RowVar (Some var)))) in
			  let rec_row' = flatten_row' (IntMap.add var point' rec_env) rec_row in
			    Unionfind.change point' (field_env', `RecRowVar (var, rec_row'));
			    field_env_union (field_env, field_env'), `MetaRowVar point')
		   | (_, `MetaRowVar _) ->
		       let field_env', row_var' = flatten_row' rec_env row' in
			 field_env_union (field_env, field_env'), row_var')
	  | (field_env, `RowVar None) -> row
	  | _ -> assert(false)
      in
	assert (is_flattened_row row');
	row'
  in
    flatten_row' IntMap.empty


(*
 As flatten_row except if the flattened row_var is of the form:

  `MetaRowVar (`RecRowVar (var, rec_row))

then it is unwrapped. This ensures that all the fields are exposed
in field_env.
 *)
let unwrap_row : inference_row -> inference_row =
  let rec unwrap_row' : (inference_row Unionfind.point) IntMap.t -> inference_row -> inference_row =
    fun rec_env row ->
      let row' =
	match row with
	  | (field_env, `MetaRowVar point) ->
	      let row' = Unionfind.find point in
		(match row' with
		   | (field_env', `RowVar None) ->
		       field_env_union (field_env, field_env'), `RowVar None
		   | (field_env', `RowVar (Some var)) ->
		       assert(not (contains_present_fields field_env'));
		       if IntMap.mem var rec_env then
			 (field_env, `MetaRowVar (IntMap.find var rec_env))
		       else
		         row
		   | (field_env', `RecRowVar (var, rec_row)) ->
		       assert(not (contains_present_fields field_env'));
		       if IntMap.mem var rec_env then
			 field_env, `MetaRowVar (IntMap.find var rec_env)
		       else
			 (let point' = Unionfind.fresh (field_env', `RecRowVar (var, (StringMap.empty, `RowVar (Some var)))) in
			  let rec_row' = unwrap_row' (IntMap.add var point' rec_env) rec_row in
			    Unionfind.change point' (field_env', `RecRowVar (var, rec_row'));
			    let field_env'', row_var' = rec_row' in
			      (field_env_union ((field_env_union (field_env, field_env')), field_env''), row_var'))
		   | (_, `MetaRowVar _) ->
		       let field_env', row_var' = unwrap_row' rec_env row' in
			 field_env_union (field_env, field_env'), row_var')
	  | (field_env, `RowVar None) -> row
	  | _ -> assert(false)
      in
	assert (is_flattened_row row');
	row'
  in
    unwrap_row' IntMap.empty

module InferenceTypeOps :
  (Type_basis.TYPEOPS
   with type typ = inference_type
   and type row_var = inference_row_var
   and type collection_type = inference_collection_type) = Type_basis.TypeOpsGen(BasicInferenceTypeOps)

let empty_var_maps : unit ->
    ((inference_type Unionfind.point) IntMap.t ref *
     (inference_row Unionfind.point) IntMap.t ref *
     (inference_collection_type Unionfind.point) IntMap.t ref) =
  fun () ->
    let type_var_map : (inference_type Unionfind.point) IntMap.t ref = ref IntMap.empty in
    let row_var_map : (inference_row Unionfind.point) IntMap.t ref = ref IntMap.empty in
    let collection_var_map : (inference_collection_type Unionfind.point) IntMap.t ref = ref IntMap.empty in
      (type_var_map, row_var_map, collection_var_map)
    

(*** Conversions ***)

(* implementation *)
let rec type_to_inference_type = fun ((type_var_map, row_var_map, collection_var_map) as var_maps) -> function
  | `Not_typed -> `Not_typed
  | `Primitive p -> `Primitive p
  | `TypeVar var ->
      if IntMap.mem var (!type_var_map) then
	`MetaTypeVar (IntMap.find var (!type_var_map))
      else
	let point = Unionfind.fresh (`TypeVar var)
	in
	  type_var_map := IntMap.add var point (!type_var_map);
	  `MetaTypeVar point
  | `Function (f, t) -> `Function (type_to_inference_type var_maps f, type_to_inference_type var_maps t)
  | `Record row -> `Record (row_to_inference_row var_maps row)
  | `Variant row -> `Variant (row_to_inference_row var_maps row)
  | `Recursive (var, t) ->
      if IntMap.mem var (!type_var_map) then
	`MetaTypeVar (IntMap.find var (!type_var_map))
      else
	let point = Unionfind.fresh (`TypeVar var)
	in
	  type_var_map := IntMap.add var point (!type_var_map);
	  let t' = `Recursive (var, type_to_inference_type var_maps t) in
	    Unionfind.change point t';
	    `MetaTypeVar point
  | `Collection (ct, t) -> `Collection (collection_type_to_inference_collection_type var_maps ct,
					type_to_inference_type var_maps t)
  | `DB -> `DB
and field_spec_to_inference_field_spec = fun ((type_var_map, row_var_map, collection_var_map) as var_maps) -> function
  | `Present t -> `Present (type_to_inference_type var_maps t)
  | `Absent -> `Absent
and row_to_inference_row = fun ((type_var_map, row_var_map, collection_var_map) as var_maps) -> function
  | fields, `RowVar None ->
      (StringMap.map (field_spec_to_inference_field_spec var_maps) fields, `RowVar None)
  | fields, `RowVar (Some var) ->
      let field_env : inference_field_spec_map = StringMap.map (field_spec_to_inference_field_spec var_maps) fields
      in
	if IntMap.mem var (!row_var_map) then
	  (field_env, `MetaRowVar (IntMap.find var (!row_var_map)))
	else
	  let point = Unionfind.fresh (StringMap.empty, `RowVar (Some var))
	  in
	    row_var_map := IntMap.add var point (!row_var_map);
	    (field_env, `MetaRowVar point)
  | fields, `RecRowVar (var, rec_row) ->
      let field_env : inference_field_spec_map = StringMap.map (field_spec_to_inference_field_spec var_maps) fields
      in
	if IntMap.mem var (!row_var_map) then
	  (field_env, `MetaRowVar (IntMap.find var (!row_var_map)))
	else
	  let point = Unionfind.fresh (StringMap.empty, `RecRowVar (var, (StringMap.empty, `RowVar None)))
	  in
	    row_var_map := IntMap.add var point (!row_var_map);
	    let rec_row' = row_to_inference_row var_maps rec_row in
	      Unionfind.change point (StringMap.empty, `RecRowVar (var, rec_row'));
	      (field_env, `MetaRowVar point)
and collection_type_to_inference_collection_type = fun ((_, _, collection_var_map) as var_maps) -> function
  | `Set -> `Set | `Bag -> `Bag | `List -> `List
  | `CollectionTypeVar var ->
      if IntMap.mem var (!collection_var_map) then
	`MetaCollectionVar (IntMap.find var (!collection_var_map))
      else
	let point = Unionfind.fresh (`CollectionTypeVar var)
	in
	  collection_var_map := IntMap.add var point (!collection_var_map);
	  `MetaCollectionVar point

(* interface *)
let type_to_inference_type = type_to_inference_type (empty_var_maps ())
let field_spec_to_inference_field_spec = field_spec_to_inference_field_spec (empty_var_maps ())
let row_to_inference_row = row_to_inference_row (empty_var_maps ())
let collection_type_to_inference_collection_type = collection_type_to_inference_collection_type (empty_var_maps ())

(* implementation *)
let rec inference_type_to_type : type_var_set -> inference_type -> Kind.kind = fun rec_vars ->
  function
    | `Not_typed -> `Not_typed
    | `Primitive p -> `Primitive p
    | `TypeVar var -> `TypeVar var
    | `Function (f, t) -> `Function (inference_type_to_type rec_vars f, inference_type_to_type rec_vars t)
    | `Record row -> `Record (inference_row_to_row rec_vars row)
    | `Variant row -> `Variant (inference_row_to_row rec_vars row)
    | `Recursive (var, t) ->
	if IntSet.mem var rec_vars then
	  `TypeVar var
	else
	  `Recursive (var, inference_type_to_type (IntSet.add var rec_vars) t)
    | `Collection (ct, t) -> `Collection (inference_collection_type_to_collection_type ct, inference_type_to_type rec_vars t)
    | `DB -> `DB
    | `MetaTypeVar point -> inference_type_to_type rec_vars (Unionfind.find point)
and inference_field_spec_to_field_spec = fun rec_vars ->
  function
    | `Present t -> `Present (inference_type_to_type rec_vars t)
    | `Absent -> `Absent
and inference_row_to_row = fun rec_vars row ->
  let field_env, row_var = flatten_row row in
  let field_env' = StringMap.map (inference_field_spec_to_field_spec rec_vars) field_env in
  let row_var' = 
    match row_var with
      | `MetaRowVar point ->
	  (match Unionfind.find point with
	     | (env, `RowVar var) ->
		 assert(not (contains_present_fields env));
		 `RowVar var
	     | (env, `RecRowVar (var, rec_row)) ->
		 if IntSet.mem var rec_vars then
		   `RowVar (Some var)
		 else
		   `RecRowVar (var, inference_row_to_row (IntSet.add var rec_vars) rec_row)
	     | (_, `MetaRowVar _) -> assert(false))
      | `RowVar None ->
	  `RowVar None
      | `RowVar (Some _) ->
	  assert(false)
      | `RecRowVar (var, rec_row) ->
	  assert(false)
  in
    field_env', row_var'
(* implementation and interface *)
and inference_collection_type_to_collection_type = function
  | `Set -> `Set | `Bag -> `Bag | `List -> `List
  | `CollectionTypeVar var -> `CollectionTypeVar var
  | `MetaCollectionVar point -> inference_collection_type_to_collection_type (Unionfind.find point)

(* interface *)
let inference_type_to_type = inference_type_to_type IntSet.empty
let inference_field_spec_to_field_spec = inference_field_spec_to_field_spec IntSet.empty
let inference_row_to_row = inference_row_to_row IntSet.empty


(* assumptions *)
let assumption_to_inference_assumption : Kind.assumption -> inference_assumption = function
  | (quantifiers, t) -> (quantifiers, type_to_inference_type t)
let inference_assumption_to_assumption : inference_assumption -> Kind.assumption = function
  | (quantifiers, t) -> (quantifiers, inference_type_to_type t)

(* environments *)
let environment_to_inference_environment : Kind.environment -> inference_environment =
  List.map (fun (name, assumption) -> (name, assumption_to_inference_assumption assumption))
let inference_environment_to_environment : inference_environment -> Kind.environment =
  List.map (fun (name, assumption) -> (name, inference_assumption_to_assumption assumption))

(* output as a string *)
let string_of_type = Kind.string_of_kind -<- inference_type_to_type
let string_of_type_raw = Kind.string_of_kind_raw -<- inference_type_to_type
let string_of_row : inference_row -> string = Kind.string_of_row -<- inference_row_to_row

let string_of_assumption = Kind.string_of_assumption -<- inference_assumption_to_assumption
let string_of_environment = Kind.string_of_environment -<- inference_environment_to_environment
