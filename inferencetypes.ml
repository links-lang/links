open Utility

type type_var_set = Kind.type_var_set

type inference_type = [
  | (inference_type, inference_row, inference_collection_type) Kind.type_basis
  | `MetaTypeVar of inference_type Unionfind.point ]
and inference_collection_type = [
  | Kind.collection_type
  | `MetaCollectionVar of inference_collection_type Unionfind.point ]
and inference_field_spec = inference_type Kind.field_spec_basis
and inference_field_spec_map = inference_field_spec StringMap.t
and inference_row_var = [
  | Kind.row_var
  | `MetaRowVar of inference_row Unionfind.point ]
and inference_row = (inference_type, inference_row_var) Kind.row_basis

let type_vars, row_type_vars =
  let rec type_vars' : type_var_set -> inference_type -> int list = fun rec_vars ->
    function
      | `Not_typed               -> []
      | `Primitive _             -> []
      | `TypeVar var             ->
	  if IntSet.mem var rec_vars then
	    []
	  else
	    [var]
      | `Function (from, into)   -> type_vars' rec_vars from @ type_vars' rec_vars into
      | `Record row              -> row_type_vars' rec_vars row
      | `Variant row             -> row_type_vars' rec_vars row
      | `Recursive (id, body)    ->
	  if IntSet.mem id rec_vars then
	    []
	  else
	    type_vars' (IntSet.add id rec_vars) body
      | `Collection (`CtypeVar id, kind)             -> id :: type_vars' rec_vars kind
      | `Collection (`MetaCollectionVar point, kind) -> type_vars' rec_vars (`Collection (Unionfind.find point, kind))
      | `Collection (_, kind)    -> type_vars' rec_vars kind
      | `DB                      -> []
      | `MetaTypeVar point       -> type_vars' rec_vars (Unionfind.find point)
  and row_type_vars' : type_var_set -> inference_row -> int list = fun rec_vars (field_env, row_var) ->
    let field_vars = List.concat (List.map (fun (_, t) -> type_vars' rec_vars t) (Kind.get_present_fields field_env)) in
    let row_vars =
      match row_var with
	| `RowVar (Some var) -> [var]
	| `RowVar None -> []
	| `MetaRowVar point -> row_type_vars' rec_vars (Unionfind.find point)
    in
      field_vars @ row_vars
  in
    ((fun t -> Utility.unduplicate (=) (type_vars' IntSet.empty t)),
     (fun t -> Utility.unduplicate (=) (row_type_vars' IntSet.empty t)))

type inference_assumption = inference_type Kind.assumption_basis
type inference_environment = inference_type Kind.environment_basis



module BasicInferenceTypeOps :
  (Kind.BASICTYPEOPS with type typ = inference_type
			and type row_var' = inference_row_var
			and type collection_type' = inference_collection_type) =
struct
  type typ = inference_type
  type row_var' = inference_row_var
  type collection_type' = inference_collection_type

  type field_spec = typ Kind.field_spec_basis
  type field_spec_map = typ Kind.field_spec_map_basis
  type row = (typ, row_var') Kind.row_basis

  let empty_field_env = StringMap.empty
  let closed_row_var = `RowVar None

  let make_type_var var = `MetaTypeVar (Unionfind.fresh (`TypeVar var))
  let make_row_var var = `MetaRowVar (Unionfind.fresh (empty_field_env, `RowVar (Some var)))
  let make_collection_var var = `MetaCollectionVar (Unionfind.fresh (`CtypeVar var))

  let rec is_closed_row = function
    | (_, `RowVar (Some _)) -> false
    | (_, `RowVar None) -> true
    | (_, `MetaRowVar point) ->
	is_closed_row (Unionfind.find point)
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

let is_flattened_row = function
  | (field_env, `MetaRowVar point) ->
      (match Unionfind.find point with 
	 | (field_env', `RowVar None) -> false
	 | (field_env', `RowVar (Some _)) ->
	     not (contains_present_fields field_env')
	 | (field_env', `MetaRowVar _) -> false)
  | (field_env, `RowVar (Some _ )) -> false
  | (field_env, `RowVar None) -> true
	
let rec flatten_row : inference_row -> inference_row =
  fun row ->
    let row' =
      match row with
	| (field_env, `MetaRowVar point) ->
	    let row' = Unionfind.find point in
	      (match row' with
		 | (field_env', `RowVar None) ->
		     field_env_union (field_env, field_env'), `RowVar None
		 | (field_env', `RowVar (Some _)) ->
		     assert(not (contains_present_fields field_env'));
		     row
		 | (_, `MetaRowVar _) ->
		     let field_env', row_var' = flatten_row row' in
		       field_env_union (field_env, field_env'), row_var')
	| (field_env, `RowVar None) -> row
	| _ -> assert(false)
    in
      assert (is_flattened_row row');
      row'

let get_field_env : inference_row -> inference_field_spec_map = fst @@ flatten_row
let get_row_var : inference_row -> inference_row_var = snd @@ flatten_row





module InferenceTypeOps :
  (Kind.TYPEOPS
   with type typ = inference_type
   and type row_var = inference_row_var
   and type collection_type = inference_collection_type) = Kind.TypeOpsGen(BasicInferenceTypeOps)
module ITO = InferenceTypeOps

(*
let row_to_inference_row' (r: Kind.row) = (r: Kind.row :> inference_row)
let type_to_inference_type' (t : Kind.kind) = (t : Kind.kind :> inference_type)
*)

let type_to_inference_type : Kind.kind -> inference_type = fun kind ->
  let type_var_map : (inference_type Unionfind.point) IntMap.t ref = ref IntMap.empty in
  let row_var_map : (inference_row Unionfind.point) IntMap.t ref = ref IntMap.empty in
  let collection_var_map: (inference_collection_type Unionfind.point) IntMap.t ref = ref IntMap.empty in

  let rec type_to_inference_type = function
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
    | `Function (f, t) -> `Function (type_to_inference_type f, type_to_inference_type t)
    | `Record row -> `Record (row_to_inference_row row)
    | `Variant row -> `Variant (row_to_inference_row row)
    | `Recursive (var, t) ->
	if IntMap.mem var (!type_var_map) then
	  `MetaTypeVar (IntMap.find var (!type_var_map))
	else
	  let point = Unionfind.fresh (`TypeVar var)
	  in
	    type_var_map := IntMap.add var point (!type_var_map);
	    let t' = `Recursive (var, type_to_inference_type t) in
	      Unionfind.change point t';
	      `MetaTypeVar point
    | `Collection (ct, t) -> `Collection (collection_type_to_inference_collection_type ct, type_to_inference_type t)
    | `DB -> `DB
  and field_spec_to_inference_field_spec = function
    | `Present t -> `Present (type_to_inference_type t)
    | `Absent -> `Absent
  and row_to_inference_row = function
    | fields, `RowVar None ->
	(StringMap.map field_spec_to_inference_field_spec fields, `RowVar None)
    | fields, `RowVar (Some var) ->
	let field_env : inference_field_spec_map = StringMap.map field_spec_to_inference_field_spec fields
	in
	  if IntMap.mem var (!row_var_map) then
	    (field_env, `MetaRowVar (IntMap.find var (!row_var_map)))
	  else
	    let point = Unionfind.fresh (StringMap.empty, `RowVar (Some var))
	    in
	      row_var_map := IntMap.add var point (!row_var_map);
	      (field_env, `MetaRowVar point)
  and collection_type_to_inference_collection_type = function
    | `Set -> `Set | `Bag -> `Bag | `List -> `List
    | `CtypeVar var ->
	if IntMap.mem var (!collection_var_map) then
	  `MetaCollectionVar (IntMap.find var (!collection_var_map))
	else
	  let point = Unionfind.fresh (`CtypeVar var)
	  in
	    collection_var_map := IntMap.add var point (!collection_var_map);
	    `MetaCollectionVar point
  in
    type_to_inference_type kind

(* HACK *)
let collection_type_to_inference_collection_type ct =
  let t' = type_to_inference_type (`Collection (ct, `DB))
  in
    match t' with 
      | `Collection (ct', _) -> ct'
      | _ -> assert (false)



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
    | `Collection (ct, t) -> `Collection (inference_collection_to_collection ct, inference_type_to_type rec_vars t)
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
(*		 assert(StringMap.is_empty env); *)
		 `RowVar var
	     | (_, `MetaRowVar _) -> assert(false))
      | `RowVar None ->
	  `RowVar None
      | `RowVar (Some _) ->
	  assert(false)
  in
    field_env', row_var'
and inference_collection_to_collection = function
  | `Set -> `Set | `Bag -> `Bag | `List -> `List
  | `CtypeVar var -> `CtypeVar var
  | `MetaCollectionVar point -> inference_collection_to_collection (Unionfind.find point)

let inference_type_to_type = inference_type_to_type IntSet.empty
let inference_field_spec_to_field_spec = inference_field_spec_to_field_spec IntSet.empty
let inference_row_to_row = inference_row_to_row IntSet.empty


let assumption_to_inference_assumption : Kind.assumption -> inference_assumption = function
  | (quantifiers, t) -> (quantifiers, type_to_inference_type t)
let inference_assumption_to_assumption : inference_assumption -> Kind.assumption = function
  | (quantifiers, t) -> (quantifiers, inference_type_to_type t)


let environment_to_inference_environment : Kind.environment -> inference_environment =
  List.map (fun (name, assumption) -> (name, assumption_to_inference_assumption assumption))
let inference_environment_to_environment : inference_environment -> Kind.environment =
  List.map (fun (name, assumption) -> (name, inference_assumption_to_assumption assumption))

let string_of_kind = Kind.string_of_kind @@ inference_type_to_type
let string_of_kind_raw = Kind.string_of_kind_raw @@ inference_type_to_type
let string_of_row : inference_row -> string = Kind.string_of_row @@ inference_row_to_row

let string_of_assumption = Kind.string_of_assumption @@ inference_assumption_to_assumption
let string_of_environment = Kind.string_of_environment @@ inference_environment_to_environment
