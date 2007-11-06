open Utility
open Types

(* debug flags *)
let show_generalisation = Settings.add_bool("show_generalisation", false, `User)
let show_recursion = Instantiate.show_recursion

(*
 get the quantifiers for a datatype
 i.e. the quantifiers required to close a datatype
 
 (the first argument specifies type variables that should remain free)
*)
let rec get_quantifiers : TypeVarSet.t -> datatype -> quantifier list = 
  fun bound_vars -> 
    function
      | `Not_typed -> failwith "Internal error: Not_typed encountered in get_quantifiers"
      | `Primitive _ -> []
      | `MetaTypeVar point ->
	  (match Unionfind.find point with
	     | `Flexible var
	     | `Rigid var when TypeVarSet.mem var bound_vars -> []
	     | `Flexible var -> [`TypeVar var]
	     | `Rigid var -> [`RigidTypeVar var]
	     | `Recursive (var, body) ->
		 Debug.if_set (show_recursion) (fun () -> "rec (get_quantifiers): " ^(string_of_int var));
		 if TypeVarSet.mem var bound_vars then
		   []
		 else
		   get_quantifiers (TypeVarSet.add var bound_vars) body
	     | `Body t -> get_quantifiers bound_vars t)
      | `Function (f, m, t) ->
          let from_gens = get_quantifiers bound_vars f
          and mailbox_gens = get_quantifiers bound_vars m
          and to_gens = get_quantifiers bound_vars t in
            unduplicate (=) (from_gens @ mailbox_gens @ to_gens)
      | `Record row
      | `Variant row -> get_row_quantifiers bound_vars row 
      | `Table (r, w) -> unduplicate (=) (get_quantifiers bound_vars r @ get_quantifiers bound_vars w)
      | `ForAll (qs, t) -> 
          get_quantifiers (List.fold_right (Types.type_var_number ->- TypeVarSet.add) qs bound_vars) t
      | `Application (_, args) ->
          unduplicate (=) (Utility.concat_map (get_quantifiers bound_vars) args)

and get_row_var_quantifiers : TypeVarSet.t -> row_var -> quantifier list =
  fun bound_vars row_var ->
    match Unionfind.find row_var with
      | `Closed -> []
      | `Flexible var
      | `Rigid var when TypeVarSet.mem var bound_vars -> []
      | `Flexible var
      | `Rigid var -> [`RowVar var]
      | `Recursive (var, rec_row) ->
	  Debug.if_set (show_recursion) (fun () -> "rec (get_row_var_quantifiers): " ^(string_of_int var));
	  (if TypeVarSet.mem var bound_vars then
	     []
	   else
	     get_row_quantifiers (TypeVarSet.add var bound_vars) rec_row)
      | `Body row -> get_row_quantifiers bound_vars row

and get_field_spec_quantifiers : TypeVarSet.t -> field_spec -> quantifier list =
    fun bound_vars ->
      function
	| `Present t -> get_quantifiers bound_vars t
	| `Absent -> []

and get_row_quantifiers : TypeVarSet.t -> row -> quantifier list =
    fun bound_vars (field_env, row_var) ->
      let field_vars = StringMap.fold
	(fun _ field_spec vars ->
	   get_field_spec_quantifiers bound_vars field_spec @ vars
	) field_env [] in
      let row_vars = get_row_var_quantifiers bound_vars (row_var:row_var)
      in
	field_vars @ row_vars

let env_type_vars (env : Types.environment) =
  TypeVarSet.union_all (List.map free_type_vars (Env.String.range env))

(** generalise: 
    Universally quantify any free type variables in the expression.
*)
let generalise : environment -> datatype -> datatype = 
  fun env t ->
    let vars_in_env = env_type_vars env in
    let quantifiers = get_quantifiers vars_in_env t in
    let quantified = Types.for_all (quantifiers, t) in 
      Debug.if_set show_generalisation (fun () -> "Generalised: " ^ string_of_datatype quantified);
      quantified 
