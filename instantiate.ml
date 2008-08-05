open Utility
open Types

let show_recursion = Settings.add_bool("show_recursion", false, `User)
let show_instantiation = Settings.add_bool("show_instantiation", false, `User)

(*
  instantiation environment:
    for stopping cycles during instantiation
*)
type inst_type_env = meta_type_var IntMap.t
type inst_row_env = meta_row_var IntMap.t
type inst_env = inst_type_env * inst_row_env

let instantiate_datatype : (datatype IntMap.t * row_var IntMap.t) -> datatype -> datatype =
  fun (tenv, renv) ->
    let rec inst : inst_env -> datatype -> datatype = fun rec_env datatype ->
      let rec_type_env, rec_row_env = rec_env in
	match datatype with
	  | `Not_typed -> failwith "Internal error: `Not_typed' passed to `instantiate'"
	  | `Primitive _  -> datatype
	  | `MetaTypeVar point ->
	      let t = Unionfind.find point in
		(match t with
		   | `Flexible var
		   | `Rigid var ->
		       if IntMap.mem var tenv then
			 IntMap.find var tenv
		       else
			 datatype
		   | `Recursive (var, t) ->
		       Debug.if_set (show_recursion) (fun () -> "rec (instantiate)1: " ^(string_of_int var));

		       if IntMap.mem var rec_type_env then
			 (`MetaTypeVar (IntMap.find var rec_type_env))
		       else
			 (
			   let var' = Types.fresh_raw_variable () in
			   let point' = Unionfind.fresh (`Flexible var') in
			   let t' = inst (IntMap.add var point' rec_type_env, rec_row_env) t in
			   let _ = Unionfind.change point' (`Recursive (var', t')) in
			     `MetaTypeVar point'
			 )
		   | `Body t -> inst rec_env t)
	  | `Function (f, m, t) -> `Function (inst rec_env f, inst rec_env m, inst rec_env t)
	  | `Record row -> `Record (inst_row rec_env row)
	  | `Variant row -> `Variant (inst_row rec_env row)
	  | `Table (r, w) -> `Table (inst rec_env r, inst rec_env w)
          | `ForAll (_,t) -> inst rec_env t
          | `Alias ((name, ts), d) -> 
              `Alias ((name, List.map (inst rec_env) ts), inst rec_env d)
	  | `Application (n, elem_type) ->
	      `Application (n, List.map (inst rec_env) elem_type)
    and inst_row : inst_env -> row -> row = fun rec_env row ->
      let field_env, row_var = flatten_row row in
	
      let is_closed =
        match Unionfind.find row_var with
          | `Closed -> true
          | _ -> false in

      let field_env' = StringMap.fold
	(fun label field_spec field_env' ->
	   match field_spec with
	     | `Present t -> StringMap.add label (`Present (inst rec_env t)) field_env'
	     | `Absent ->
		 if is_closed then field_env'
		 else StringMap.add label `Absent field_env'
	) field_env StringMap.empty in
      let row_var' = inst_row_var rec_env row_var
      in
	field_env', row_var'
          (* precondition: row_var has been flattened *)
    and inst_row_var : inst_env -> row_var -> row_var = fun (rec_type_env, rec_row_env) row_var ->
      match row_var with
	| point ->
	    begin
              match Unionfind.find point with
	        | `Closed -> row_var
                | `Flexible var
                | `Rigid var ->
		    if IntMap.mem var renv then
		      IntMap.find var renv
		    else
		      row_var
	        | `Recursive (var, rec_row) ->
		    if IntMap.mem var rec_row_env then
		      IntMap.find var rec_row_env
		    else
		      begin
		        let var' = Types.fresh_raw_variable () in
		        let point' = Unionfind.fresh (`Flexible var') in
		        let rec_row' = inst_row (rec_type_env, IntMap.add var point' rec_row_env) rec_row in
		        let _ = Unionfind.change point' (`Recursive (var', rec_row')) in
			  point'
		      end
	        | `Body _ -> assert(false)
            end
    in
      inst (IntMap.empty, IntMap.empty)

(** instantiate_typ t
    remove any quantifiers and rename bound type vars accordingly
*)
let instantiate_typ : datatype -> (type_arg list * datatype) = fun t ->
  let rec collapse qs t =
    match qs, t with
      | qs, `ForAll (qs', t) -> collapse (qs @ qs') t
      | [], t -> t
      | qs, t -> `ForAll (qs, t)
  in
    match collapse [] t with
      | `ForAll (quantifiers, t) as dtype ->
	(
	  let () = Debug.if_set (show_instantiation)
	    (fun () -> "Instantiating datatype: " ^ string_of_datatype dtype) in

          let typ var (tenv, renv, tys) =
            let t = fresh_type_variable () in
              IntMap.add var t tenv, renv, `Type t :: tys in

          let row var (tenv, renv, tys) =
            let r = fresh_row_variable () in
              tenv, IntMap.add var r renv, `Row (StringMap.empty, r) :: tys in

	  let tenv, renv, tys = List.fold_left
	    (fun env -> function
	       | `TypeVar var
	       | `RigidTypeVar var -> typ var env
	       | `RowVar var
	       | `RigidRowVar var -> row var env
	    ) (IntMap.empty, IntMap.empty, []) quantifiers in
          let tys = List.rev tys in
	    tys, instantiate_datatype (tenv, renv) t)
      | t -> [], t

(** instantiate env var
    Get the type of `var' from the environment, and rename bound typevars.

    This returns the original type of var, the type arguments it is instantiated with
    and the instantiated type.
 *)
let instantiate : environment -> string -> type_arg list * datatype =
  fun env var ->
    let t =
      try
        Env.String.lookup env var
      with NotFound _ ->
        raise (Errors.UndefinedVariable ("Variable '"^ var ^
                                         "' does not refer to a declaration"))
    in
      instantiate_typ t
        
let var = instantiate
let typ = instantiate_typ
let datatype = instantiate_datatype

module SEnv = Env.String

let alias name ts env = 
  (* This is just type application.
  
     (\Lambda x1 ... xn . t) (t1 ... tn) ~> t[ti/xi]
  *)
  match (SEnv.find env name : Types.tycon_spec option) with
    | None -> 
        failwith (Printf.sprintf "Unrecognised type constructor: %s" name)
    | Some (`Abstract _) ->
        failwith (Printf.sprintf "The type constructor: %s is abstract, not an alias" name)
    | Some (`Alias (vars, _)) when List.length vars <> List.length ts ->
        failwith (Printf.sprintf
                    "Type alias %s applied with incorrect arity (%d instead of %d)"
                    name (List.length ts) (List.length vars))
    | Some (`Alias (vars, body)) ->
        let tenv = List.fold_right2 IntMap.add vars ts IntMap.empty in
          `Alias ((name, ts),
                  instantiate_datatype (tenv, IntMap.empty) (Types.freshen_mailboxes body))
