(*pp deriving *)
open List

open Utility
open Syntax
open Inferencetypes
open Forms
open Errors

(* debug flags *)
let show_unification = Settings.add_bool("show_unification", false, `User)
let show_row_unification = Settings.add_bool("show_row_unification", false, `User)

let show_instantiation = Settings.add_bool("show_instantiation", false, `User)
let show_generalisation = Settings.add_bool("show_generalisation", false, `User)

let show_typechecking = Settings.add_bool("show_typechecking", false, `User)
let show_recursion = Settings.add_bool("show_recursion", false, `User)

let rigid_type_variables = Settings.add_bool("rigid_type_variables", true, `User)

(* whether to allow negative recursive types to be inferred *)
let infer_negative_types = Settings.add_bool("infer_negative_types", true, `User)

exception Unify_failure of string
exception UndefinedVariable of string

module ITO = InferenceTypeOps

let db_descriptor_type =
  snd (Parse.parse_string Parse.datatype "(driver:String, name:String, args:String)")

(* extract data from inference_expressions *)
let type_of_expression : expression -> datatype =
  fun exp -> let `T (_, t, _) = expression_data exp in t
let pos_of_expression : expression -> position =
  fun exp -> let `T (pos, _, _) = expression_data exp in pos

let rec extract_row : datatype -> row = function
  | `Record row -> row
  | `Variant row -> row
  | `MetaTypeVar point ->
      extract_row (Unionfind.find point)
  | t -> failwith
      ("Internal error: attempt to extract a row from a datatype that is not a record or variant: " ^ (string_of_datatype t))

let var_is_free_in_type var datatype = mem var (free_type_vars datatype)

(* a special kind of structural equality on types that doesn't look
inside points *)
let rec eq_types : (datatype * datatype) -> bool =
  fun (t1, t2) ->
    match (t1, t2) with
      | `Not_typed, `Not_typed -> true
      | `Primitive x, `Primitive y -> x = y
      | `MetaTypeVar lpoint, `MetaTypeVar rpoint ->
	  Unionfind.equivalent lpoint rpoint
      | `Function (lfrom, lm, lto), `Function (rfrom, rm, rto) ->
	  eq_types (lfrom, rfrom) && eq_types (lto, rto) && eq_types (lm, rm)
      | `Record l, `Record r -> eq_rows (l, r)
      | `Variant l, `Variant r -> eq_rows (l, r)
      | `Application (s, ts), `Application (s', ts') when s = s' -> List.for_all2 (Utility.curry eq_types) ts ts'
      | _, _ -> false
and eq_rows : (row * row) -> bool =
  fun ((lfield_env, lrow_var), (rfield_env, rrow_var)) ->
    eq_field_envs (lfield_env, rfield_env) && eq_row_vars (lrow_var, rrow_var)
and eq_field_envs (lfield_env, rfield_env) =
  let compare_specs = fun a b -> 
    match (a,b) with
      | `Absent, `Absent -> true
      | `Present t1, `Present t2 -> eq_types (t1, t2)
      | _, _ -> false
  in
    StringMap.equal compare_specs lfield_env rfield_env
and eq_row_vars = function
  | `RowVar (None), `RowVar (None) -> true
  | `MetaRowVar lpoint, `MetaRowVar rpoint -> Unionfind.equivalent lpoint rpoint
  | _, _ -> false


(*
  instantiation environment:
    for stopping cycles during instantiation
*)
type inst_type_env = (datatype Unionfind.point) IntMap.t
type inst_row_env = (row Unionfind.point) IntMap.t
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
		   | `RigidTypeVar var
		   | `TypeVar var ->
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
			   let var' = Inferencetypes.fresh_raw_variable () in
			   let point' = Unionfind.fresh (`TypeVar var') in
			   let t' = inst (IntMap.add var point' rec_type_env, rec_row_env) t in
			   let _ = Unionfind.change point' (`Recursive (var', t')) in
			     `MetaTypeVar point'
			 )
		   | _ -> inst rec_env t)
	  | `Function (f, m, t) -> `Function (inst rec_env f, inst rec_env m, inst rec_env t)
	  | `Record row -> `Record (inst_row rec_env row)
	  | `Variant row ->  `Variant (inst_row rec_env row)
	  | `Table row -> `Table (inst_row rec_env row)
	  | `Application (n, elem_type) ->
	      `Application (n, List.map (inst rec_env) elem_type)
	  | `Recursive _
	  | `RigidTypeVar _
	  | `TypeVar _ -> assert false
    and inst_row : inst_env -> row -> row = fun rec_env row ->
      let field_env, row_var = flatten_row row in
	
      let is_closed = (row_var = `RowVar None) in
	
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
	| `MetaRowVar point ->
	    (match Unionfind.find point with
	       | (_, `RowVar None)
	       | (_, `MetaRowVar _) -> assert(false)
               | (field_env, `RigidRowVar var)
	       | (field_env, `RowVar (Some var)) ->
		   assert(StringMap.is_empty field_env);
		   if IntMap.mem var renv then
		     IntMap.find var renv
		   else
		     row_var
	       | (field_env, `RecRowVar (var, rec_row)) ->
		   assert(StringMap.is_empty field_env);
		   if IntMap.mem var rec_row_env then
		     (`MetaRowVar (IntMap.find var rec_row_env))
		   else
		     (
		       let var' = Inferencetypes.fresh_raw_variable () in
		       let point' = Unionfind.fresh (field_env, `RowVar (Some var')) in
		       let rec_row' = inst_row (rec_type_env, IntMap.add var point' rec_row_env) rec_row in
		       let _ = Unionfind.change point' (field_env, `RecRowVar (var', rec_row')) in
			 `MetaRowVar point'
		     ))
	| `RowVar None ->
	    `RowVar None
	| `RigidRowVar _
	| `RowVar (Some _)
	| `RecRowVar (_, _) -> assert false
    in
      inst (IntMap.empty, IntMap.empty)

(*
  unification environment:
    for stopping cycles during unification
    and for type aliases
*)
type unify_type_env = (datatype list) IntMap.t
type unify_row_env = (row list) IntMap.t
type unify_env = unify_type_env * unify_row_env * alias_environment

let rec unify' : unify_env -> (datatype * datatype) -> unit = fun rec_env ->
  let rec_types, rec_rows, alias_env = rec_env in

  let is_unguarded_recursive t =
    let rec is_unguarded rec_types t = 
      match t with
        | `MetaTypeVar point ->
            begin
              match (Unionfind.find point) with
                | `Recursive (var, body) when IntSet.mem var rec_types -> true
                | `Recursive (var, body) -> is_unguarded (IntSet.add var rec_types) body
                | t -> is_unguarded rec_types t
            end
        |  _ -> false
    in
      is_unguarded IntSet.empty t in
    
  let unify_rec ((var, body), t) =
    let ts =
      if IntMap.mem var rec_types then
	IntMap.find var rec_types
      else
	[body]
    in
      (* break cycles *)
      if List.exists (fun t' -> eq_types (t, t')) ts then
	()
      else
	unify' (IntMap.add var (t::ts) rec_types, rec_rows, alias_env) (body, t) in

  let unify_rec2 ((lvar, lbody), (rvar, rbody)) =
    let lts =
      if IntMap.mem lvar rec_types then
	IntMap.find lvar rec_types
      else
	[lbody] in
      
    let rts =
      if IntMap.mem rvar rec_types then
	IntMap.find rvar rec_types
      else
	[rbody]
    in
      (* break cycles *)
      if (List.exists (fun t -> eq_types (t, rbody)) lts
	  || List.exists (fun t -> eq_types (t, lbody)) rts) then
	()
      else
	unify' ((IntMap.add lvar (rbody::lts) ->- IntMap.add rvar (lbody::rts)) rec_types, rec_rows, alias_env) (lbody, rbody) in

  (* introduce a recursive type
     give an error if it is non-well-founded and
     non-well-founded type inference is switched off

     preconditions:
     - Unionfind.find point = t
     - var is free in t
  *)
  let rec_intro point (var, t) =
    if Settings.get_value infer_negative_types || not (is_negative var (`MetaTypeVar point)) then
      (* 
         Using instantiate_datatype here is overkill
         but at least it's correct!

         The only tricky case is where t is `Recursive (var', t'). In this case
         we need to make sure var' is given a new point inside t', but we don't
         actually have to give new points to any of the other recursive types inside t'
         (which is one of the side-effects of instantiation).

         Note that we cannot just pass t to instantiate_datatype as it may not be
         a valid datatype (e.g. `Recursive (var, t) is not a type, but
         `MetaTypeVar point where Unionfind.point = `Recursive (var, t) is). This is
         an annoying consequence of deriving Types.datatype and Inferencetypes.datatype
         from a common basis.
      *)
      Unionfind.change point (`Recursive (var,
                                          instantiate_datatype
                                            (IntMap.add var (`MetaTypeVar point) (IntMap.empty), IntMap.empty) (`MetaTypeVar point)))
    else
      failwith "non-well-founded type inferred!" in

  let lookup_alias (s, ts) alias_env =
    let vars, alias =
      if StringMap.mem s alias_env then
        StringMap.find s alias_env
      else
        raise (Unify_failure ("Unbound typename "^s))
    in
      if List.length vars <> List.length ts then
        raise (Unify_failure
                 ("Alias '"^s^"' takes "^string_of_int(List.length vars)^" arguments but is applied to "^
                    string_of_int(List.length ts)^" arguments ("^String.concat "," (List.map string_of_datatype ts)^")"))
      else
        vars, alias in

  let instantiate_alias (vars, alias) ts =
    let _, tenv =
      List.fold_left (fun (ts, tenv) tv ->
                        match ts, tv with
                          | (t::ts), `TypeVar var ->
                              ts, IntMap.add var t tenv
                          | _ -> assert false) (ts, IntMap.empty) vars
    in
      instantiate_datatype (tenv, IntMap.empty) (Inferencetypes.freshen_mailboxes alias) in

    fun (t1, t2) ->
      (Debug.if_set (show_unification) (fun () -> "Unifying "^string_of_datatype t1^" with "^string_of_datatype t2);
       (match (t1, t2) with
          | `Not_typed, _ | _, `Not_typed -> failwith "Internal error: `Not_typed' passed to `unify'"
          | `Primitive x, `Primitive y when x = y -> ()
          | `MetaTypeVar lpoint, `MetaTypeVar rpoint ->
	      if Unionfind.equivalent lpoint rpoint then
	        ()
	      else
	        (match (Unionfind.find lpoint, Unionfind.find rpoint) with
	           | `RigidTypeVar l, `RigidTypeVar r ->
                       if l <> r then 
                         raise (Unify_failure ("Rigid type variables "^ string_of_int l ^" and "^ string_of_int r ^" do not match"))
                       else 
		         Unionfind.union lpoint rpoint
	           | `TypeVar _, `TypeVar _ ->
		       Unionfind.union lpoint rpoint
	           | `TypeVar var, t ->
		       (if var_is_free_in_type var (`MetaTypeVar rpoint) then
		          (Debug.if_set (show_recursion) (fun () -> "rec intro1 (" ^ (string_of_int var) ^ ")");
		           rec_intro rpoint (var, t))
		        else
		          ());
		       Unionfind.union lpoint rpoint
	           | t, `TypeVar var ->
		       (if var_is_free_in_type var (`MetaTypeVar lpoint) then
		          (Debug.if_set (show_recursion) (fun () -> "rec intro2 (" ^ (string_of_int var) ^ ")");
		           rec_intro lpoint (var, t))
		        else
		          ());
		       Unionfind.union rpoint lpoint
                   | `RigidTypeVar l, _ ->
                       raise (Unify_failure ("Couldn't unify the rigid type variable "^
                                               string_of_int l ^" with the type "^ string_of_datatype (`MetaTypeVar rpoint)))
                   | _, `RigidTypeVar r ->
                       raise (Unify_failure ("Couldn't unify the rigid type variable "^
                                               string_of_int r ^" with the type "^ string_of_datatype (`MetaTypeVar lpoint)))
	           | `Recursive (lvar, t), `Recursive (rvar, t') ->
		       assert(lvar <> rvar);
		       Debug.if_set (show_recursion)
		         (fun () -> "rec pair (" ^ (string_of_int lvar) ^ "," ^ (string_of_int rvar) ^")");
                       begin
                         if is_unguarded_recursive (`MetaTypeVar lpoint) then
                           begin
                             if not (is_unguarded_recursive (`MetaTypeVar rpoint)) then
                               raise (Unify_failure ("Couldn't unify the unguarded recursive type "^
                                                       string_of_datatype (`MetaTypeVar lpoint) ^
                                                       " with the guarded recursive type "^ string_of_datatype (`MetaTypeVar rpoint)))
                           end
                         else if is_unguarded_recursive (`MetaTypeVar lpoint) then
                           raise (Unify_failure ("Couldn't unify the unguarded recursive type "^
                                                   string_of_datatype (`MetaTypeVar rpoint) ^
                                                   " with the guarded recursive type "^ string_of_datatype (`MetaTypeVar lpoint)))
                         else
		           unify_rec2 ((lvar, t), (rvar, t'))
                       end;
		       Unionfind.union lpoint rpoint
	           | `Recursive (var, t'), t ->
		       Debug.if_set (show_recursion) (fun () -> "rec left (" ^ (string_of_int var) ^ ")");
                       begin
                         if is_unguarded_recursive (`MetaTypeVar lpoint) then
                           raise (Unify_failure ("Couldn't unify the unguarded recursive type "^
                                                   string_of_datatype (`MetaTypeVar lpoint) ^
                                                   " with the non-recursive type "^ string_of_datatype (`MetaTypeVar rpoint)))
                         else                   
		           unify_rec ((var, t'), t)
                       end;
		       Unionfind.union rpoint lpoint
	           | t, `Recursive (var, t') ->
		       Debug.if_set (show_recursion) (fun () -> "rec right (" ^ (string_of_int var) ^ ")");
                       begin
                         if is_unguarded_recursive (`MetaTypeVar rpoint) then
                           raise (Unify_failure ("Couldn't unify the unguarded recursive type "^
                                                   string_of_datatype (`MetaTypeVar rpoint) ^
                                                   " with the non-recursive type "^ string_of_datatype (`MetaTypeVar lpoint)))
                         else                   
		           unify_rec ((var, t'), t)
                       end;
		       Unionfind.union lpoint rpoint
	           | t, t' -> unify' rec_env (t, t'); Unionfind.union lpoint rpoint)
          | `MetaTypeVar point, t | t, `MetaTypeVar point ->
              (match (Unionfind.find point) with
                 | `RigidTypeVar l -> 
                     raise (Unify_failure ("Couldn't unify the rigid type variable "^ string_of_int l ^" with the type "^ string_of_datatype t))
	         | `TypeVar var ->
		     if var_is_free_in_type var t then
                       begin
   		         Debug.if_set (show_recursion)
		           (fun () -> "rec intro3 ("^string_of_int var^","^string_of_datatype t^")");
                         let point' = Unionfind.fresh t
                         in
                           rec_intro point' (var, t);
		           Unionfind.union point point'
                       end
		     else
		       (Debug.if_set (show_recursion) (fun () -> "non-rec intro (" ^ string_of_int var ^ ")");
		        Unionfind.change point t)
	         | `Recursive (var, t') ->
   		     Debug.if_set (show_recursion) (fun () -> "rec single (" ^ (string_of_int var) ^ ")");
                     begin
                       if is_unguarded_recursive (`MetaTypeVar point) then
                         raise (Unify_failure ("Couldn't unify the unguarded recursive type "^
                                                 string_of_datatype (`MetaTypeVar point) ^
                                                 " with the non-recursive type "^ string_of_datatype t))
                       else                   
		         unify_rec ((var, t'), t)
                     end
		       (* It's tempting to try to do this, but it isn't sound
		          as point may appear inside t
		          
		          Unionfind.change point t;
		       *)
	         | t' -> unify' rec_env (t, t'))
          | `Function (lfrom, lm, lto), `Function (rfrom, rm, rto)
	      when Inferencetypes.using_mailbox_typing () ->
              (unify' rec_env (lm, rm);
               unify' rec_env (lfrom, rfrom);
               unify' rec_env (lto, rto))
          | `Function (lfrom, _, lto), `Function (rfrom, _, rto) ->
              unify' rec_env (lfrom, rfrom);
              unify' rec_env (lto, rto)
          | `Record l, `Record r -> unify_rows' rec_env (l, r)
          | `Variant l, `Variant r -> unify_rows' rec_env (l, r)
          | `Table l, `Table r -> unify_rows' rec_env (l, r)
          | `Application (ls, lts), `Application (rs, rts) when ls = rs -> List.iter2 (fun lt rt -> unify' rec_env (lt, rt)) lts rts
          | `Application (ls, lts), `Application (rs, rts) ->
              let lvars, lalias = lookup_alias (ls, lts) alias_env
              and rvars, ralias = lookup_alias (rs, rts) alias_env in
                begin
                  match lalias, ralias with
                    | `Primitive `Abstract, `Primitive `Abstract ->
                        raise (Unify_failure
                                 ("Cannot unify abstract type '"^ls^"' with abstract type '"^rs^"'"))
                    | `Primitive `Abstract, _ ->
                        unify' rec_env (t1, instantiate_alias (rvars, ralias) rts)
                    | _, `Primitive `Abstract ->
                        unify' rec_env (instantiate_alias (lvars, lalias) lts, t2)
                    | _, _ ->
                        unify' rec_env (instantiate_alias (lvars, lalias) lts,
                                        instantiate_alias (rvars, ralias) rts)
                end
          | `Application (s, ts), t | t, `Application (s, ts) ->
              let vars, alias = lookup_alias (s, ts) alias_env in
                begin
                  match alias with
                    | `Primitive `Abstract ->
                        raise (Unify_failure
                                 ("Cannot unify abstract type '"^s^"' with type '"^string_of_datatype t^"'"))
                    | _ ->
                        unify' rec_env (instantiate_alias (vars, alias) ts, t)
                end
          | _, _ ->
              raise (Unify_failure ("Couldn't match "^ string_of_datatype t1 ^" against "^ string_of_datatype t2)));
       Debug.if_set (show_unification) (fun () -> "Unified types: " ^ string_of_datatype t1);
      )

and unify_rows' : unify_env -> ((row * row) -> unit) = 
  fun rec_env (lrow, rrow) ->
    Debug.if_set (show_row_unification) (fun () -> "Unifying row: " ^ (string_of_row lrow) ^ " with row: " ^ (string_of_row rrow));

    (* 
       [NOTE]

       - All calls to fail_on_absent_fields are currently disabled,
       as under the current model absent fields have
       to be allowed in closed rows (although they're ignored).

       - There's no way of getting rid of absent variables as they're stored in the field
       environment rather than the row variable (good argument for moving them into the
       row variable).
    *)
    (*
      let fail_on_absent_fields field_env =
      StringMap.iter
      (fun _ -> function
      | `Present _ -> ()
      | `Absent ->
      failwith "Internal error: closed row with absent variable"
      ) field_env in
    *)

    let is_unguarded_recursive row =
      let rec is_unguarded rec_rows (field_env, row_var) =
        StringMap.is_empty field_env &&
          (match row_var with
             | `MetaRowVar point ->
                 let ((field_env, row_var) as row) = Unionfind.find point in
                   StringMap.is_empty field_env &&
                     (match row_var with
                        | `RecRowVar (var, row) when IntSet.mem var rec_rows -> true
                        | `RecRowVar (var, row) -> is_unguarded (IntSet.add var rec_rows) row
                        | _ -> is_unguarded rec_rows row)
             |  _ -> false)
      in
        is_unguarded IntSet.empty row in

    (* extend_field_env traversal_env extending_env
       extends traversal_env with all the fields in extending_env

       Matching `Present fields are unified.

       Any fields in extending_env, but not in traversal_env are
       added to an extension environment which is returned.
    *)
    let extend_field_env
	(rec_env : unify_env)
	(traversal_env : field_spec_map)
	(extending_env : field_spec_map) =
      StringMap.fold
	(fun label field_spec extension ->
	   if StringMap.mem label extending_env then
	     (match field_spec, (StringMap.find label extending_env) with
	        | `Present t, `Present t' ->
		    unify' rec_env (t, t');
		    extension
	        | `Absent, `Absent ->
		    extension
		| `Present _, `Absent
		| `Absent, `Present _ ->
		    raise (Unify_failure ("Rows\n "^ string_of_row lrow
					  ^"\nand\n "^ string_of_row rrow
					  ^"\n could not be unified because they have conflicting fields"))
	     )
	   else
	     StringMap.add label field_spec extension
	) traversal_env (StringMap.empty) in

    let unify_compatible_field_environments rec_env (field_env1, field_env2) =
      ignore (extend_field_env rec_env field_env1 field_env2) in

    (* introduce a recursive row
       give an error if it is non-well-founded and
       non-well-founded type inference is switched off
    *)
    (*
      [BUG]
      need to do the same instantiation trick here that we do for rec_intro
      [TODO]
      * expose instantiate_row
      * use it here!
    *) 
    let rec_row_intro point (field_env, var, row) =
      if Settings.get_value infer_negative_types || not (is_negative_row var row) then
	Unionfind.change point (field_env, `RecRowVar (var, row))
      else
	failwith "non-well-founded row type inferred!" in


    (*
      unify_row_var_with_row rec_env (row_var, row)
      attempts to unify row_var with row
      
      However, row_var may already have been instantiated, in which case
      it is unified with row.
    *)
    let unify_row_var_with_row : unify_env -> row_var * row -> unit =
      fun rec_env (row_var, ((extension_field_env, extension_row_var) as extension_row)) ->
        (* unify row_var with `RowVar None *)
        let close_empty_row_var : row_var -> unit = function
          | `RowVar None ->
              ()
          | `MetaRowVar point ->
              let row = Unionfind.find point in
                if not (ITO.is_closed_row row) && is_rigid_row row then
                  raise (Unify_failure ("Closed row var cannot be unified with rigid row var\n"))
                else
                  Unionfind.change point (StringMap.empty, `RowVar None)
          | _ -> assert false in

        (* unify row_var with `RigidRowVar var *)
        let rigidify_empty_row_var var : row_var -> unit = function
          | `RowVar None ->
	      raise (Unify_failure ("Rigid row var cannot be unified with empty closed row\n"))
          | `MetaRowVar point ->
              let row = Unionfind.find point in
                if ITO.is_closed_row row then
		  raise (Unify_failure ("Rigid row var cannot be unified with empty closed row\n"))
                else if is_rigid_row row && not (is_rigid_row_with_var var row) then
                  raise (Unify_failure ("Incompatible rigid row variables cannot be unified\n"))
                else
                  Unionfind.change point (StringMap.empty, `RigidRowVar var)
          | _ -> assert false in

	let rec extend = function
	  | `MetaRowVar point ->
	      (* point should be a row variable *)
	      let (field_env, row_var) as row = Unionfind.find point in
		if StringMap.is_empty field_env then
		  begin
		    match row_var with
		      | `RowVar None ->
                          if is_empty_row extension_row then
                            close_empty_row_var extension_row_var
                          else
			    raise (Unify_failure ("Closed row cannot be extended with non-empty row\n"
						  ^string_of_row extension_row))
		      | `RigidRowVar var ->
                          if is_empty_row extension_row then
                            rigidify_empty_row_var var extension_row_var
                          else
			    raise (Unify_failure ("Rigid row variable cannot be unified with non-empty row\n"
						  ^string_of_row extension_row))
		      | `RowVar (Some var) ->
			  if mem var (free_row_type_vars extension_row) then
			    rec_row_intro point (field_env, var, extension_row)
			  else
			    begin
			      if StringMap.is_empty extension_field_env then
				match extension_row_var with
				  | `MetaRowVar point' ->
				      Unionfind.union point point'
				  | `RowVar None ->
				      Unionfind.change point extension_row
				  | _ -> assert false
			      else
				Unionfind.change point extension_row
			    end
		      | `RecRowVar _ ->
			  unify_rows' rec_env ((StringMap.empty, row_var), extension_row)
		      | `MetaRowVar _ -> assert false
		  end
		else
		  unify_rows' rec_env (row, extension_row)
	  | `RowVar None ->
              if is_empty_row extension_row then
                close_empty_row_var extension_row_var
              else
		raise (Unify_failure ("Closed row cannot be extended with non-empty row\n"
				      ^string_of_row extension_row))
	  | `RowVar _
	  | `RigidRowVar _
	  | `RecRowVar _ -> assert false
	in
          extend row_var in


    (* 
       matching_labels (big_field_env, small_field_env)
       return the set of labels that appear in both big_field_env and small_field_env

       precondition: big_field_env contains small_field_env
    *)
    let matching_labels : field_spec_map * field_spec_map -> StringSet.t = 
      fun (big_field_env, small_field_env) ->
	StringMap.fold (fun label _ labels ->
			  if StringMap.mem label small_field_env then
			    StringSet.add label labels
			  else
			    labels) big_field_env StringSet.empty in

    let row_without_labels : StringSet.t -> row -> row =
      fun labels (field_env, row_var) ->
	let restricted_field_env =
	  StringSet.fold (fun label field_env ->
			    StringMap.remove label field_env) labels field_env
	in
	  (restricted_field_env, row_var) in

    (*
      register a recursive row in the rec_env environment
      
      return:
      None if the recursive row already appears in the environment
      Some rec_env, otherwise, where rec_env is the updated environment
    *)
    let register_rec_row (wrapped_field_env, unwrapped_field_env, rec_row, unwrapped_row') : unify_env -> unify_env option =
      fun ((rec_types, rec_rows, alias_env) as rec_env) ->
	match rec_row with
	  | Some (var, body) ->
	      let restricted_row = row_without_labels (matching_labels (unwrapped_field_env, wrapped_field_env)) unwrapped_row' in
	      let rs =
		if IntMap.mem var rec_rows then
		  IntMap.find var rec_rows
		else
		  [(StringMap.empty, `RecRowVar (var, body))]
	      in
		if List.exists (fun r -> eq_rows (r, restricted_row)) rs then
		  None
		else
		  Some (rec_types, IntMap.add var (restricted_row::rs) rec_rows, alias_env)
	  | None -> 
	      Some rec_env in

    (*
      register two recursive rows and return None if one of them is already in the environment
    *)
    let register_rec_rows p1 p2 : unify_env -> unify_env option = fun rec_env ->
      let rec_env' = register_rec_row p1 rec_env in
	match rec_env' with
	  | None -> None
	  | Some rec_env -> register_rec_row p2 rec_env in

    let unify_both_rigid_with_rec_env rec_env ((lfield_env, _ as lrow), (rfield_env, _ as rrow)) =
      let get_present_labels (field_env, row_var) =
	let rec get_present' rec_vars (field_env, row_var) =
	  let top_level_labels = 
	    StringMap.fold (fun label field_spec labels ->
			      match field_spec with
				| `Present _ -> StringSet.add label labels
				| `Absent -> labels) field_env StringSet.empty
	  in
	    StringSet.union top_level_labels 
	      (match row_var with
		 | `RecRowVar (var, body) when (not (IntSet.mem var rec_vars)) ->
		     get_present' (IntSet.add var rec_vars) body
		 | _ -> StringSet.empty) in
	  get_present' IntSet.empty (field_env, row_var) in
	
      let fields_are_compatible (lrow, rrow) =
	(StringSet.equal (get_present_labels lrow) (get_present_labels rrow)) in

      let (lfield_env', lrow_var') as lrow', lrec_row = unwrap_row lrow in
      let (rfield_env', rrow_var') as rrow', rrec_row = unwrap_row rrow in
        (*
 	  fail_on_absent_fields lfield_env;
	  fail_on_absent_fields rfield_env;
        *)
        if lrow_var' = rrow_var' then
          begin
	    if fields_are_compatible (lrow', rrow') then
	      let rec_env' =
	        (register_rec_rows
		   (lfield_env, lfield_env', lrec_row, rrow')
		   (rfield_env, rfield_env', rrec_row, lrow')
		   rec_env)
	      in
	        match rec_env' with
		  | None -> ()
		  | Some rec_env ->
		      unify_compatible_field_environments rec_env (lfield_env', rfield_env')
	    else
	      raise (Unify_failure ("Rigid rows\n "^ string_of_row lrow
				    ^"\nand\n "^ string_of_row rrow
				    ^"\n could not be unified because they have different fields"))
          end
        else
          raise (Unify_failure ("Rigid rows\n "^ string_of_row lrow
				^"\nand\n "^ string_of_row rrow
				^"\n could not be unified because they have distinct rigid row variables")) in

    let unify_both_rigid = unify_both_rigid_with_rec_env rec_env in

    let unify_one_rigid ((rigid_field_env, _ as rigid_row), (open_field_env, _ as open_row)) =
      let (rigid_field_env', rigid_row_var') as rigid_row', rigid_rec_row = unwrap_row rigid_row in
      let (open_field_env', open_row_var') as open_row', open_rec_row = unwrap_row open_row in 
	(* check that the open row contains no extra fields *)
        StringMap.iter
	  (fun label field_spec ->
	     if (StringMap.mem label rigid_field_env') then
	       ()
	     else
	       match field_spec with
		 | `Present _ ->
		     raise (Unify_failure
			      ("Rows\n "^ string_of_row rigid_row
			       ^"\nand\n "^ string_of_row open_row
			       ^"\n could not be unified because the former is rigid"
			       ^" and the latter contains fields not present in the former"))
		 | `Absent -> ()
	  ) open_field_env';
        
	(* check that the closed row contains no absent fields *)
        (*          fail_on_absent_fields closed_field_env; *)
	
	let rec_env' =
	  (register_rec_rows
	     (rigid_field_env, rigid_field_env', rigid_rec_row, open_row')
	     (open_field_env, open_field_env', open_rec_row, rigid_row')
	     rec_env)
	in
	  match rec_env' with
	    | None -> ()
	    | Some rec_env ->
		let open_extension = extend_field_env rec_env rigid_field_env' open_field_env' in
		  unify_row_var_with_row rec_env (open_row_var', (open_extension, rigid_row_var')) in

    let unify_both_open ((lfield_env, _ as lrow), (rfield_env, _ as rrow)) =
      let (lfield_env', lrow_var') as lrow', lrec_row = unwrap_row lrow in
      let (rfield_env', rrow_var') as rrow', rrec_row = unwrap_row rrow in
      let _ = assert(is_flattened_row rrow') in
      let rec_env' =
	(register_rec_rows
	   (lfield_env, lfield_env', lrec_row, rrow')
	   (rfield_env, rfield_env', rrec_row, lrow')
	   rec_env)
      in
      let _ = assert(is_flattened_row rrow') in
	match rec_env' with
	  | None -> ()
	  | Some rec_env ->
	      if (ITO.get_row_var lrow = ITO.get_row_var rrow) then     
		unify_both_rigid_with_rec_env rec_env ((lfield_env', `RowVar None), (rfield_env', `RowVar None))
	      else
		begin		
		  let fresh_row_var = ITO.fresh_row_variable() in	      
		    (* each row can contain fields missing from the other; 
		       thus we call extend_field_env once in each direction *)
		  let rextension =
		    extend_field_env rec_env lfield_env' rfield_env' in
		    (* [NOTE]
		       extend_field_env may change rrow_var' or lrow_var', as either
		       could occur inside the body of lfield_env' or rfield_env'
		    *)
		    unify_row_var_with_row rec_env (rrow_var', (rextension, fresh_row_var));
		    let lextension = extend_field_env rec_env rfield_env' lfield_env' in
		      unify_row_var_with_row rec_env (lrow_var', (lextension, fresh_row_var))
		end in
      
    (* report an error if an attempt is made to unify
       an unguarded recursive row with a row that is not
       unguarded recursive
    *)
    let check_unguarded_recursion lrow rrow =      
      if is_unguarded_recursive lrow then
        if not (is_unguarded_recursive rrow) then
	  raise (Unify_failure
		   ("Could not unify unguarded recursive row"^ string_of_row lrow
		    ^"\nwith row "^ string_of_row rrow))
        else if is_unguarded_recursive rrow then
	  raise (Unify_failure
		   ("Could not unify unguarded recursive row"^ string_of_row rrow
		    ^"\nwith row "^ string_of_row lrow)) in
      
    let _ =
      check_unguarded_recursion lrow rrow;

      if is_rigid_row lrow then
	if is_rigid_row rrow then
	  unify_both_rigid (lrow, rrow)
        else
	  unify_one_rigid (lrow, rrow)
      else if is_rigid_row rrow then
	unify_one_rigid (rrow, lrow)	    
      else
	unify_both_open (rrow, lrow)
    in
      Debug.if_set (show_row_unification)
	(fun () -> "Unified rows: " ^ (string_of_row lrow) ^ " and: " ^ (string_of_row rrow))

let unify alias_env (t1, t2) =
  unify' (IntMap.empty, IntMap.empty, alias_env) (t1, t2)
(* Debug.if_set (show_unification) (fun () -> "Unified types: " ^ string_of_datatype t1) *)
and unify_rows alias_env (row1, row2) =
  unify_rows' (IntMap.empty, IntMap.empty, alias_env) (row1, row2)

(** instantiate env var
    Get the type of `var' from the environment, and rename bound typevars.
 *)
let instantiate : environment -> string -> datatype = fun env var ->
  try
    let quantifiers, t = Inferencetypes.lookup var env in
      if quantifiers = [] then
	t
      else
	(
	  let _ = Debug.if_set (show_instantiation)
	    (fun () -> "Instantiating assumption: " ^ (string_of_assumption (quantifiers, t))) in

	  let tenv, renv = List.fold_left
	    (fun (tenv, renv) -> function
	       | `TypeVar var -> IntMap.add var (ITO.fresh_type_variable ()) tenv, renv
	       | `RigidTypeVar var -> IntMap.add var (ITO.fresh_type_variable ()) tenv, renv
	       | `RowVar var -> tenv, IntMap.add var (ITO.fresh_row_variable ()) renv
	    ) (IntMap.empty, IntMap.empty) quantifiers
	  in
	    instantiate_datatype (tenv, renv) t)
  with Not_found ->
    raise (UndefinedVariable ("Variable '"^ var ^"' does not refer to a declaration"))


(*
 get the quantifiers for a datatype
 i.e. the quantifiers required to close a datatype
 
 (the first argument specifies type variables that should remain free)
*)
let rec get_quantifiers : type_var_set -> datatype -> quantifier list = 
  fun bound_vars -> 
    function
      | `Not_typed -> failwith "Internal error: Not_typed encountered in get_quantifiers"
      | `Primitive _ -> []
      | `Recursive _
      | `RigidTypeVar _
      | `TypeVar _ -> assert false
      | `MetaTypeVar point ->
	  (match Unionfind.find point with
	     | `RigidTypeVar var
	     | `TypeVar var when IntSet.mem var bound_vars -> []
	     | `TypeVar var -> [`TypeVar var]
	     | `RigidTypeVar var -> [`RigidTypeVar var]
	     | `Recursive (var, body) ->
		 Debug.if_set (show_recursion) (fun () -> "rec (get_quantifiers): " ^(string_of_int var));
		 if IntSet.mem var bound_vars then
		   []
		 else
		   get_quantifiers (IntSet.add var bound_vars) body
	     | t -> get_quantifiers bound_vars t)
      | `Function (f, m, t) ->
          let from_gens = get_quantifiers bound_vars f
          and mailbox_gens = get_quantifiers bound_vars m
          and to_gens = get_quantifiers bound_vars t in
            unduplicate (=) (from_gens @ mailbox_gens @ to_gens)
      | `Record row
      | `Variant row 
      | `Table row -> get_row_quantifiers bound_vars row
      | `Application (_, args) ->
          unduplicate (=) (Utility.concat_map (get_quantifiers bound_vars) args)

and get_row_var_quantifiers : type_var_set -> row_var -> quantifier list =
  fun bound_vars ->
    function
      | `RowVar (None) -> []
      | `RecRowVar _
      | `RigidRowVar _
      | `RowVar (Some _) -> assert false
      | `MetaRowVar point ->
	  let field_env, row_var = Unionfind.find point in
	    if StringMap.is_empty field_env then
	      (match row_var with
		 | `RowVar (None) -> []
		 | `RigidRowVar var
		 | `RowVar (Some var) when IntSet.mem var bound_vars -> []
		 | `RigidRowVar var
		 | `RowVar (Some var) -> [`RowVar var]
		 | `RecRowVar (var, rec_row) ->
		     Debug.if_set (show_recursion) (fun () -> "rec (get_row_var_quantifiers): " ^(string_of_int var));
		     (if IntSet.mem var bound_vars then
			[]
		      else
			get_row_quantifiers (IntSet.add var bound_vars) rec_row)
		 | `MetaRowVar _ -> get_row_var_quantifiers bound_vars row_var)
	    else
	      get_row_quantifiers bound_vars (field_env, row_var)

and get_field_spec_quantifiers : type_var_set -> field_spec -> quantifier list =
    fun bound_vars ->
      function
	| `Present t -> get_quantifiers bound_vars t
	| `Absent -> []

and get_row_quantifiers : type_var_set -> row -> quantifier list =
    fun bound_vars (field_env, row_var) ->
      let field_vars = StringMap.fold
	(fun _ field_spec vars ->
	   get_field_spec_quantifiers bound_vars field_spec @ vars
	) field_env [] in
      let row_vars = get_row_var_quantifiers bound_vars (row_var:row_var)
      in
	field_vars @ row_vars

let env_type_vars env =
  concat_map (free_type_vars -<- snd) (Inferencetypes.environment_values env)

let type_mismatch ~expected ~inferred ~pos ~src msg =
  raise (Type_error (pos, 
                     src ^" has type "^string_of_datatype inferred
                     ^" but is annotated with type "
                     ^ string_of_datatype expected^"\n"^
                       msg))

(** generalise: 
    Universally quantify any free type variables in the expression.
*)
let generalise : environment -> datatype -> assumption = 
  fun env t ->
    let vars_in_env = intset_of_list (env_type_vars env) in
    let quantifiers = get_quantifiers vars_in_env t in
      Debug.if_set (show_generalisation) (fun () -> "Generalised: " ^ (string_of_assumption (quantifiers, t)));
      (quantifiers, t) 

type typing_environment = environment * alias_environment

let rec type_check : typing_environment -> untyped_expression -> expression =
  fun ((env, alias_env) as typing_env) expression ->
    let unify = unify alias_env
    and unify_rows = unify_rows alias_env in
  try
    Debug.if_set (show_typechecking) (fun () -> "Typechecking expression: " ^ (string_of_expression expression));
    match (expression : Syntax.untyped_expression) with
  | (Define (variable, _, _, `U pos) : Syntax.untyped_expression) -> nested_def pos variable
  | Boolean (value, `U pos) -> Boolean (value, `T (pos, `Primitive `Bool, None))
  | Integer (value, `U pos) -> Integer (value, `T (pos, `Primitive `Int, None))
  | Float (value, `U pos) -> Float (value, `T (pos, `Primitive `Float, None))
  | String (value, `U pos) -> String (value, `T (pos, string_type, None))
  | Char (value, `U pos) -> Char (value, `T (pos, `Primitive `Char, None))
  | Variable (name, `U pos) ->
      Variable (name, `T (pos, instantiate env name, None))
  | Apply (f, p, `U pos) ->
      let f = type_check typing_env f in
      let p = type_check typing_env p in
      let mb_type = instantiate env "_MAILBOX_" in
      let m = Variable ("_MAILBOX_", `T (pos, mb_type, None)) in
      let f_type = type_of_expression f in
      let p_type = type_of_expression p in
      let return_type = ITO.fresh_type_variable () in

      let _ =
	try unify (`Function(p_type, mb_type, return_type), f_type)
	with Unify_failure _ ->
          if Inferencetypes.using_mailbox_typing () then
            mistyped_application pos (f, f_type) (p, type_of_expression p) (Some (m, mb_type))
          else
            mistyped_application pos (f, f_type) (p, type_of_expression p) None
      in
	Apply (f, p, `T (pos, return_type, None))
  | Condition (if_, then_, else_, `U pos) ->
      let if_ = type_check typing_env if_ in
      let _ = (try unify (type_of_expression if_, `Primitive `Bool)
               with Unify_failure _ -> mistype (pos_of_expression if_) (if_, type_of_expression if_) (`Primitive `Bool)) in
      let then_expr = type_check typing_env then_ in
      let else_expr = type_check typing_env else_ in
      let _ = try 
        unify (type_of_expression then_expr, type_of_expression else_expr)
          (* FIXME: This can't be right!*)
      with _ ->         
        unify (type_of_expression else_expr, type_of_expression then_expr) in
      let node' = Condition (if_, 
                             then_expr,
                             else_expr,
                             `T (pos, 
                               type_of_expression then_expr,
                               None
                             )) in
        node'
  | Comparison (l, oper, r, `U pos) ->
      let l = type_check typing_env l in
      let r = type_check typing_env r in
	unify (type_of_expression l, type_of_expression r);
        Comparison (l, oper, r, `T (pos, `Primitive `Bool, None))
  | Abstr (variable, body, `U pos) ->
      begin
	let variable_type = ITO.fresh_type_variable () in
        let mb_type = ITO.fresh_type_variable () in
	let body_env = (variable, ([], variable_type)) :: ("_MAILBOX_", ([], mb_type)) :: env in
	let body = type_check (body_env, alias_env) body in
	let type' = `Function (variable_type, mb_type, type_of_expression body) in
	  Abstr (variable, body, `T (pos, type', None))
      end
  | Let (variable, value, body, `U pos) ->
      let value = type_check typing_env value in
      let vtype = (if is_value value then (generalise env (type_of_expression value))
                   else ([], type_of_expression value)) in
      let body = type_check (((variable, vtype) :: env), alias_env) body in
	Let (variable, value, body, `T (pos, type_of_expression body, None))
  | Rec (variables, body, `U pos) ->
      let best_typing_env, vars = type_check_mutually typing_env variables in
      let body = type_check best_typing_env body in
	Rec (vars, body, `T (pos, type_of_expression body, None))
  | Xml_node (tag, atts, cs, `U pos) as xml -> 
      let separate = partition (is_special -<- fst) in
      let (special_attrs, nonspecial_attrs) = separate atts in
      let bindings = 
(*         try *)
          lname_bound_vars xml 
(*         with InvalidLNameExpr ->  *)
(*           raise UndefinedVariable "Invalid l:name parameter " ^ string_of_expression  *)
      in
        (* "event" is always in scope for the event handlers *)
      let attr_env = ("event", ([], `Application ("Event", []))) :: env in
(* should now use alien javascript jslib : ... to import library functions *)
(*      let attr_env = ("jslib", ([], `Record(ITO.make_empty_open_row()))) :: attr_env in *)
        (* extend the env with each l:name bound variable *)
      let attr_env = 
	("_MAILBOX_", ([], ITO.fresh_type_variable ())) ::
          fold_right (fun s env -> (s, ([], string_type)) :: env) bindings attr_env in
      let special_attrs = map (fun (name, expr) -> (name, type_check (attr_env, alias_env) expr)) special_attrs in
        (* Check that the bound expressions have type 
           <strike>XML</strike> unit. *)
(*      let _ =
	List.iter (fun (_, expr) -> unify(type_of_expression expr, ITO.fresh_type_variable ()(*Types.xml*))) special_attrs in*)
      let contents = map (type_check typing_env) cs in
      let nonspecial_attrs = map (fun (k,v) -> k, type_check typing_env v) nonspecial_attrs in
(*      let attr_type = if islhref xml then Types.xml else Types.string_type in *)
      let attr_type = string_type in
        (* force contents to be XML, attrs to be strings *)
      let _ = List.iter (fun node -> unify (type_of_expression node, xml_type)) contents in
      let _ = List.iter (fun (_, node) -> unify (type_of_expression node, attr_type)) nonspecial_attrs in
      let trimmed_node =
        Xml_node (tag, 
                  nonspecial_attrs,         (* +--> up here I mean *)
                  contents,                 (* | *)
                  `T (pos, xml_type, None))
      in                                    (* | *)
        (* could just tack these on up there --^ *)
        add_attrs special_attrs trimmed_node
  | Record_intro (bs, r, `U pos) ->
      let bs, field_env =
        StringMap.fold (fun label e (bs, field_env)  ->
                          let e = type_check typing_env e in
                          let t = type_of_expression e in
                            StringMap.add label e bs, StringMap.add label (`Present t) field_env)
          bs (StringMap.empty, StringMap.empty)
      in
        begin
          match r with
            | None ->
                Record_intro (bs, None, `T (pos, `Record (field_env, `RowVar None), None))
            | Some r ->
                let r = type_check typing_env r in
                let rtype = type_of_expression r in
                  (* make sure rtype is a record type! *)
                  unify(rtype, `Record (ITO.make_empty_open_row ()));
                  
                  let (rfield_env, rrow_var), _ = unwrap_row (extract_row rtype) in
                    
                  (* attempt to extend field_env with the labels from rfield_env
                     i.e. all the labels belonging to the record r
                  *)
                  let field_env' =
                    StringMap.fold (fun label t field_env' ->
                                      match t with
                                        | `Absent ->
                                            if StringMap.mem label field_env then
                                              field_env'
                                            else
                                              StringMap.add label `Absent field_env'
                                        | `Present _ ->
                                            if StringMap.mem label field_env then
                                              failwith ("Could not extend record "^string_of_expression r^" (of type "^
                                                          string_of_datatype rtype^") with "^
                                                          string_of_expression
                                                          (Record_intro (bs, None, `T (pos, `Record (field_env, `RowVar None), None)))^
                                                          " (of type"^string_of_datatype (`Record (field_env, `RowVar None))^
                                                          ") because the labels overlap")
                                            else
                                              StringMap.add label t field_env') rfield_env field_env in
                    Record_intro (bs, Some r, `T (pos, `Record (field_env', rrow_var), None))                  
        end           
  | Record_selection (label, label_variable, variable, value, body, `U pos) ->
      let value = type_check typing_env value in
      let label_variable_type = ITO.fresh_type_variable () in
	unify (type_of_expression value, `Record (ITO.make_singleton_open_row (label, `Present (label_variable_type))));

	let value_row = extract_row (type_of_expression value) in
	let label_var_equiv = label_variable, ([], label_variable_type) in
	let var_equiv = variable, ([], `Record (ITO.set_field (label, `Absent) value_row)) in
	  
	let body_env = label_var_equiv :: var_equiv :: env in
	let body = type_check (body_env, alias_env) body in
	let body_type = type_of_expression body in
	  Record_selection (label, label_variable, variable, value, body, `T (pos, body_type, None))
  | Project (expr, label, `U pos) ->
      let expr = type_check typing_env expr in
      let label_variable_type = ITO.fresh_type_variable () in
	unify (type_of_expression expr, `Record (ITO.make_singleton_open_row (label, `Present (label_variable_type))));
        Project (expr, label, `T (pos, label_variable_type, None))
  | Erase (value, label, `U pos) ->
      let value = type_check typing_env value in
      let label_variable_type = ITO.fresh_type_variable () in
      let value_row = extract_row (type_of_expression value) in
        Erase (value, label, `T (pos, `Record (ITO.set_field (label, `Absent) value_row), None))
  | Variant_injection (label, value, `U pos) ->
      let value = type_check typing_env value in
      let type' = `Variant (ITO.make_singleton_open_row (label, `Present (type_of_expression value))) in
        Variant_injection (label, value, `T (pos, type', None))
  | Variant_selection (value, case_label, case_variable, case_body, variable, body, `U pos) ->
      let value = type_check typing_env value in
      let value_type = type_of_expression value in
      
      let case_var_type = ITO.fresh_type_variable() in
      let body_row = ITO.make_empty_open_row () in
      let variant_type = `Variant (ITO.set_field (case_label, `Present case_var_type) body_row) in
	unify (variant_type, value_type);

	let case_body = type_check (((case_variable, ([], case_var_type)) :: env), alias_env) case_body in

	(*
           We take advantage of absence information to give a more refined type when
           the variant does not match the label i.e. inside 'body'.

           This allows us to type functions such as the following which fail to
           typecheck in OCaml!

            fun f(x) {
             switch x {
              case A(B) -> B;
              case A(y) -> A(f(y));
             }
            }
           
           On the right-hand-side of the second case y is assigned the type:
             [|B - | c|]
           which unifies with the argument to f whose type is:
             [|A:[|B:() | c|] |]
           as opposed to:
             [|B:() | c|]
           which clearly doesn't!
        *)
	let body_var_type = `Variant (ITO.set_field (case_label, `Absent) body_row) in
	let body = type_check (((variable, ([], body_var_type)) :: env), alias_env) body in

	let case_type = type_of_expression case_body in
	let body_type = type_of_expression body in
	  unify (case_type, body_type);
	  Variant_selection (value, case_label, case_variable, case_body, variable, body, `T (pos, body_type, None))
  | Variant_selection_empty (value, `U pos) ->
      let value = type_check typing_env value in
      let new_row_type = `Variant (ITO.make_empty_closed_row()) in
        unify(new_row_type, type_of_expression value);
        Variant_selection_empty (value, `T (pos, ITO.fresh_type_variable (), None))
  | Nil (`U pos) ->
      Nil (`T (pos, `Application ("List", [ITO.fresh_type_variable ()]), None))
  | List_of (elem, `U pos) ->
      let elem = type_check typing_env elem in
	List_of (elem,
		 `T (pos, `Application ("List", [type_of_expression elem]), None))
  | Concat (l, r, `U pos) ->
      let tvar = ITO.fresh_type_variable () in
      let l = type_check typing_env l in
	unify (type_of_expression l, `Application ("List", [tvar]));
	let r = type_check typing_env r in
	  unify (type_of_expression r, type_of_expression l);
	  let type' = `Application ("List", [tvar]) in
	    Concat (l, r, `T (pos, type', None))
  | For (expr, var, value, `U pos) ->
      let value_tvar = ITO.fresh_type_variable () in
      let expr_tvar = ITO.fresh_type_variable () in
      let value = type_check typing_env value in
	unify (type_of_expression value, `Application ("List", [value_tvar]));
	let expr_env = (var, ([], value_tvar)) :: env in
	let expr = type_check (expr_env, alias_env) expr in
	  unify (type_of_expression expr, `Application ("List", [expr_tvar]));
	  let type' = type_of_expression expr in
	    For (expr, var, value, `T (pos, type', None))
  | Call_cc(arg, `U pos) -> 
      (* TBD: Make this a primitive function (need to pass c.c. to prims). *)
      let arg = type_check typing_env arg in
      let contrettype = ITO.fresh_type_variable () in
      let anytype = ITO.fresh_type_variable () in
        (* It'd be better if this mailbox didn't intrude here.
           Perhaps there's some rewrite rule for `escape' that we
           could use instead. 
           (FIXME: What does this mean? --eekc 1/07)*)
      let mailboxtype = 
	if Inferencetypes.using_mailbox_typing () then
          instantiate env "_MAILBOX_" 
        else
          ITO.fresh_type_variable () in
      let conttype =
        `Function (contrettype, mailboxtype, anytype) in
      let argtype = `Function (conttype, mailboxtype, contrettype) in
        unify (argtype, type_of_expression arg);
        Call_cc(arg, `T (pos, contrettype, None))
  | Database (params, `U pos) ->
      let params = type_check typing_env params in
        unify (type_of_expression params, db_descriptor_type);
        Database (params, `T (pos, `Primitive `DB, None))
  | TableQuery (ths, query, `U pos) ->
      let row =
	(List.fold_right
	   (fun col env -> 
              match col with 
                | Left col -> StringMap.add col.Query.name (`Present col.Query.col_type) env
                | Right _ -> env)
	   query.Query.result_cols StringMap.empty, `RowVar None) in
      let datatype =  `Application ("List", [`Record row]) in
      let row' = ITO.make_empty_open_row () in
      let ths = alistmap (type_check typing_env) ths
      in
        Utility.for_each ths 
          (fun _, th -> 
             unify (type_of_expression th, `Table row'));
	unify_rows (row, row');
        TableQuery (ths, query, `T (pos, datatype, None))
  | TableHandle (db, tableName, row, `U pos) ->
      let datatype =  `Table row in
      let db = type_check typing_env db in
      let tableName = type_check typing_env tableName in
	unify (type_of_expression db, `Primitive `DB);
	unify (type_of_expression tableName, string_type); 
        TableHandle (db, tableName, row, `T (pos, datatype, None))
  | SortBy(expr, byExpr, `U pos) ->
      (* FIXME: the byExpr is typed freely as yet. It could have any
         orderable type, of which there are at least several. How to
         resolve this? Would kill for type classes. *)
      let byExpr = type_check typing_env byExpr in
      let expr = type_check typing_env expr in
        SortBy(expr, byExpr, `T (pos, type_of_expression expr, None))
  | Wrong (`U pos) ->
      Wrong(`T (pos, ITO.fresh_type_variable(), None))
  | HasType(expr, datatype, `U pos) ->
      let expr = type_check typing_env expr in
      let expr_type = type_of_expression expr in
      let inference_datatype = datatype in
        free_alias_check alias_env inference_datatype;
        begin
          try unify(expr_type, inference_datatype);
          with Unify_failure msg -> 
            let _,_,src = position expr in
            type_mismatch
              ~expected:inference_datatype
              ~inferred:expr_type
              ~src:src
              ~pos:pos msg
        end;
	HasType(expr, datatype, `T (pos, inference_datatype, None))
  | TypeDecl _ ->
      failwith "Type declarations only supported at top-level"
  | Placeholder _ 
  | Alien _ ->
      assert(false)
 with 
     Unify_failure msg
   | UndefinedVariable msg
   | UndefinedAlias msg ->
       raise (Type_error(position expression, msg))
         (* end "type_check" *)

(** type_check_mutually
    Companion to "type_check"; does mutual type-inference

    [QUESTIONS]
      - what are the constraints on the definitions?
      - do the functions have to be recursive?
*)
and
    type_check_mutually (env, alias_env) (defns : (string * untyped_expression * Inferencetypes.datatype option) list) =
      let var_env = (map (fun (name, _, t) ->
                            match t with
                              | Some t ->
                                  (name, generalise env t)
                              | None -> (name, ([], ITO.fresh_type_variable ())))
		       defns) in
      let inner_env = var_env @ env in
      let type_check result (name, expr, t) =
        let expr = type_check (inner_env, alias_env) expr in
          match type_of_expression expr with
            | `Function _ as f  ->
		unify alias_env (snd (assoc name var_env), f);
		(name, expr, t) :: result
            | datatype -> Errors.letrec_nonfunction (pos_of_expression expr) (expr, datatype) in

      let defns = fold_left type_check [] defns in
      let defns = rev defns in

      let env = (List.map (fun (name, value,_) -> 
			     (name, generalise env (type_of_expression value))) defns
		 @ env)
      in
        (env, alias_env), defns     

let mutually_type_defs
    ((env, alias_env) : Inferencetypes.typing_environment)
    (defs : (string * untyped_expression * 'a option) list)
    : (Inferencetypes.typing_environment * (string * expression * 'c) list) =
  let (new_type_env, new_alias_env), new_defs = type_check_mutually (env, alias_env) defs
  in
    ((new_type_env, new_alias_env), new_defs)

let register_alias (typename, vars, datatype, pos) (typing_env, alias_env) =
  let _ =
    if StringMap.mem typename alias_env then
      failwith ("Duplicate typename: "^typename) in
  let aliases = Inferencetypes.type_aliases datatype in
  let free_aliases =
    StringSet.filter (fun alias -> not (StringMap.mem alias alias_env)) aliases
  in
    if not (StringSet.is_empty free_aliases) then
      failwith ("Undefined typename(s) in type declaration: "^String.concat "," (StringSet.elements free_aliases))
    else
      StringMap.add typename ((List.map (fun var -> `TypeVar var) vars), datatype) alias_env

let type_expression : Inferencetypes.typing_environment -> untyped_expression -> (Inferencetypes.typing_environment * expression) =
  fun (env, alias_env) untyped_expression ->
    let (env', alias_env'), exp' =
      match untyped_expression with
	| Define (variable, value, loc, `U pos) ->
	    let value = type_check (env, alias_env) value in
	    let value_type = if is_value value then 
              (generalise env (type_of_expression value))
            else [], type_of_expression value in
              (((variable, value_type) :: env), alias_env),
    	       Define (variable, value, loc, `T (pos, type_of_expression value, None))
        | TypeDecl (typename, vars, datatype, `U pos) ->
            (env,
             register_alias (typename, vars, datatype, pos) (env, alias_env)),
            TypeDecl (typename, vars, datatype, `T (pos, `Record (ITO.make_empty_closed_row ()), None))
        | Alien (language, name, assumption, `U pos)  ->
            let (qs, k) = assumption
            in
              (((name, (qs, k)) :: env), alias_env), Alien (language, name, assumption, `T (pos, k, None))
	| expr -> let value = type_check (env, alias_env) expr in (env, alias_env), value
    in
      (env', alias_env'), exp'

let type_program
    (typing_env : Inferencetypes.typing_environment)
    (exprs : untyped_expression list) :
    Inferencetypes.typing_environment * expression list =
  let type_group (typing_env, typed_exprs) : untyped_expression list ->
    Inferencetypes.typing_environment * expression list = function
      | [x] -> (* A single node *)
	  let typing_env, expression = type_expression typing_env x in 
            typing_env, typed_exprs @ [expression]
      | xs  -> (* A group of potentially mutually-recursive definitions *)
          let defparts = map (fun (Define x) -> x) xs in
          let defbodies = map (fun (name, Rec ([(_, expr, t)], _, _), _, _) -> 
                                 name, expr, t) defparts in
          let (typing_env : Inferencetypes.typing_environment), defs = mutually_type_defs typing_env defbodies in
          let defs = (map2 (fun (name, _, location, _) (_, expr, _) -> 
                              Define(name, expr, location, expression_data expr))
			defparts defs) in
            typing_env, typed_exprs @ defs

    and bothdefs l r = match l, r with
      | Define (_, Rec _, _, _), Define (_, Rec _, _, _) -> true
      | _ ->  false
    in
    let def_seqs = groupBy bothdefs exprs in
    let mutrec_groups = (Callgraph.refine_def_groups def_seqs) in
      fold_left type_group (typing_env, []) mutrec_groups

(* Check for duplicate top-level definitions.  This probably shouldn't
   appear in the type inference module.

   (Duplicate top-level definitions are simply not allowed.)

   In future we should probably allow duplicate top-level definitions, but
   only if we implement the correct semantics!
*)
let check_for_duplicate_defs 
    (type_env, _)
    (expressions :  untyped_expression list) =
  let check (env, defined) = function
    | Define (name, _, _, `U position) when StringMap.mem name defined ->
        (env, StringMap.add name (position :: StringMap.find name defined) defined)
    | Define (name, _, _, `U position) when StringSet.mem name env ->
        (env, StringMap.add name [position] defined)
    | Define (name, _, _, _) ->
        (StringSet.add name env, defined)
    | _ -> 
        (env, defined) in 
  let env = List.fold_right (fst ->- StringSet.add) type_env StringSet.empty in
  let _, duplicates = List.fold_left check (env,StringMap.empty) expressions in
    if not (StringMap.is_empty duplicates) then
      raise (Errors.MultiplyDefinedToplevelNames duplicates)

(* [HACKS] *)
(* two pass typing: yuck! *)
let type_program typing_env expressions = 
  check_for_duplicate_defs typing_env expressions;
  let _ =
    (* without mailbox parameters *)
    Debug.if_set (show_typechecking) (fun () -> "Typechecking program without mailbox parameters");
    Inferencetypes.with_mailbox_typing false
      (fun () ->
	 type_program typing_env expressions)
  in
    (* with mailbox parameters *)
    Debug.if_set (show_typechecking) (fun () -> "Typechecking program with mailbox parameters");
    Inferencetypes.with_mailbox_typing true
      (fun () ->
	 type_program typing_env expressions)

let type_expression typing_env expression =
  check_for_duplicate_defs typing_env [expression];
  let _ =
    (* without mailbox parameters *)	
    Debug.if_set (show_typechecking) (fun () -> "Typechecking expression without mailbox parameters");
    Inferencetypes.with_mailbox_typing false
      (fun () ->
	 type_expression typing_env expression)
  in
    (* with mailbox parameters *)
    Debug.if_set (show_typechecking) (fun () -> "Typechecking expression with mailbox parameters");
    Inferencetypes.with_mailbox_typing true
      (fun () ->
	 type_expression typing_env expression)
