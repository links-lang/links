open List

open Utility
open Debug
open Syntax
open Inferencetypes
open Forms
open Errors

(* debug flags *)
let show_unification = Settings.add_bool("show_unification", false, true)
let show_row_unification = Settings.add_bool("show_row_unification", false, true)

let show_instantiation = Settings.add_bool("show_instantiation", false, true)
let show_generalization = Settings.add_bool("show_generalization", false, true)

let show_typechecking = Settings.add_bool("show_typechecking", false, true)
let show_recursion = Settings.add_bool("show_recursion", false, true)

let rigid_type_variables = Settings.add_bool("rigid_type_variables", true, true)

(* whether to allow negative recursive types to be inferred *)
let infer_negative_types = Settings.add_bool("infer_negative_types", true, true)

exception Unify_failure of string
exception UndefinedVariable of string

module ITO = InferenceTypeOps

let db_descriptor_type =
  inference_type_of_type
    (Inferencetypes.empty_var_maps ())
    (snd (Parse.parse_datatype "(driver:String, name:String, args:String)"))

(* extract data from inference_expressions *)
let type_of_expression : inference_expression -> datatype =
  fun exp -> let _, t, _ = expression_data exp in t
let pos_of_expression : inference_expression -> position =
  fun exp -> let pos, _, _ = expression_data exp in pos

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
    (match (t1, t2) with
       | `Not_typed, `Not_typed -> true
       | `Primitive x, `Primitive y -> x = y
       | `MetaTypeVar lpoint, `MetaTypeVar rpoint ->
	   Unionfind.equivalent lpoint rpoint
       | `Function (lfrom, lto), `Function (rfrom, rto) ->
	   eq_types (lfrom, rfrom) && eq_types (lto, rto)
       | `Record l, `Record r -> eq_rows (l, r)
       | `Variant l, `Variant r -> eq_rows (l, r)
       | `List t, `List t' -> eq_types (t, t')
       | `Mailbox t, `Mailbox t' -> eq_types (t, t')
       | _, _ -> false)
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
  unification environment:
    for stopping cycles during unification
*)
type unify_type_env = (datatype list) IntMap.t
type unify_row_env = (row list) IntMap.t
type unify_env = unify_type_env * unify_row_env


let rec unify' : unify_env -> (datatype * datatype) -> unit = fun rec_env ->
  let rec_types, rec_rows = rec_env in

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
	 unify' (IntMap.add var (t::ts) rec_types, rec_rows) (body, t) in

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
	unify' ((IntMap.add lvar (rbody::lts) ->- IntMap.add rvar (lbody::rts)) rec_types, rec_rows) (lbody, rbody) in

  (* introduce a recursive type
       give an error if it is non-well-founded and
       non-well-founded type inference is switched off
  *)
  let rec_intro point (var, t) =
    if Settings.get_value infer_negative_types || not (is_negative var t) then
       Unionfind.change point (`Recursive (var, t))
    else
       failwith "non-well-founded type inferred!" in
    
    fun (t1, t2) ->
      (debug_if_set (show_unification) (fun () -> "Unifying "^string_of_datatype t1^" with "^string_of_datatype t2);
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
		   (if var_is_free_in_type var t then
		      (debug_if_set (show_recursion) (fun () -> "rec intro1 (" ^ (string_of_int var) ^ ")");
		       rec_intro rpoint (var, t))
		    else
		      ());
		   Unionfind.union lpoint rpoint
	       | t, `TypeVar var ->
		   (if var_is_free_in_type var t then
		      (debug_if_set (show_recursion) (fun () -> "rec intro2 (" ^ (string_of_int var) ^ ")");
		       rec_intro lpoint (var, t))
		    else
		      ());
		   Unionfind.union rpoint lpoint
               | `RigidTypeVar l, t
               | t, `RigidTypeVar l -> 
                     raise (Unify_failure ("Couldn't unify the rigid type variable "^ string_of_int l ^" with the type "^ string_of_datatype t))
	       | `Recursive (lvar, t), `Recursive (rvar, t') ->
		   assert(lvar <> rvar);
		   debug_if_set (show_recursion)
		     (fun () -> "rec pair (" ^ (string_of_int lvar) ^ "," ^ (string_of_int rvar) ^")");
		   unify_rec2 ((lvar, t), (rvar, t'));
		   Unionfind.union lpoint rpoint
	       | `Recursive (var, t'), t ->
		   debug_if_set (show_recursion) (fun () -> "rec left (" ^ (string_of_int var) ^ ")");
		   unify_rec ((var, t'), t);
		   Unionfind.union rpoint lpoint
	       | t, `Recursive (var, t')->
		   debug_if_set (show_recursion) (fun () -> "rec right (" ^ (string_of_int var) ^ ")");
		   unify_rec ((var, t'), t);
		   Unionfind.union lpoint rpoint
	       | t, t' -> unify' rec_env (t, t'); Unionfind.union lpoint rpoint)
      | `MetaTypeVar point, t | t, `MetaTypeVar point ->
	  (match (Unionfind.find point) with
             | `RigidTypeVar l -> 
                 raise (Unify_failure ("Couldn't unify the rigid type variable "^ string_of_int l ^" with the type "^ string_of_datatype t))
	     | `TypeVar var ->
		 if var_is_free_in_type var t then
   		   (let _ = debug_if_set (show_recursion)
		      (fun () -> "rec intro3 ("^string_of_int var^","^string_of_datatype t^")") in
		      rec_intro point (var, t))
		 else
		   (debug_if_set (show_recursion) (fun () -> "non-rec intro (" ^ string_of_int var ^ ")");
		   Unionfind.change point t)
	     | `Recursive (var, t') ->
   		 debug_if_set (show_recursion) (fun () -> "rec single (" ^ (string_of_int var) ^ ")");
		 unify_rec ((var, t'), t)
		 (* It's tempting to try to do this, but it isn't sound
		    as point may appear inside t
		 
		    Unionfind.change point t;
		 *)
	     | t' -> unify' rec_env (t, t'))
      | `Function (lmbvar, `Function(lvar, lbody)), `Function (rmbvar, `Function (rvar, rbody))
	  when Types.using_mailbox_typing () ->
	  (unify' rec_env (lmbvar, rmbvar);
           unify' rec_env (lvar, rvar);
           unify' rec_env (lbody, rbody))
      | `Function (lvar, lbody), `Function (rvar, rbody) ->
	  (if Types.using_mailbox_typing() then
	     Debug.debug "mailbox typing assertion failure"
	   else
	     ());
            unify' rec_env (lvar, rvar);
            unify' rec_env (lbody, rbody)
      | `Record l, `Record r -> unify_rows' rec_env (l, r)
      | `Variant l, `Variant r -> unify_rows' rec_env (l, r)
      | `Table l, `Table r -> unify_rows' rec_env (l, r)
      | `List t, `List t' -> unify' rec_env (t, t')
      | `Mailbox t, `Mailbox t' -> unify' rec_env (t, t')
      | _, _ ->
          raise (Unify_failure ("Couldn't match "^ string_of_datatype t1 ^" against "^ string_of_datatype t2)));
       debug_if_set (show_unification) (fun () -> "Unified types: " ^ string_of_datatype t1)
      )

and unify_rows' : unify_env -> ((row * row) -> unit) = 
  fun rec_env (lrow, rrow) ->
      debug_if_set (show_row_unification) (fun () -> "Unifying row: " ^ (string_of_row lrow) ^ " with row: " ^ (string_of_row rrow));

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

	unify_row_var_with_row rec_env (row_var, extension_row)
          precondition:
            extension_row = (_, extension_row_var) and             
            extension_row_var is 'canonical', i.e. of the form
                `RowVar None 
              | `MetaRowVar (field_env, `RowVar (Some _))
              | `MetaRowVar (field_env, `RigidRowVar _)
            with
              StringMap.is_empty field_env
      *)
      let unify_row_var_with_row : unify_env -> row_var * row -> unit =
	fun rec_env (row_var, ((extension_field_env, extension_row_var) as extension_row)) ->
          assert (is_canonical_row_var extension_row_var);

          let unify_closed_row_var : row_var -> unit = function
            | `RowVar None ->
                ()
            | `MetaRowVar point ->
                begin
                  match Unionfind.find point with
                    | (field_env, `RowVar (Some _)) ->
                        Unionfind.change point (field_env, `RowVar None)
                    | (_, `RigidRowVar _) ->
                        raise (Unify_failure ("Closed row cannot be unified with rigid open row\n"))
                    | _ -> assert false
                end
            | _ -> assert false in

          let unify_rigid_row_var var : row_var -> unit = function
            | `RowVar None ->
		raise (Unify_failure ("Rigid row cannot be extended with empty closed row\n"))
            | `MetaRowVar point ->
                begin
                  match Unionfind.find point with
                    | (field_env, `RowVar (Some _)) ->
                        Unionfind.change point (field_env, `RigidRowVar var)
                    | (_, `RigidRowVar var') ->
                        if var=var' then
                          ()
                        else
                          raise (Unify_failure ("Incompatible rigid row variables cannot be unified\n"))
                    | _ -> assert false
                end
            | _ -> assert false in

	  let rec extend = function
	    | `MetaRowVar point ->
		(* point should be a row variable *)
		let (field_env, row_var) as row = Unionfind.find point in
		  if StringMap.is_empty field_env then
		    begin
		      match row_var with
			| `RowVar None ->
                            if StringMap.is_empty extension_field_env then
                              unify_closed_row_var extension_row_var
                            else
			      raise (Unify_failure ("Closed row cannot be extended with non-empty row\n"
						    ^string_of_row extension_row))
			| `RigidRowVar var ->
                            if StringMap.is_empty extension_field_env then
                              unify_rigid_row_var var extension_row_var
                            else
			      raise (Unify_failure ("Rigid row variable cannot be unified with non-empty row\n"
						    ^string_of_row extension_row))
			| `RowVar (Some var) ->
			    if mem var (free_row_type_vars extension_row) then
			      rec_row_intro point (field_env, var, extension_row)
			    else
			      Unionfind.change point extension_row
			| (`RecRowVar _) as row_var ->
			    unify_rows' rec_env ((StringMap.empty, row_var), extension_row)
			| `MetaRowVar _ -> assert false
		    end
		  else
		    unify_rows' rec_env (row, extension_row)
	    | `RowVar None ->
                if StringMap.is_empty extension_field_env then
                  unify_closed_row_var extension_row_var
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
	fun ((rec_types, rec_rows) as rec_env) ->
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
		    Some (rec_types, IntMap.add var (restricted_row::rs) rec_rows)
	    | None -> 
	        Some (rec_env) in

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
        
      let _ =
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
	debug_if_set (show_row_unification)
	  (fun () -> "Unified rows: " ^ (string_of_row lrow) ^ " and: " ^ (string_of_row rrow))

let unify (t1, t2) =
  unify' (IntMap.empty, IntMap.empty) (t1, t2)
(* debug_if_set (show_unification) (fun () -> "Unified types: " ^ string_of_datatype t1) *)
and unify_rows (row1, row2) =
  unify_rows' (IntMap.empty, IntMap.empty) (row1, row2)

(*
  instantiation environment:
    for stopping cycles during instantiation
*)
type inst_type_env = (datatype Unionfind.point) IntMap.t
type inst_row_env = (row Unionfind.point) IntMap.t
type inst_env = inst_type_env * inst_row_env

(** instantiate env var
    Get the type of `var' from the environment, and rename bound typevars.
 *)
let instantiate : environment -> string -> datatype = fun env var ->
  try
    let generics, t = Type_basis.lookup var env in
      if generics = [] then
	t
      else
	(
	  let _ = debug_if_set (show_instantiation)
	    (fun () -> "Instantiating assumption: " ^ (string_of_assumption (generics, t))) in

	  let tenv, renv = List.fold_left
	    (fun (tenv, renv) -> function
	       | `TypeVar var -> IntMap.add var (ITO.fresh_type_variable ()) tenv, renv
	       | `RowVar var -> tenv, IntMap.add var (ITO.fresh_row_variable ()) renv
	    ) (IntMap.empty, IntMap.empty) generics in
	    
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
				 (*			`MetaTypeVar (Unionfind.fresh (inst rec_vars t)) *)
			 | `Recursive (var, t) ->
			     debug_if_set (show_recursion) (fun () -> "rec (instantiate)1: " ^(string_of_int var));

			     if IntMap.mem var rec_type_env then
			       (`MetaTypeVar (IntMap.find var rec_type_env))
			     else
			       (
				 let var' = Type_basis.fresh_raw_variable () in
				 let point' = Unionfind.fresh (`TypeVar var') in
				 let t' = inst (IntMap.add var point' rec_type_env, rec_row_env) t in
				 let _ = Unionfind.change point' (`Recursive (var', t')) in
				   `MetaTypeVar point'
			       )

			 | _ -> inst rec_env t)
		| `Function (var, body) -> `Function (inst rec_env var, inst rec_env body)
		| `Record row -> `Record (inst_row rec_env row)
		| `Variant row ->  `Variant (inst_row rec_env row)
		| `Table row -> `Table (inst_row rec_env row)
		| `List (elem_type) ->
		    `List (inst rec_env elem_type)
		| `Mailbox (elem_type) ->
		    `Mailbox (inst rec_env elem_type)
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
(*	    debug ("Instantiating row var: "^string_of_row_var row_var);*)
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
			     let var' = Type_basis.fresh_raw_variable () in
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
	    inst (IntMap.empty, IntMap.empty) t)
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
	     | `RigidTypeVar var
	     | `TypeVar var -> [`TypeVar var]
	     | `Recursive (var, body) ->
		 debug_if_set (show_recursion) (fun () -> "rec (get_quantifiers): " ^(string_of_int var));
		 if IntSet.mem var bound_vars then
		   []
		 else
		   get_quantifiers (IntSet.add var bound_vars) body
	     | t -> get_quantifiers bound_vars t)
      | `Function (var, body) ->
          let var_gens = get_quantifiers bound_vars var
          and body_gens = get_quantifiers bound_vars body in
            unduplicate (=) (var_gens @ body_gens)
      | `Record row
      | `Variant row 
      | `Table row -> get_row_quantifiers bound_vars row
      | `List (elem_type) ->
	  get_quantifiers bound_vars elem_type
      | `Mailbox (elem_type) ->
	  get_quantifiers bound_vars elem_type

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
		     debug_if_set (show_recursion) (fun () -> "rec (row_generics): " ^(string_of_int var));
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


(** generalize: 
    Universally quantify any free type variables in the expression.
*)
let generalize : environment -> datatype -> assumption = 
  fun env t ->
    let vars_in_env = concat_map (free_type_vars -<- snd) (Type_basis.environment_values env) in
    let bound_vars = intset_of_list vars_in_env in
    let quantifiers = get_quantifiers bound_vars t in
      debug_if_set (show_generalization) (fun () -> "Generalized: " ^ (string_of_assumption (quantifiers, t)));
      (quantifiers, t)

(*
  [SUGGESTION]
    rather than threading both var_maps and env through all of the type checking
    functions we could incorporate var_maps into the environment type
*)

let rec type_check : inference_type_map -> environment -> untyped_expression -> inference_expression =
  fun var_maps env expression -> let type_check = type_check var_maps in
  try
    debug_if_set (show_typechecking) (fun () -> "Typechecking expression: " ^ (string_of_expression expression));
    match expression with
  | Define (variable, _, _, pos) -> nested_def pos variable
  | Boolean (value, pos) -> Boolean (value, (pos, `Primitive `Bool, None))
  | Integer (value, pos) -> Integer (value, (pos, `Primitive `Int, None))
  | Float (value, pos) -> Float (value, (pos, `Primitive `Float, None))
  | String (value, pos) -> String (value, (pos, `List (`Primitive `Char), None))
  | Char (value, pos) -> Char (value, (pos, `Primitive `Char, None))
  | Variable (name, pos) ->
      Variable (name, (pos, instantiate env name, None))
  | Apply (Apply (f, mb, inner_pos), p, pos) when Types.using_mailbox_typing () ->
      let f = type_check env f in
      let mb = type_check env mb in
      let p = type_check env p in
	
      let f_type = type_of_expression f in
      let mb_type = type_of_expression mb in
      let p_type = type_of_expression p in

      let return_type = ITO.fresh_type_variable () in
	
      let _ =
	try
	  unify (`Function (mb_type, `Function (p_type, return_type)), f_type)
	with
	    Unify_failure _ -> mistyped_application pos (f, f_type) (p, p_type) (Some (mb, mb_type))
      in
	Apply (Apply (f, mb, (inner_pos, `Function (p_type, return_type), None)), p, (pos, return_type, None))
  | Apply (f, p, pos) ->
      let f = type_check env f in
      let p = type_check env p in
      let f_type = type_of_expression f in
      let return_type = ITO.fresh_type_variable () in

      let _ =
	try unify (`Function(type_of_expression p, return_type), f_type)
	with Unify_failure _ -> mistyped_application pos (f, f_type) (p, type_of_expression p) None
      in
	Apply (f, p, (pos, return_type, None))
  | Condition (if_, then_, else_, pos) ->
      let if_ = type_check env if_ in
      let _ = (try unify (type_of_expression if_, `Primitive `Bool)
               with Unify_failure _ -> mistype (pos_of_expression if_) (if_, type_of_expression if_) (`Primitive `Bool)) in
      let then_expr = type_check env then_ in
      let else_expr = type_check env else_ in
      let _ = try 
        unify (type_of_expression then_expr, type_of_expression else_expr)
          (* FIXME: This can't be right!*)
      with _ ->         
        unify (type_of_expression else_expr, type_of_expression then_expr) in
      let node' = Condition (if_, 
                             then_expr,
                             else_expr,
                             (pos, 
                               type_of_expression then_expr,
                               None
                             )) in
        node'
  | Comparison (l, oper, r, pos) ->
      let l = type_check env l in
      let r = type_check env r in
	unify (type_of_expression l, type_of_expression r);
        Comparison (l, oper, r, (pos, `Primitive `Bool, None))
  | Abstr (mailbox_variable, Abstr(variable, body, inner_pos), pos) when Types.using_mailbox_typing () ->
      begin
	  (*
	    if the type inferred for a function term has
	    an uninstantiated mailbox parameter then
	    we can be sure that the body of the function
	    cannot receive messages (unless further function application
	    is performed)
	  *)
	let mailbox_type = ITO.fresh_type_variable () in
	let variable_type = ITO.fresh_type_variable () in
	let body_env = (mailbox_variable, ([], mailbox_type)) :: (variable, ([], variable_type)) :: env in
	let body = type_check body_env body in
	let inner_type = `Function (variable_type, type_of_expression body) in
	let outer_type = `Function (mailbox_type, inner_type) in
	  Abstr (mailbox_variable,
		 Abstr (variable, body, (inner_pos, inner_type, None)),
		 (pos, outer_type, None))
      end
  | Abstr (variable, body, pos) ->
      begin
	(if (Types.using_mailbox_typing ()) then
	   Debug.debug "mailbox typing assertion failure"
	 else
	   ());
	let variable_type = ITO.fresh_type_variable () in
	let body_env = (variable, ([], variable_type)) :: env in
	let body = type_check body_env body in
	let type' = `Function (variable_type, type_of_expression body) in
	  Abstr (variable, body, (pos, type', None))
      end
  | Let (variable, value, body, pos) ->
      let value = type_check env value in
      let vtype = (if is_value value then (generalize env (type_of_expression value))
                   else ([], type_of_expression value)) in
      let body = type_check ((variable, vtype) :: env) body in
	Let (variable, value, body, (pos, type_of_expression body, None))
  | Rec (variables, body, pos) ->
      let best_env, vars = type_check_mutually var_maps env variables in
      let body = type_check best_env body in
	Rec (vars, body, (pos, type_of_expression body, None))
  | Xml_node (tag, atts, cs, pos) as xml -> 
      let separate = partition (is_special -<- fst) in
      let (special_attrs, nonspecial_attrs) = separate atts in
      let bindings = 
(*         try *)
          lname_bound_vars xml 
(*         with InvalidLNameExpr ->  *)
(*           raise UndefinedVariable "Invalid l:name parameter " ^ string_of_expression  *)
      in
        (* "event" is always in scope for the event handlers *)
      let attr_env = ("event", ([], `Primitive(`Abstract "Event"))) :: env in
(* should now use alien javascript jslib : ... to import library functions *)
(*      let attr_env = ("jslib", ([], `Record(ITO.make_empty_open_row()))) :: attr_env in *)
        (* extend the env with each l:name bound variable *)
      let attr_env = fold_right (fun s env -> (s, ([], string_type)) :: env) bindings attr_env in
      let special_attrs = map (fun (name, expr) -> (name, type_check attr_env expr)) special_attrs in
        (* Check that the bound expressions have type 
           <strike>XML</strike> unit. *)
(*      let _ =
	List.iter (fun (_, expr) -> unify(type_of_expression expr, ITO.fresh_type_variable ()(*Types.xml*))) special_attrs in*)
      let contents = map (type_check env) cs in
      let nonspecial_attrs = map (fun (k,v) -> k, type_check env v) nonspecial_attrs in
(*      let attr_type = if islhref xml then Types.xml else Types.string_type in *)
      let attr_type = string_type in
        (* force contents to be XML, attrs to be strings *)
      let _ = List.iter (fun node -> unify (type_of_expression node, `List (`Primitive `XMLitem))) contents in
      let _ = List.iter (fun (_, node) -> unify (type_of_expression node, attr_type)) nonspecial_attrs in
      let trimmed_node =
        Xml_node (tag, 
                  nonspecial_attrs,         (* +--> up here I mean *)
                  contents,                 (* | *)
                  (pos, `List (`Primitive `XMLitem), None))
      in                                    (* | *)
        (* could just tack these on up there --^ *)
        add_attrs special_attrs trimmed_node

  | Record_empty (pos) ->
      Record_empty (pos, `Record (ITO.make_empty_closed_row ()), None)
  | Record_extension (label, value, record, pos) ->
      let value = type_check env value in
      let record = type_check env record in
      let unif_datatype = `Record (ITO.make_singleton_open_row (label, `Absent)) in
	unify (type_of_expression record, unif_datatype);

	let record_row = extract_row (type_of_expression record) in
	let value_type = type_of_expression value in
	  
	let type' = `Record (ITO.set_field (label, `Present value_type) record_row) in
	  Record_extension (label, value, record, (pos, type', None))
  | Record_selection (label, label_variable, variable, value, body, pos) ->
      let value = type_check env value in
      let label_variable_type = ITO.fresh_type_variable () in
	unify (type_of_expression value, `Record (ITO.make_singleton_open_row (label, `Present (label_variable_type))));

	let value_row = extract_row (type_of_expression value) in
	let label_var_equiv = label_variable, ([], label_variable_type) in
	let var_equiv = variable, ([], `Record (ITO.set_field (label, `Absent) value_row)) in
	  
	let body_env = label_var_equiv :: var_equiv :: env in
	let body = type_check body_env body in
	let body_type = type_of_expression body in
	  Record_selection (label, label_variable, variable, value, body, (pos, body_type, None))
  | Record_selection_empty (value, body, pos) ->
      let value = type_check env value in
	unify (`Record (ITO.make_empty_closed_row ()), type_of_expression value);
	let body = type_check env body in
          Record_selection_empty (value, body, (pos, type_of_expression body, None))
  | Variant_injection (label, value, pos) ->
      let value = type_check env value in
      let type' = `Variant (ITO.make_singleton_open_row (label, `Present (type_of_expression value))) in
        Variant_injection (label, value, (pos, type', None))
  | Variant_selection (value, case_label, case_variable, case_body, variable, body, pos) ->
      let value = type_check env value in
      let value_type = type_of_expression value in
      
      let case_var_type = ITO.fresh_type_variable() in
      let body_row = ITO.make_empty_open_row () in
      let variant_type = `Variant (ITO.set_field (case_label, `Present case_var_type) body_row) in
	unify (variant_type, value_type);

	let case_body = type_check ((case_variable, ([], case_var_type)) :: env) case_body in

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
	let body = type_check ((variable, ([], body_var_type)) :: env) body in

	let case_type = type_of_expression case_body in
	let body_type = type_of_expression body in
	  unify (case_type, body_type);
	  Variant_selection (value, case_label, case_variable, case_body, variable, body, (pos, body_type, None))
  | Variant_selection_empty (value, pos) ->
      let value = type_check env value in
      let new_row_type = `Variant (ITO.make_empty_closed_row()) in
        unify(new_row_type, type_of_expression value);
        Variant_selection_empty (value, (pos, ITO.fresh_type_variable (), None))
  | Nil (pos) ->
      Nil (pos, `List (ITO.fresh_type_variable ()), None)
  | List_of (elem, pos) ->
      let elem = type_check env elem in
	List_of (elem,
		 (pos, `List (type_of_expression elem), None))
  | Concat (l, r, pos) ->
      let tvar = ITO.fresh_type_variable () in
      let l = type_check env l in
	unify (type_of_expression l, `List (tvar));
	let r = type_check env r in
	  unify (type_of_expression r, type_of_expression l);
	  let type' = `List (tvar) in
	    Concat (l, r, (pos, type', None))
  | For (expr, var, value, pos) ->
      let value_tvar = ITO.fresh_type_variable () in
      let expr_tvar = ITO.fresh_type_variable () in
      let value = type_check env value in
	unify (type_of_expression value, `List (value_tvar));
	let expr_env = (var, ([], value_tvar)) :: env in
	let expr = type_check expr_env expr in
	  unify (type_of_expression expr, `List (expr_tvar));
	  let type' = type_of_expression expr in
	    For (expr, var, value, (pos, type', None))
  | Escape(var, body, pos) -> 
      let exprtype = ITO.fresh_type_variable () in
      let contrettype = ITO.fresh_type_variable () in
        (* It'd be better if this mailbox didn't intrude here.
           Perhaps there's some rewrite rule for `escape' that we
           could use instead. *)
      let conttype =
	if Types.using_mailbox_typing () then
	  let mailboxtype = instantiate env "_MAILBOX_" in
	    `Function (mailboxtype, `Function (exprtype, contrettype))
	else
	  `Function (exprtype, contrettype) in
      let body = type_check ((var, ([], conttype)):: env) body in
      let exprtype = exprtype in
	unify (exprtype, type_of_expression body);
        Escape(var, body, (pos, type_of_expression body, None))
  | Database (params, pos) ->
      let params = type_check env params in
        unify (type_of_expression params, db_descriptor_type);
        Database (params, (pos, `Primitive `DB, None))
  | TableQuery (ths, query, pos) ->
      let row =
	(List.fold_right
	   (fun col env ->
	      StringMap.add col.Query.name
		(`Present (inference_type_of_type var_maps col.Query.col_type)) env)
	   query.Query.result_cols StringMap.empty, `RowVar None) in
      let datatype =  `List (`Record row) in
      let row' = ITO.make_empty_open_row () in
      let ths = map (type_check env) ths
      in
        Utility.iter_over ths 
          (fun th -> 
             unify (type_of_expression th, `Table row'));
	unify_rows (row, row');
        TableQuery (ths, query, (pos, datatype, None))
  | TableHandle (db, tableName, row, pos) ->
      let datatype =  `Table (inference_row_of_row var_maps row) in
      let db = type_check env db in
      let tableName = type_check env tableName in
	unify (type_of_expression db, `Primitive `DB);
	unify (type_of_expression tableName, `List (`Primitive `Char)); 
        TableHandle (db, tableName, row, (pos, datatype, None))
  | SortBy(expr, byExpr, pos) ->
      (* FIXME: the byExpr is typed freely as yet. It could have any
         orderable type, of which there are at least several. How to
         resolve this? Would kill for type classes. *)
      let byExpr = type_check env byExpr in
      let expr = type_check env expr in
        SortBy(expr, byExpr, (pos, type_of_expression expr, None))
  | Wrong pos ->
      Wrong(pos, ITO.fresh_type_variable(), None)
  | HasType(expr, datatype, pos) ->
      let expr = type_check env expr in
	let expr_type = type_of_expression expr in
	let inference_datatype = inference_type_of_type var_maps datatype in
	  unify(expr_type, inference_datatype);
	  HasType(expr, datatype, (pos, type_of_expression expr, None))
  | Placeholder _ 
  | Alien _ ->
      assert(false)
  with 
      Unify_failure msg
    | UndefinedVariable msg ->
        raise (Type_error(untyped_pos expression, msg))
          (* end "type_check" *)

(** type_check_mutually
    Companion to "type_check"; does mutual type-inference

    [QUESTIONS]
      - what are the constraints on the definitions?
      - do the functions have to be recursive?
*)
and
    type_check_mutually var_maps env (defns : (string * untyped_expression * Types.datatype option) list) =
      let var_env = (map (fun (name, _, t) ->
                            match t with
                              | Some t ->
                                  (name, generalize env (inference_type_of_type var_maps t))
                              | None -> (name, ([], ITO.fresh_type_variable ())))
		       defns) in
      let inner_env = var_env @ env in
      let type_check var_maps result (name, expr, t) =
        let expr = type_check var_maps inner_env expr in
          match type_of_expression expr with
            | `Function _ as f  ->
		unify (snd (assoc name var_env), f);
		(name, expr, t) :: result
            | datatype -> Errors.letrec_nonfunction (pos_of_expression expr) (expr, datatype) in

      let defns = fold_left (type_check var_maps) [] defns in
      let defns = rev defns in

      let env = (List.map (fun (name, value,_) -> 
			     (name, generalize env (type_of_expression value))) defns
		 @ env) in
        env, defns     

(** {1 Callgraph ordering}

    Find the cliques in a group of functions.  Whenever there's mutual
    recursion we need to type all the functions in the cycle as
    `letrec-bound'; we want to avoid doing this in all other cases to
    make everything as polymorphic as possible (and to make typing
    faster).  In such cases the bindings must be reordered so that we
    type called functions before their callers.

    The plan is as follows:
    1. Find the call graph (by analysing the rhs for free variables.)
    2. Find all the cycles (strongly-connected components) in the call graph.
    3. Collapse cycles to single nodes and perform a topological sort
       to obtain the ordering.
*)

let is_mapped_by alist x = mem_assoc x alist

(** [make_callgraph bindings] returns an alist that gives a list of called
    functions for each function in [bindings] *)
let make_callgraph bindings = 
  alistmap
    (fun expr -> 
       filter (is_mapped_by bindings) (freevars expr)) 
    bindings

let group_and_order_bindings_by_callgraph 
    (bindings : (string * untyped_expression) list) 
    : string list list = 
  
  let call_graph = make_callgraph bindings in
    (* TBD: let's make a setting to print the callgraph any old time! *)
(*     debug("call_graph is " ^ mapstrcat ", " (Graph.edge_to_str) (Graph.unroll_edges call_graph)); *)
  let call_cliques = Graph.topo_sort_cliques call_graph in
(*     debug("call_cliques are: " ^ groupingsToString (identity) call_cliques); *)
    call_cliques

(* let defs_as_alist =  *)
(*   map (fun (Define (name, body, _, _) as e) -> name, e) *)

let defs_to_bindings = 
  map (fun (Define (name, body, _, _)) -> name, body)

let rec defn_of symbol = function
  | Define(n, _, _, _) as expr :: _ when n = symbol -> expr
  | _ :: defns -> defn_of symbol defns

let find_defn_in = flip defn_of

(** order_exprs_by_callgraph takes a list of groupings of functions
    and returns a new, possibly finer, list of groupings of functions.
    Each of the new groupings should truly be mutually recursive and
    the groupings should be ordered in callgraph-order (but note that
    bindings are only determined within the original groupings; how
    does this work with redefined function names that are part of
    mut-rec call groups? )*)
let refine_def_groups (expr_lists : untyped_expression list list) : untyped_expression list list = 
  let regroup_defs defs = 
    let bindings = defs_to_bindings defs in
    let cliques = group_and_order_bindings_by_callgraph bindings in
      map (map (find_defn_in defs)) cliques 
  in
    (* Each grouping in the input will be broken down into a new list
       of groupings. We only care about the new groupings, so we
       concat_map to bring them together *)
    concat_map (function
                  | Define _ :: _ as defs -> regroup_defs defs
                  | e                     -> [e]) expr_lists
      
let mutually_type_defs
    (var_maps : inference_type_map)
    (env : Types.environment)
    (defs : (string * untyped_expression * 'a option) list)
    : (Types.environment * (string * expression * 'c) list) =
  let env = inference_environment_of_environment var_maps env in
  let new_type_env, new_defs = type_check_mutually var_maps env defs in
    environment_of_inference_environment new_type_env,
  List.map (fun (name, exp, t) -> 
              name, expression_of_inference_expression exp, t) 
    new_defs

let type_expression : inference_type_map -> Types.environment -> untyped_expression -> (Types.environment * expression) =
  fun var_maps env untyped_expression ->
    let env = inference_environment_of_environment var_maps env in
    let env', exp' =
      match untyped_expression with
	| Define (variable, value, loc, pos) ->
	    let value = type_check var_maps env value in
	    let value_type = if is_value value then 
              (generalize env (type_of_expression value))
            else [], type_of_expression value in
              (((variable, value_type) :: env),
    	       Define (variable, value, loc, (pos, type_of_expression value, None)))
        | Alien (language, name, assumption, pos)  ->
            let (qs, k) = inference_assumption_of_assumption var_maps assumption in
              ((name, (qs, k)) :: env),
            Alien (language, name, assumption, (pos, k, None))
	| expr -> let value = type_check var_maps env expr in env, value
    in
      environment_of_inference_environment env', expression_of_inference_expression exp'

let type_program : inference_type_map -> Types.environment -> untyped_expression list -> (Types.environment * expression list) =
  fun var_maps env exprs ->

    let type_group (env, typed_exprs) : untyped_expression list -> (Types.environment * expression list) = function
      | [x] -> (* A single node *)
	  let env, expression = type_expression var_maps env x in 
            env, typed_exprs @ [expression]
      | xs  -> (* A group of potentially mutually-recursive definitions *)
          let defparts = map (fun (Define x) -> x) xs in
            (* Why can we assume we'll find a [Rec] with a single term here?*)
          let defbodies = map (fun (name, Rec ([(_, expr, t)], _, _), _, _) -> 
                                 name, expr, t) defparts in
          let env, defs = mutually_type_defs var_maps env defbodies in
          let defs = (map2 (fun (name, _, location, _) (_, expr, _) -> 
                              Define(name, expr, location, expression_data expr))
			defparts defs) in
            env, typed_exprs @ defs

    and bothdefs l r = match l, r with
      | Define (_, Rec _, _, _), Define (_, Rec _, _, _) -> true
      | _ ->  false
    in
    let def_seqs = groupBy bothdefs exprs in
    let mutrec_groups = (refine_def_groups def_seqs) in
      fold_left type_group (env, []) mutrec_groups

(** {1 Message typing trick.}
    This might be better off somewhere else (but where?).
*)

module RewriteTypes = 
  Rewrite.Rewrite
    (Rewrite.SimpleRewrite
       (struct
          type t = Types.datatype
          type rewriter = t -> t option
          let process_children = Types.perhaps_process_children
        end))

let remove_mailbox : RewriteTypes.rewriter = function
  | `Function (_, (`Function _ as f)) -> Some f
  | _                                 -> None

let remove_mailbox k = fromOption k (RewriteTypes.topdown remove_mailbox k)

(* Correct (hopefully), albeit imperative code for introducing mailboxes *)
let mailboxify_assumption (quantifiers, datatype) =
  let mailboxify mailboxes : RewriteTypes.rewriter = function
    | `Function _ as f ->
	let var = Type_basis.fresh_raw_variable () in
	  mailboxes := (`TypeVar var) :: !mailboxes;
	  Some (`Function (`TypeVar var, f))
    | _ -> None in
  let mailboxes = ref [] in
    match RewriteTypes.bottomup (mailboxify mailboxes) datatype with
      | None -> quantifiers, datatype
      | Some datatype' -> (!mailboxes @ quantifiers), datatype'

let unmailboxify_type : RewriteTypes.rewriter = function
  | `Function (_, (`Function _ as f)) -> Some f
  | _ -> None

let unmailboxify_assumption (quantifiers, datatype) = 
  match RewriteTypes.topdown unmailboxify_type datatype with
    | None -> quantifiers, datatype
    | Some datatype' -> quantifiers, datatype'


(* 
  add mailbox typing to a type environment.
  Ignore spawn, recv and
  self (which should perhaps be syntax tree nodes).
*)
let mailboxify_type_env = 
  let specials = ["spawn"; "recv"; "self"] in
    List.map (function
                | name, assumption when mem name specials -> (name, assumption)
                | name, assumption -> name, mailboxify_assumption assumption)


(* remove mailbox typing from a type environment *)
let unmailboxify_type_env =
    List.map (function
                | name, assumption -> name, unmailboxify_assumption assumption)

let retype_primitives = mailboxify_type_env
let unretype_primitives = unmailboxify_type_env  

module RewriteSyntaxU = Syntax.RewriteUntypedExpression
module RewriteSyntax = Syntax.RewriteSyntax

(* add / remove mailbox parameter to / from expressions *)
let add_parameter : RewriteSyntaxU.rewriter = function
  | Abstr (_,_,d) as e -> Some (Abstr ("_MAILBOX_", e, d))
  | Apply (f,a,d)      -> Some (Apply (Apply (f, Variable ("_MAILBOX_", Syntax.dummy_position), Syntax.dummy_position), a, d))
  | _                  -> None
and remove_parameter : RewriteSyntax.rewriter = function
  | Abstr ("_MAILBOX_", (Abstr (f,a,_)), d)              -> Some (Abstr (f,a,d))
  | Apply (Apply (f,Variable ("_MAILBOX_", _),_), a, d) -> Some (Apply (f,a,d))
  | _                                                   -> None

let add_parameter s = fromOption s (RewriteSyntaxU.bottomup add_parameter s)
and remove_parameter s = fromOption s (RewriteSyntax.bottomup remove_parameter s)

(* rewrite type annotations appearing in expressions *)
let rewrite_annotations : RewriteSyntaxU.rewriter = function
  | Rec (bs, body, d)    -> Some (Rec (List.map (fun (e,v,t) -> e,v, opt_map (snd -<- mailboxify_assumption -<- (fun t -> ([], t))) t) bs, body, d))

  | HasType (e, k, d)    -> Some (HasType (e, snd (mailboxify_assumption ([], k)), d))
  | Alien (s1, s2, k, d) -> Some (Alien (s1, s2, mailboxify_assumption k, d))
  | _                    -> None
let rewrite_annotations k = fromOption k (RewriteSyntaxU.bottomup rewrite_annotations k)


(* Check for duplicate top-level definitions.  This probably shouldn't
   appear in the type inference module.

   (Duplicate top-level definitions are simply not allowed.)

   In future we should probably allow duplicate top-level definitions, but
   only if we implement the correct semantics!
*)
let check_for_duplicate_defs 
    (type_env : Types.environment)
    (expressions :  untyped_expression list) =
  let check (env, defined) = function
    | Define (name, _, _, position) when StringMap.mem name defined ->
        (env, StringMap.add name (position :: StringMap.find name defined) defined)
    | Define (name, _, _, position) when StringSet.mem name env ->
        (env, StringMap.add name [position] defined)
    | Define (name, _, _, _) ->
        (StringSet.add name env, defined)
    | _ -> 
        (env, defined) in 
  let env = List.fold_right (fst ->- StringSet.add) type_env StringSet.empty in
  let _, duplicates = List.fold_left check (env,StringMap.empty) expressions in
    if not (StringMap.is_empty duplicates) then
      raise (Errors.MultiplyDefinedToplevelNames duplicates)


(*
  Create var maps for keeping track of mapping between typevars / rowvars and
  points. Initially these are primed with rigid vars occurring in type annotations.
  (Currently all type vars in type annotations are rigid.)
*)
let create_var_maps expressions =
  if Settings.get_value rigid_type_variables then
    let var_maps = Inferencetypes.empty_var_maps () in
    let tv = (get_quantifiers IntSet.empty)  -<- (inference_type_of_type var_maps) in
    let rec get_quantifiers e = 
      let annotations default = function
        | HasType (e, datatype, _) -> get_quantifiers e @ tv datatype
        | Rec (bs, e, _) -> Utility.concat_map (fun (_,e,t) -> fromOption [] (opt_map tv t) @ get_quantifiers e) bs @ get_quantifiers e
        | e -> default e in
        reduce_expression annotations (snd ->- List.concat) e in
      
    let quantifiers = Utility.concat_map get_quantifiers expressions in
    let tvars, rows = Inferencetypes.empty_var_maps () in
      List.iter (function
		   | `TypeVar var ->
                       if not (IntMap.mem var !tvars) 
                       then
			 tvars := IntMap.add var (Unionfind.fresh (`RigidTypeVar var)) !tvars
		   | `RowVar var ->
                       if not (IntMap.mem var !rows) 
                       then
			 rows := IntMap.add var (Unionfind.fresh (StringMap.empty, `RigidRowVar var)) !rows)
        quantifiers;
      tvars, rows
  else 
    Inferencetypes.empty_var_maps ()

(* [HACKS] *)
(* two pass typing: yuck! *)
let type_program env expressions = 
  check_for_duplicate_defs env expressions;
  let _ =
    (* without mailbox parameters *)
    debug_if_set (show_typechecking) (fun () -> "Typechecking program without mailbox parameters");
    Types.with_mailbox_typing false
      (fun () ->
	 type_program (create_var_maps expressions) (unmailboxify_type_env env) expressions) in
  let env', expressions' =
    (* with mailbox parameters *)
    debug_if_set (show_typechecking) (fun () -> "Typechecking program with mailbox parameters");
    let env, expressions =
      Types.with_mailbox_typing true
	(fun () ->
	   type_program (create_var_maps expressions) env (List.map (rewrite_annotations -<- add_parameter) expressions))
    in
      env, List.map remove_parameter expressions
  in
    env', expressions'

let type_expression env expression =
  check_for_duplicate_defs env [expression];
  let _ =
    (* without mailbox parameters *)	
    debug_if_set (show_typechecking) (fun () -> "Typechecking expression without mailbox parameters");
    Types.with_mailbox_typing false
      (fun () ->
	 type_expression (create_var_maps [expression]) (unmailboxify_type_env env) expression) in
  let env', expressions' =
    (* with mailbox parameters *)
    debug_if_set (show_typechecking) (fun () -> "Typechecking expression with mailbox parameters");
    let env, expression = 
      Types.with_mailbox_typing true
	(fun () ->
	   type_expression (create_var_maps [expression]) env ((rewrite_annotations -<- add_parameter)(expression)))
    in
      env, remove_parameter expression
  in
    env', expressions'
