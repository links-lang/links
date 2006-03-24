open List

open Utility
open Syntax
open Kind
open Inferencetypes
open Forms
open Errors

exception Unify_failure of string
exception UndefinedVariable of string

let vars : 'typ environment_basis -> string list = compose fst List.split
let vals : 'typ environment_basis -> 'typ assumption_basis list = compose snd List.split


type inference_expression = (position * inference_type * string option (* label *)) expression'

let node_kind : inference_expression -> inference_type =
  fun exp -> let _,t,_ = expression_data exp in t

let node_pos  : inference_expression -> position =
  fun exp -> let pos, _, _ = expression_data exp in pos


let expression_to_inference_expression : expression -> inference_expression =
  redecorate (fun (pos, t, label) -> (pos, type_to_inference_type t, label))

let inference_expression_to_expression : inference_expression -> expression =
  redecorate (fun (pos, t, label) -> (pos, inference_type_to_type t, label))

let rec extract_row : inference_type -> inference_row = function
  | `Record row -> row
  | `Variant row -> row
  | `MetaTypeVar point ->
      extract_row (Unionfind.find point)
  | t -> failwith
      ("Internal error: attempt to extract a row from a kind that is not a record or variant: " ^ (string_of_kind t))

let var_is_free_in_type var typ = mem var (type_vars typ)

let rec unify' : (int Unionfind.point) IntMap.t -> (inference_type * inference_type) -> unit = fun rec_vars ->
  let make_point id rec_vars =
    if IntMap.mem id rec_vars then
      IntMap.find id rec_vars, rec_vars
    else
      let point = Unionfind.fresh id in
	point, IntMap.add id point rec_vars in
    
    fun (t1, t2) ->
      (debug ("Unifying "^string_of_kind t1^" with "^string_of_kind t2);
       match t1, t2 with   
      | `Not_typed, _ | _, `Not_typed -> failwith "Internal error: `Not_typed' passed to `unify'"
      | `Primitive x, `Primitive y when x = y -> ()
(*      | `TypeVar lid, `TypeVar rid when lid = rid -> () *)
      | `MetaTypeVar lpoint, `MetaTypeVar rpoint ->
	  (match (Unionfind.find lpoint, Unionfind.find rpoint) with
	     | `TypeVar id, t -> Unionfind.union lpoint rpoint
	     | t, `TypeVar id -> Unionfind.union rpoint lpoint
	     | `Recursive (id, t), `Recursive (id', t') ->
		 debug ("rec (" ^ (string_of_int id) ^ "," ^ (string_of_int id') ^")");
		 let point, rec_vars = make_point id rec_vars in
		 let point', rec_vars = make_point id' rec_vars in
		   if Unionfind.equivalent point point' then
		     ()
		   else
		     Unionfind.union point point'; unify' rec_vars (t, t')
	     | `Recursive (id, t'), t | t, `Recursive (id, t')->
		 debug ("rec (" ^ (string_of_int id) ^ ")");
		 let point, rec_vars = make_point id rec_vars in
		   unify' rec_vars (t, t')
	     | t, t' -> unify' rec_vars (t, t'); Unionfind.union lpoint rpoint)
      | `MetaTypeVar point, t | t, `MetaTypeVar point ->
	  (match (Unionfind.find point) with
	     | `TypeVar id ->
		 if var_is_free_in_type id t then
   		   let _ = debug ("rec intro (" ^ (string_of_int id) ^ ")") in
		     Unionfind.change point (`Recursive (id, t))
		 else
		   Unionfind.change point t
	     | `Recursive (id, t') ->
   		 debug ("rec (" ^ (string_of_int id) ^ ")");
		 let point, rec_vars = make_point id rec_vars in
		   unify' rec_vars (t, t')
	     | t' -> unify' rec_vars (t, t'))
      | `Function (lvar, lbody), `Function (rvar, rbody) ->
          unify' rec_vars (lvar, rvar);
          unify' rec_vars (lbody, rbody)
      | `Record l, `Record r -> unify_row' rec_vars (l, r)
      | `Variant l, `Variant r -> unify_row' rec_vars (l, r)

      | `Collection (`List, `Primitive `XMLitem), `Collection (`List, `Primitive `XMLitem) -> ()

      | `Collection (`Set, t),  `Collection (`Set, t')  -> unify' rec_vars (t, t')
      | `Collection (`Bag, t),  `Collection (`Bag, t')  -> unify' rec_vars (t, t')
      | `Collection (`List, t), `Collection (`List, t') -> unify' rec_vars (t, t')

      | `Collection (`MetaCollectionVar lpoint, lelems), `Collection (`MetaCollectionVar rpoint, relems) ->
	  unify' rec_vars (lelems, relems);
	  let elem_type = lelems in
	    (match (Unionfind.find lpoint, Unionfind.find rpoint) with
	       | `CtypeVar id, ctype -> Unionfind.union lpoint rpoint
	       | ctype, `CtypeVar id -> Unionfind.union rpoint lpoint
	       | ctype, ctype' -> 
		   if ctype = ctype' then
		     ()
		   else
		     raise (Unify_failure
			      ("Couldn't match "^
				 string_of_kind (`Collection (ctype, elem_type)) ^" against "^
				 string_of_kind (`Collection (ctype', elem_type)))))
              
      | `Collection (`MetaCollectionVar point, elems), `Collection (ctype, elems') 
      | `Collection (ctype, elems), `Collection (`MetaCollectionVar point, elems') ->
	  unify' rec_vars (elems, elems');
	  let elem_type = elems in
	    (match (Unionfind.find point, ctype) with
	       | `CtypeVar id, ctype -> 
		   Unionfind.change point ctype
	       | ctype', ctype ->
		   if ctype = ctype' then
		     ()
		   else
		     raise (Unify_failure
			      ("Couldn't match "^
				 string_of_kind (`Collection (ctype, elem_type)) ^" against "^
				 string_of_kind (`Collection (ctype', elem_type)))))    
(*    
      | `Collection (`CtypeVar id, lelems), `Collection (reftype, relems) 
      | `Collection (reftype, lelems), `Collection (`CtypeVar id, relems) ->
          let var_subst = [Colltype_equiv (id, reftype)] in 
            compose_subst [var_subst; unify (lelems, relems)]
*)
      | `DB, `DB -> ()
      | l, r ->
          raise (Unify_failure ("Couldn't match "^ string_of_kind l ^" against "^ string_of_kind r)))

(* Unifies two rows which produces a substitution which, when
 * applied will transform both rows into a single row.  The algorithm
 * is split into four situations depending on whether the rows to
 * unify are closed (have no row variable) or not. *)
and unify_row' : (int Unionfind.point) IntMap.t -> ((inference_row * inference_row) -> unit) = 
  fun rec_vars (lrow, rrow) ->
      debug ("Unifying row: " ^ (string_of_row lrow) ^ " with row: " ^ (string_of_row rrow));

    (* 
       [NOTE]

       - All calls to fail_on_absent_fields are currently disabled,
       as under the current model absent fields have
       to be allowed in closed rows (although they're ignored).

       - There's no way of getting rid of absent variables as they're stored in the field
       environment rather than the row variable (good argument for moving them into the
       row variable).
    *)
      let fail_on_absent_fields field_env =
	StringMap.iter
	  (fun label -> function
	     | `Present _ -> ()
	     | `Absent ->
		 failwith "Internal error: closed row with absent variable"
	  ) field_env in

      let extend_field_env
	  (traversal_env : inference_field_spec_map)
	  (extending_env : inference_field_spec_map) =
	StringMap.fold
	  (fun label field_spec extension ->
	     if StringMap.mem label extending_env then
               (match field_spec, (StringMap.find label extending_env) with
	          | `Present t, `Present t' ->
		      unify' rec_vars (t, t');
		      extension
	          | `Absent, `Absent ->
		      (* Is this right? Yes. Throwing away the `Absent tag? No.*)
		      (* The `Absent tag is present in both field environments, so
			 doesn't need to be added to either environment. *)
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

      let unify_compatible_field_environments (field_env1, field_env2) =
	ignore (extend_field_env field_env1 field_env2) in

      let extend_row_var : inference_row_var * inference_row -> unit =
	fun (row_var, extension_row) ->
	  match row_var with
	    | `MetaRowVar point ->
		(* point should be a row variable *)
		(match Unionfind.find point with
		   | (env, `RowVar (Some _)) ->
		       assert(not (contains_present_fields env));
		       Unionfind.change point extension_row
		   | _ -> assert(false))
	    | `RowVar _ -> assert(false) in
	
      let unify_both_closed (lrow, rrow) =
	let get_present_labels field_env =
	  StringMap.fold (fun label field_spec labels ->
			    match field_spec with
			      | `Present _ -> label :: labels
			      | `Absent -> labels) field_env [] in

	let fields_are_compatible (field_env1, field_env2) =
	  (get_present_labels field_env1 = get_present_labels field_env2) in

(*
	let labels_are_equal (field_env, field_env') =
	  StringMap.equal (fun _ _ -> true) field_env field_env' in
*)

	let (lfield_env, lrow_var) = flatten_row lrow in
	let (rfield_env, rrow_var) = flatten_row rrow in
(*
 	  fail_on_absent_fields lfield_env;
	  fail_on_absent_fields rfield_env;
*)
	  if fields_are_compatible (lfield_env, rfield_env) then
	    unify_compatible_field_environments (lfield_env, rfield_env)
	  else
	    raise (Unify_failure ("Closed rows\n "^ string_of_row lrow
				 ^"\nand\n "^ string_of_row rrow
				 ^"\n could not be unified because they have different fields")) in


      let unify_one_closed (closed_row, open_row) =
	let (closed_field_env, _) as closed_row = flatten_row closed_row in
	let (open_field_env, open_row_var) as open_row = flatten_row open_row in

	  debug ("Flattened rows: " ^ (string_of_row closed_row) ^ " and: " ^ (string_of_row open_row));
	  
	  (* check that the open row contains no extra fields *)
          StringMap.iter
	    (fun label field_spec ->
	       if (StringMap.mem label closed_field_env) then
	         ()
	       else
	         match field_spec with
		   | `Present _ ->
		       raise (Unify_failure
			        ("Rows\n "^ string_of_row closed_row
			         ^"\nand\n "^ string_of_row open_row
			         ^"\n could not be unified because the former is closed"
			         ^" and the latter contains fields not present in the former"))
		   | `Absent -> ()
	    ) open_field_env;
          
	(* check that the closed row contains no absent fields *)
(*          fail_on_absent_fields closed_field_env; *)
		 
	let open_extension = extend_field_env closed_field_env open_field_env in
(*
	  debug ("Flattened rows: " ^ (string_of_row closed_row) ^ " and: " ^ (string_of_row open_row));

	  debug ("Extension: " ^ (string_of_row (open_extension, `RowVar None)));
	  debug ("Open row var: " ^ (string_of_row (ITO.empty_field_env, open_row_var)));
*)
	  extend_row_var (open_row_var, (open_extension, `RowVar None));
(*	  debug ("Unified flattened rows: " ^ (string_of_row closed_row) ^ " and: " ^ (string_of_row open_row))*) in

      let unify_both_open (lrow, rrow) =
	let lfield_env, lrow_var = flatten_row lrow in
	let rfield_env, rrow_var = flatten_row rrow in
	  if lrow_var = rrow_var then
	    unify_both_closed ((lfield_env, `RowVar None), (rfield_env, `RowVar None))
	  else
	    let row_var = ITO.new_row_variable() in	      
              (* each row can contain fields missing from the other; 
                 thus we call extend_field_env once in each direction *)
	    let rextension = extend_field_env lfield_env rfield_env in
	      extend_row_var (rrow_var, (rextension, row_var));
	      let lextension = extend_field_env rfield_env lfield_env in
		extend_row_var (lrow_var, (lextension, row_var)) in
      
      let _ =
	if ITO.is_closed_row lrow && ITO.is_closed_row rrow then
	  (* Both rows are closed. They can only be unified if they are equivalent *)
	  unify_both_closed (lrow, rrow)
	else if ITO.is_closed_row lrow then
          (* Only one row is closed, the other is open.  The open row's
           * row variable must be substituted so that its row is
           * equivalent to the closed one.  If fields are present in the
           * open row that are not in the closed row, they are not
           * unifiable *)
	  unify_one_closed (lrow, rrow)
        else if ITO.is_closed_row rrow then
          (* Only one row is closed, the other is open.  The open row's
           * row variable must be substituted so that its row is
           * equivalent to the closed one.  If fields are present in the
           * open row that are not in the closed row, they are not
           * unifiable *)
	  unify_one_closed (rrow, lrow)	    
        else
          (* Both rows are opened. Both row variables must be
           * substituted so that both rows are equivalent to a new row
           * that is the union of all fields of both rows plus a new row
           * variable.  If a field with the same label is either present
           * in one row and absent in the other or is present in both and
           * has non-unifiable types, the rows are not unifiable *)
	  unify_both_open (rrow, lrow)
      in
	debug ("Unified rows: " ^ (string_of_row lrow) ^ " and: " ^ (string_of_row rrow))

let unify = unify' IntMap.empty



(** instantiate env var
    Get the type of `var' from the environment, and rename bound typevars.
 *)
let instantiate : inference_environment -> string -> inference_type = fun env var ->
  try
    let generics, t = lookup var env in
      if generics = [] then
	t
      else
	let _ = debug ("Instantiating assumption: " ^ (string_of_assumption (generics, t))) in
(*    let kind = inference_type_to_type t in *)

	let tenv, renv, cenv = List.fold_left
	  (fun (tenv, renv, cenv) -> function
	     | `TypeVar id -> IntMap.add id (ITO.new_type_variable ()) tenv, renv, cenv
	     | `RowVar id -> tenv, IntMap.add id (ITO.new_row_variable ()) renv, cenv
	     | `CtypeVar id -> tenv, renv, IntMap.add id (ITO.new_collection_variable ()) cenv
	  ) (IntMap.empty, IntMap.empty, IntMap.empty) generics in
	  
	let rec inst : type_var_set -> inference_type -> inference_type = fun rec_vars typ ->
	  match typ with
	    | `Not_typed -> failwith "Internal error: `Not_typed' passed to `instantiate'"
	    | `Primitive _  -> typ
	    | `TypeVar id -> failwith "Internal error: (instantiate) TypeVar should be inside a MetaTypeVar"
	    | `MetaTypeVar point ->
		let t = Unionfind.find point in
		  (match t with
		     | `TypeVar id ->
			 if IntMap.mem id tenv then
			   IntMap.find id tenv
			 else
			   typ
			     (*			`MetaTypeVar (Unionfind.fresh (inst rec_vars t)) *)
		     | `Recursive (id, t) ->
			 debug ("rec (instantiate): " ^(string_of_int id));
			 if IntSet.mem id rec_vars then
			   typ
			 else
			   inst (IntSet.add id rec_vars) t
		     | _ -> inst rec_vars t)
(*	     
	     (match subst with
		| [] -> kind
		| Var_equiv (var_id, var_kind) :: substs when id = var_id -> var_kind
		| _ :: substs -> substitute substs kind)
*)
	    | `Function (var, body) -> `Function (inst rec_vars var, inst rec_vars body)
	    | `Record row -> `Record (inst_row rec_vars row)
	    | `Variant row ->  `Variant (inst_row rec_vars row)
	    | `Recursive (id, t) ->
		assert(false)
		  (*`Recursive (id, inst (IntSet.add id rec_vars) t) *)
	    | `Collection (collection_type, elem_type) ->
		`Collection (inst_collection_type collection_type, inst rec_vars elem_type)
		  (* `Collection (substitute_colltype subst coll_type, substitute subst elems) *)
	    | `DB -> `DB

(*
    and inst_row_var : type_var_set -> inference_row_var -> inference_row =
      fun rec_vars ->
	function
	  | `MetaRowVar point ->
	      let row_var = Unionfind.find point in
		(match row_var with
		   | `RowVar (Some var) -> 
		       if IntMap.mem var renv then
			 ITO.empty_field_env, IntMap.find var renv
		       else
			 ITO.empty_field_env, row_var
		  (*		ITO.empty_field_env, `MetaRowVar (Unionfind.fresh (ITO.empty_field_env, row_var)) *)
		   | `RowVar None -> ITO.make_empty_closed_row ()
		   | `MetaRowVar _ -> inst_row_var rec_vars row_var)
	  | `RowVar None ->
	      ITO.make_empty_closed_row ()
	  | `RowVar (Some _) ->
	      assert(false)
*)

	and inst_row : type_var_set -> inference_row -> inference_row = fun rec_vars row ->
	  let field_env, row_var = flatten_row row in
	    
	  let is_closed = (row_var = `RowVar None) in
	    
	  let field_env' = StringMap.fold
	    (fun label field_spec field_env' ->
	       match field_spec with
		 | `Present t -> StringMap.add label (`Present (inst rec_vars t)) field_env'
		 | `Absent ->
		     if is_closed then field_env'
		     else StringMap.add label `Absent field_env'
	    ) field_env StringMap.empty in

	  let row_var' =
	    match row_var with
	      | `MetaRowVar point ->
		  (match Unionfind.find point with
		     | (env, `RowVar (Some var)) ->
			 (* assert(StringMap.is_empty env); *)
			 if IntMap.mem var renv then
			   IntMap.find var renv
			 else
			   row_var
		     | (_, `RowVar None)
		     | (_, `MetaRowVar _) -> assert(false))
	      | `RowVar None ->
		  `RowVar None
	      | `RowVar (Some _) ->
		  assert(false)
	  in
	    field_env', row_var'
	
(*
      let row_var_field_env, row_var' = inst_row_var rec_vars row_var in
      let is_closed = ITO.is_closed_row (row_var_field_env, row_var') in

      let field_env' = StringMap.fold
	(fun label field_spec field_env' ->
	   match field_spec with
	     | `Present t -> StringMap.add label (`Present (inst rec_vars t)) field_env'
	     | `Absent ->
		 if is_closed then field_env'
		 else StringMap.add label `Absent field_env'
	 ) field_env StringMap.empty in
	
      let field_env'' = field_env_union (row_var_field_env, field_env') in
	(field_env'', row_var')
*)

	and inst_collection_type : inference_collection_type -> inference_collection_type = function
	  | `Set | `Bag | `List as k -> k
	  | `CtypeVar id as k ->
	      if IntMap.mem id cenv then
		IntMap.find id cenv
	      else
		`MetaCollectionVar (Unionfind.fresh k)
	  | `MetaCollectionVar point ->
	      inst_collection_type (Unionfind.find point)

	in
	  inst IntSet.empty t
  with Not_found ->
    raise (UndefinedVariable ("Variable '"^ var ^"' does not refer to a declaration"))

let rec get_quantifiers : type_var_set -> inference_type -> quantifier list = 
  fun used_vars -> 
    let is_used_var var = IntSet.mem var used_vars in 

  let free_field_spec_vars : inference_field_spec -> quantifier list =
    function
      | `Present t -> get_quantifiers used_vars t
      | `Absent -> [] in
    
  let rec row_generics : inference_row -> quantifier list = fun (field_env, row_var) ->
    let field_vars = StringMap.fold
      (fun label field_spec vars ->
	 free_field_spec_vars field_spec @ vars
      ) field_env [] in
    let row_vars = 
      match row_var with
	| `RowVar (Some var) when is_used_var var -> []
	| `RowVar (Some var) -> [`RowVar var]
	| `RowVar (None) -> []
	| `MetaRowVar point -> row_generics (Unionfind.find point) in
      field_vars @ row_vars
  in
    function
      | `Not_typed -> raise (Failure "Programming error (TY313)")
      | `Primitive _ as kind -> []
      | `TypeVar id as kind when is_used_var id -> []
      | `TypeVar id as kind -> [`TypeVar id]
      | `MetaTypeVar point ->
	  get_quantifiers used_vars (Unionfind.find point)
	  
      | `Function (var, body) ->
          let var_gens = get_quantifiers used_vars var
          and body_gens = get_quantifiers used_vars body in
            unduplicate (=) (var_gens @ body_gens)
      | `Record row -> row_generics row
      | `Variant row -> row_generics row
      | `Recursive (id, body) ->
	  debug ("rec (assumptionize): " ^(string_of_int id));
	  if is_used_var id then
	    []
	  else
	    get_quantifiers (IntSet.add id used_vars) body
(*	      assumptionize (substitute_environment [Var_equiv (id, kind)] env) body *)
      | `Collection (collection_type, elem_type) ->
(* [BUG] surely we need to generalise collection vars as well! *)
	  let collection_vars =
	    (match collection_type with
	       | `MetaCollectionVar point ->
		   (match Unionfind.find point with
		      | `CtypeVar id when is_used_var id -> []
		      | `CtypeVar id -> [`CtypeVar id]
		      | `MetaCollectionVar _ -> assert false
		      | _ -> [])
	       | `CtypeVar _ -> assert false
	       | _ -> []) in
	  let elem_vars = get_quantifiers used_vars elem_type
	  in
	    collection_vars @ elem_vars
      | `DB -> []

(** assumptionize: 
    Universally quantify any free type variables in the expression.
    TBD: Rename this to `generalize'? *)
let assumptionize : inference_environment -> inference_type -> inference_assumption = 
  fun env t ->
    let vars_in_env = concat_map (type_vars -<- snd) (vals env) in
    let used_vars = intset_of_list vars_in_env in
    let quantifiers = get_quantifiers used_vars t in
      debug ("Assumptionized: " ^ (string_of_assumption (quantifiers, t)));
      (quantifiers, t)

let rec w (env : inference_environment) : (untyped_expression -> inference_expression) = fun expression ->
  try
    debug ("Typechecking expression: " ^ (string_of_expression expression));
    match expression with
  | Define (variable, _, _, pos) -> nested_def pos variable
  | Directive (_, _) -> failwith "Internal error: directive reached type inference phase"
  | Boolean (value, pos) -> Boolean (value, (pos, `Primitive `Bool, None))
  | Integer (value, pos) -> Integer (value, (pos, `Primitive `Int, None))
  | Float (value, pos) -> Float (value, (pos, `Primitive `Float, None))
  | String (value, pos) -> String (value, (pos, `Collection (`List, `Primitive `Char), None))
  | Char (value, pos) -> Char (value, (pos, `Primitive `Char, None))
  | Variable (name, pos) -> Variable (name, (pos, instantiate env name, None))
  | Apply (f, p, pos) ->
      let f = w env f in
      let p = w env p in
      let f_type = node_kind f in
      let return_type = ITO.new_type_variable () in
      let _ =
	try unify (`Function(node_kind p, return_type), f_type)
	with Unify_failure _ -> mistyped_application pos (f, f_type) (p, node_kind p)
      in
	Apply (f, p, (pos, return_type, None))
  | Condition (if_, then_, else_, pos) as c ->
      let if_ = w env if_ in
      let _ = (try unify (node_kind if_, `Primitive `Bool)
               with Unify_failure _ -> mistype (node_pos if_) (if_, node_kind if_) (`Primitive `Bool)) in
      let then_expr = w env then_ in
      let else_expr = w env else_ in
      let _ = try 
        unify (node_kind then_expr, node_kind else_expr)
          (* FIXME: This can't be right!*)
      with _ ->         
        unify (node_kind else_expr, node_kind then_expr) in
      let node' = Condition (if_, 
                             then_expr,
                             else_expr,
                             (pos, 
                               node_kind then_expr,
                               None
                             )) in
        node'
  | Comparison (l, oper, r, pos) ->
      let l = w env l in
      let r = w env r in
      let _ = unify (node_kind l, node_kind r) in
        Comparison (l, oper, r, (pos, `Primitive `Bool, None))
  | Abstr (variable, body, pos) ->
      let variable_type = ITO.new_type_variable () in
      let body_env = (variable, ([], variable_type)) :: env in
(*      let (b, k) = hd body_env in *)
      let body = w body_env body in
      let type' = `Function (variable_type, node_kind body) in
(*
      let _ = Printf.printf "Type for Abstr: \"%s\" \n" (string_of_kind type') in
      let _ = Printf.printf "Substition: %s \n" (string_of_substitution body_subst) in
*)
      let node' = Abstr (variable, body, (pos, type', None)) in
        node'
  | Let (variable, _, _, pos) when qnamep variable -> invalid_name pos variable "qualified names (containing ':') cannot be bound"
  | Let (variable, value, body, pos) ->
      let value = w env value in
(*       let _ = debug ("Environment env: " ^ (string_of_environment env)) in *)
      let body_env = (variable, (assumptionize env (node_kind value))) :: env in
(*       let _ = debug ("Environment body_env: " ^ (string_of_environment body_env)) in *)
      let body = w body_env body in
(*       let _ = debug ("Environment body_env': " ^ (string_of_environment body_env)) in *)

(*
      let _ = Printf.printf "Type for Let: \"%s\" \n" (string_of_kind (node_kind body)) in
      let _ = Printf.printf "Substition: %s \n" (string_of_substitution body_subst) in
*)
      let node' = Let (variable, value, body, (pos, node_kind body, None)) in
	node'
  | Rec (variables, body, pos) ->
      let best_env, vars = m env variables in
      let body = w best_env body in
      let node' = Rec (vars, body, (pos, node_kind body, None)) in
        node'  

  | Xml_node (tag, atts, cs, pos) as xml -> 
      let separate = partition (is_special -<- fst) in
      let (special_attrs, nonspecial_attrs) = separate atts in
      let bindings = lname_bound_vars xml in
        (* "event" is always in scope for the event handlers *)
      let attr_env = ("event", ([], `Record(ITO.make_empty_open_row()))) :: env in
        (* extend the env with each l:name bound variable *)
      let attr_env = fold_right (fun s env -> (s, ([], Kind.string)) :: env) bindings attr_env in
      let special_attrs = map (fun (name, expr) -> (name, w attr_env expr)) special_attrs in
        (* Check that the bound expressions have type XML *)
        (* TBD: figure out what the right type for these is *)
(*      let _ =
	List.iter (fun (_, expr) -> unify(node_kind expr, ITO.new_type_variable ()(*Kind.xml*))) special_attrs in*)
      let contents = map (w env) cs in
      let nonspecial_attrs = map (fun (k,v) -> k, w env v) nonspecial_attrs in
(*      let attr_type = if islhref xml then Kind.xml else Kind.string in *)
      let attr_type = Kind.string in
        (* force contents to be XML, attrs to be strings
           unify is for side effect only! *)
      let unified_cs = map (fun node -> unify (node_kind node, `Collection (`List, `Primitive `XMLitem))) contents in
      let unified_atts = map (fun (s, node) -> unify (node_kind node, attr_type)) nonspecial_attrs in
      let trimmed_node =
        Xml_node (tag, 
                  nonspecial_attrs,         (* v-- up here I mean *)
                  contents,
                  (pos, `Collection (`List, `Primitive `XMLitem), None))
      in
        (* could just tack these on up there --^ *)
        add_attrs special_attrs trimmed_node

  | Record_empty (pos) ->
      Record_empty (pos, `Record (ITO.make_empty_closed_row ()), None)
  | Record_extension (label, value, record, pos) ->
      let value = w env value in
      let record = w env record in
      let unif_kind = `Record (ITO.make_singleton_open_row (label, `Absent)) in
      let _ = unify (node_kind record, unif_kind) in

      let record_row = extract_row (node_kind record) in
      let value_type = node_kind value in
        (* We think this is redundant! *)
(*
      let _ =
	if not (is_absent_from_row label (record_row)) then
	  raise (UndefinedVariable ("Label "^ label ^" should be absent"))
	else
          () in
*)

      let type' = `Record (ITO.set_field (label, `Present value_type) record_row) in
(* row_extend_absent label (`Present value_type) record_type in *)
(* `Record (`Field_present (label, value_type) :: (row_except_absent label (extract_fields record_type))) in *)
(*
      let _ = Printf.printf "Type for Record_extension: \"%s\" \n" (string_of_kind type') in
      let _ = Printf.printf "Substition: %s \n" (string_of_substitution subst) in
*)
      let node' = Record_extension (label, value, record, (pos, type', None)) in
        node'
  | Record_selection (label, label_variable, variable, value, body, pos) ->
      let value = w env value in
(*      let _ = Printf.printf "Substition A: %s \n" (string_of_substitution subst) in *)

(*      let _ = debug ("Environment A: " ^ (string_of_environment env)) in*)
(*      let _ = Printf.printf "Environment B: %s \n" (string_of_environment value_subst_env) in *)
      let label_variable_type = ITO.new_type_variable () in
(*
      let row_var = new_raw_variable () in
      let row = make_singleton_open_row_with_row_var label (`Present (label_variable_type)) `RowVar (Some (row_var)) in
      let unification = unify (node_kind value, `Record row in
*)
      let _ = unify (node_kind value, `Record (ITO.make_singleton_open_row (label, `Present (label_variable_type)))) in
(*      let _ = Printf.printf "Substition B: %s \n" (string_of_substitution subst) in*)

      let value_row = extract_row (node_kind value) in
      let label_var_equiv = label_variable, ([], label_variable_type) in
      let var_equiv = variable, ([], `Record (ITO.set_field (label, `Absent) value_row)) in

(*(`Record ((`Field_absent label) :: (row_except label value_fields)))) in*)

      let body_env = label_var_equiv :: var_equiv :: env in
(*      let _ = debug ("Environment C: " ^ (string_of_environment env)) in *)

(*      let _ = Printf.printf "Environment C: %s \n" (string_of_environment body_env) in*)
      let body = w body_env body in
(*      let _ = Printf.printf "Substition C: %s \n" (string_of_substitution subst) in*)

      let body_type = node_kind body in
(*
      let _ = Printf.printf "Type for Record_selection: \"%s\" \n" (string_of_kind body_type) in
      let _ = Printf.printf "Substition: %s \n" (string_of_substitution subst) in
*)
      let node' = Record_selection (label, label_variable, variable, value, body, (pos, body_type, None)) in
        node'
  | Record_selection_empty (value, body, pos) ->
      let value = w env value in
      let _ = debug ("Environment E: " ^ (string_of_environment env)) in
      let _ = unify (`Record (ITO.make_empty_closed_row ()), node_kind value) in
      let _ = debug ("Environment F: " ^ (string_of_environment env)) in
      let body = w env body in
        Record_selection_empty (value, body, (pos, node_kind body, None))
  | Variant_injection (label, value, pos) ->
      let value = w env value in
      let type' = `Variant (ITO.make_singleton_open_row (label, `Present (node_kind value))) in
(*      let type' = `Variant ([`Field_present (label, node_kind value) ; new_row_variable ()]) in *)
        Variant_injection (label, value, (pos, type', None))
  | Variant_selection (value, case_label, case_variable, case_body, variable, body, pos) ->
      let alpha = `Variant (ITO.make_empty_open_row ()) in
      let body = w ((variable, ([], alpha)) :: env) body in

      let beta = ITO.new_type_variable() in
      let case_body = w ((case_variable, ([], beta)) :: env) case_body in

      let body_type = node_kind body in
      let _ = unify(body_type, node_kind case_body) in

      let new_row_type = `Variant (ITO.set_field (case_label, `Present beta) (extract_row alpha)) in
(*      let new_row_type = row_with (case_label, beta') alpha' in *)
      let value = w env value in

      let _ = unify(new_row_type, node_kind value) in

      let final_node = Variant_selection (value, case_label, case_variable, case_body, variable, body, (pos, body_type, None)) in
        final_node
  | Variant_selection_empty (value, pos) ->

      let value = w env value in
      let new_row_type = `Variant (ITO.make_empty_closed_row()) in
        unify(new_row_type, node_kind value);
        
        let node' = 
	  Variant_selection_empty (value, (pos, ITO.new_type_variable (), None)) in
        node'
  | Collection_empty (ctype, pos) ->
      Collection_empty (ctype,
			(pos, `Collection (collection_type_to_inference_collection_type ctype, ITO.new_type_variable ()), None))
  | Collection_single (elem, ctype, pos) ->
      let elem = w env elem in
	Collection_single (elem, ctype,
			   (pos, `Collection (collection_type_to_inference_collection_type ctype, node_kind elem), None))
  | Collection_union (l, r, pos) ->
      let tvar = ITO.new_type_variable () in
      let collvar = ITO.new_collection_variable ()in
      let l = w env l in

      let _ = unify (node_kind l, `Collection (collvar, tvar)) in
      let r = w env r in
      let _ = unify (node_kind r, node_kind l) in
      let type' = `Collection (collvar, tvar) in
      let node' = Collection_union (l, r, (pos, type', None)) in
        node'
  | Collection_extension (expr, var, value, pos) ->
      let value_tvar = ITO.new_type_variable () in
      let expr_tvar = ITO.new_type_variable () in
      let collvar = ITO.new_collection_variable () in

      let value = w env value in
      let _ = unify (node_kind value, `Collection (collvar, value_tvar)) in
      let expr_env = (var, ([], value_tvar)) :: env in
      let expr = w expr_env expr in
      let _ = unify (node_kind expr, `Collection (collvar, expr_tvar)) in
        (match node_kind expr with
           | `Collection (ctype, _) ->
               let type' = `Collection (ctype, expr_tvar) in
               let value = value in
               let node = Collection_extension (expr, var, value, (pos, type', None)) in
                 node
           | _ -> failwith "Internal error: error substituting collection kind")
  | Escape(var, body, pos) -> 
      let exprtype = ITO.new_type_variable () in
      let contrettype = ITO.new_type_variable () in
      let conttype =  `Function (exprtype, contrettype) in
      let body = w ((var, ([], conttype)):: env) body in

      let exprtype = exprtype in
      let _ = unify (exprtype, node_kind body) in
        Escape(var, body, (pos, node_kind body, None))
  | Sort (up, list, pos) ->
      let list = w env list in
      let _ = unify (node_kind list, `Collection (ITO.new_collection_variable (), ITO.new_type_variable ())) in
      let new_kind = (match node_kind list
                      with `Collection(_, e) -> `Collection (`List, e)
                        | _ -> failwith "Internal error typing sort")
      in
        Sort (up, list, (pos, new_kind, None))
  | Database (params, pos) ->
      let params = w env params in
        let _ = unify (node_kind params, `Collection(`List, `Primitive `Char)) in
        Database (params, (pos, `DB, None))
  | Table (db, s, query, pos) ->
      let row =
	(List.fold_right
	   (fun col env ->
	      StringMap.add col.Query.name
		(`Present (type_to_inference_type col.Query.col_type)) env)
	   query.Query.result_cols StringMap.empty, `RowVar None) in
      let kind =  `Collection ((if query.Query.sortings <> [] then `List 
                                else if query.Query.distinct_only then `Set 
                                else `Bag),
			       `Record row) in
(*
                               `Record (map (fun col -> 
                                               `Field_present(col.Query.name, col.Query.col_type)) query.Query.result_cols))
*)
      let db = w env db in
      let _ = unify (node_kind db, `DB) in
      let _ = unify (kind, `Collection (ITO.new_collection_variable (), `Record (ITO.make_empty_open_row ()))) in
        Table (db, s, query, (pos, kind, None))
          
  with 
      Unify_failure msg
    | UndefinedVariable msg ->
        raise (Type_error(untyped_pos expression, msg))
(* end "w" *)

(** m
    Companion to "w"; does mutual type-inference 
*)
and
    m env (defns : (string * untyped_expression) list) =
      let type_check (env, result) (label, expr) = 
        let expr' = w env expr in
          match node_kind expr' with
            | `Function _ -> ((label, ([], (node_kind expr'))) :: env,
                              (label, expr') :: result)
            | kind -> Errors.letrec_nonfunction (node_pos expr') (expr', kind)
      in
      let var_env = (map (fun (name, expr) ->
			     (name, ([], ITO.new_type_variable ())))
		       defns) in
      let initial_env = (var_env @ env) in
      let env', defns = fold_left type_check (initial_env, []) defns in
      let defns = rev defns in

(*      let rec_substs = compose_subst (map2 (fun var_kind (label, expr) -> 
                                              try
                                                unify (node_kind expr, substitute var_subst var_kind)
                                              with
                                                  Unify_failure msg -> 
                                                    raise(Type_error(node_pos expr, msg)))
                                        var_kinds defns) in
*)
      let env = (alistmap (fun value -> 
			     (assumptionize initial_env (node_kind value))) defns
		 @ env') in 
        env, defns
            

(** Find the cliques in a group of functions.  Whenever there's mutual
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
let order_via_caller_lists (functions : (string * string list) list) : string list list =
(* [(fn1, [calls_1; calls_2; ...]);
    (fn2, [calls_1; calls_2; ...]);
    ...] 
   -> [[fn_i]; [fn_j; fn_k]; ...]
*)
  let find_clique cliques f = find (mem f) cliques in
  let edges = concat (map (fun (f, callers) -> map (fun caller -> f, caller) callers) functions) 
  and nodes = map fst functions in
  let cliques = Graph.strongly_connected_components nodes edges in
  let group_callers = map (fun nodes -> 
                             (nodes, unduplicate (=) (concat (map (flip assoc functions) nodes)))) cliques in
  let group_callers = map (fun (f, calls) -> f, map (find_clique cliques) calls) group_callers in
  let group_edges = concat (map (fun (f, callers) -> map (fun caller -> f, caller) callers) group_callers) in
    rev (map fst (Graph.topological_sort cliques group_edges))

let find_cliques (bindings : (string * untyped_expression) list) 
    : (string * untyped_expression) list list = 
    let callers = map (fun (name, expr) -> (name, filter (flip mem_assoc bindings) (freevars expr))) bindings in
    let orders = order_via_caller_lists callers in
      map (map (fun name -> name, assoc name bindings)) orders 

let mutually_type_defs
    : environment -> (string * untyped_expression) list -> (environment * (string * expression) list) =
  fun env defs ->
    let env = environment_to_inference_environment env in
(*    let defs = List.map (fun (name, exp) -> name, expression_to_inference_expression exp) defs in *)
    let new_type_env, new_defs = m env defs in
      inference_environment_to_environment new_type_env,
    List.map (fun (name, exp) -> name, inference_expression_to_expression exp) new_defs

let regroup exprs = 
  let regroup_defs defs = 
    let alist = map (fun (Define (name, f, _, _) as e) -> name, (e, f)) defs in
    let cliques = find_cliques (map (fun (name, (e, f)) -> (name, f)) alist) in
      map (map (fun (k,v) -> fst (assoc k alist))) cliques 
  in
    concat (map (function
                   | Define _ :: _ as defs -> regroup_defs defs
                   | e                     -> [e]) exprs)

let type_expression : environment -> untyped_expression -> (environment * expression) =
  fun env untyped_expression ->
    let env = environment_to_inference_environment env in
    let env', exp' =
      match untyped_expression with
	| Define (variable, value, loc, pos) ->
	    let value = w env value in 
              ((variable, (assumptionize env (node_kind value))) :: env),
    	       Define (variable, value, loc, (pos, node_kind value, None))
	| expr -> let value = w env expr in env, value
    in
      inference_environment_to_environment env', inference_expression_to_expression exp'

(* Is there some missing assumptionization here? *)
let type_program : environment -> untyped_expression list -> (environment * expression list) =
  fun env exprs ->
    let type_group (env, typed_exprs) : untyped_expression list -> (environment * expression list) = function
      | [x] -> (* A single node *)
	  let env, expression = type_expression env x in 
            env, typed_exprs @ [expression]
      | xs  -> (* A group of potentially mutually-recursive definitions *)
          let defparts = map (fun (Define x) -> x) xs in
          let env, defs = mutually_type_defs env (map (fun (name, expr, _, _) -> name, expr) defparts) in
          let defs = (map2 (fun (name, _, location, data) (_, expr) -> 
                              Define (name, expr, location, expression_data expr)) 
			defparts defs) in
            env, typed_exprs @ defs

    and bothdefs l r = match l, r with
      | Define (_, Rec _, _, _), Define (_, Rec _, _, _) -> true
      | _ ->  false
    in
      fold_left type_group (env, []) (regroup (groupBy bothdefs exprs))
  

      
