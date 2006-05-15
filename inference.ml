open List

open Utility
open Syntax
open Inferencetypes
open Forms
open Errors

exception Unify_failure of string
exception UndefinedVariable of string

module ITO = InferenceTypeOps

(* extract data from inference_expressions *)
let type_of_expression : inference_expression -> inference_type =
  fun exp -> let _, t, _ = expression_data exp in t
let pos_of_expression : inference_expression -> position =
  fun exp -> let pos, _, _ = expression_data exp in pos

(* conversions between expressions and inference expressions *)
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
      ("Internal error: attempt to extract a row from a kind that is not a record or variant: " ^ (string_of_type t))

let var_is_free_in_type var typ = mem var (free_type_vars typ)

let rec unify' : (int Unionfind.point) IntMap.t -> (inference_type * inference_type) -> unit = fun rec_vars ->
  let make_point var rec_vars =
    if IntMap.mem var rec_vars then
      IntMap.find var rec_vars, rec_vars
    else
      let point = Unionfind.fresh var in
	point, IntMap.add var point rec_vars in
    
    fun (t1, t2) ->
      (debug ("Unifying "^string_of_type t1^" with "^string_of_type t2);
       (match (t1, t2) with
      | `Not_typed, _ | _, `Not_typed -> failwith "Internal error: `Not_typed' passed to `unify'"
      | `Primitive x, `Primitive y when x = y -> ()
      | `MetaTypeVar lpoint, `MetaTypeVar rpoint ->
	  (match (Unionfind.find lpoint, Unionfind.find rpoint) with
	     | `TypeVar lvar, `TypeVar rvar ->
		   Unionfind.union lpoint rpoint
	     | `TypeVar var, t ->
		 (if var_is_free_in_type var t then
		    (debug ("rec intro1 (" ^ (string_of_int var) ^ ")");
		     Unionfind.change rpoint (`Recursive (var, t)))
		 else
		   ());
		 Unionfind.union lpoint rpoint
	     | t, `TypeVar var ->
		 (if var_is_free_in_type var t then
		    (debug ("rec intro2 (" ^ (string_of_int var) ^ ")");
		     Unionfind.change lpoint (`Recursive (var, t)))
		  else
		    ());
		 Unionfind.union rpoint lpoint
	     | `Recursive (var, t), `Recursive (var', t') ->
		 debug ("rec (" ^ (string_of_int var) ^ "," ^ (string_of_int var') ^")");
		 let point, rec_vars = make_point var rec_vars in
		 let point', rec_vars = make_point var' rec_vars in
		   if Unionfind.equivalent point point' then
		     ()
		   else
		     (assert(var <> var');
		      Unionfind.union point point';
		      unify' rec_vars (t, t'))
	     | `Recursive (var, t'), t | t, `Recursive (var, t')->
		 debug ("rec (" ^ (string_of_int var) ^ ")");
		 let point, rec_vars = make_point var rec_vars in
		   unify' rec_vars (t, t')
	     | t, t' -> unify' rec_vars (t, t'); Unionfind.union lpoint rpoint)
      | `MetaTypeVar point, t | t, `MetaTypeVar point ->
	  (match (Unionfind.find point) with
	     | `TypeVar var ->
		 if var_is_free_in_type var t then
   		   (let _ = debug ("rec intro3 ("^string_of_int var^","^string_of_type t^")") in
		     Unionfind.change point (`Recursive (var, t)))
		 else
		   (debug ("non-rec (" ^ string_of_int var ^ ")");
		   Unionfind.change point t)
	     | `Recursive (var, t') ->
   		 debug ("rec (" ^ (string_of_int var) ^ ")");
		 let point, rec_vars = make_point var rec_vars in
		   unify' rec_vars (t, t')
	     | t' -> unify' rec_vars (t, t'))
      | `Function (lvar, lbody), `Function (rvar, rbody) ->
          unify' rec_vars (lvar, rvar);
          unify' rec_vars (lbody, rbody)
      | `Record l, `Record r -> unify_row' rec_vars (l, r)
      | `Variant l, `Variant r -> unify_row' rec_vars (l, r)
      | `List t, `List t' -> unify' rec_vars (t, t')
      | `Mailbox t, `Mailbox t' -> unify' rec_vars (t, t')
      | `DB, `DB -> ()
      | _, _ ->
          raise (Unify_failure ("Couldn't match "^ string_of_type t1 ^" against "^ string_of_type t2)));
      debug ("Unified types: " ^ string_of_type t1)
      )

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
		   | (env, `RowVar (Some var)) ->
		       assert(not (contains_present_fields env));
		       if mem var (free_row_type_vars extension_row) then
			 Unionfind.change point (env, `RecRowVar (var, extension_row))
		       else
			 Unionfind.change point extension_row
		   | _ -> assert(false))
	    | `RowVar _ | `RecRowVar _ -> assert(false) in
	
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

	let (lfield_env, lrow_var) = unwrap_row lrow in
	let (rfield_env, rrow_var) = unwrap_row rrow in
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
	let (closed_field_env, _) as closed_row = unwrap_row closed_row in
	let (open_field_env, open_row_var) as open_row = unwrap_row open_row in 
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
	    extend_row_var (open_row_var, (open_extension, `RowVar None)) in

      let unify_both_open (lrow, rrow) =
	let rec row_var_eq = function
	  | `RowVar None, `RowVar None -> true
	  | `RowVar (Some var1), `RowVar (Some var2)
	  | `RecRowVar (var1, _), `RecRowVar (var2, _) -> var1=var2
	  | `MetaRowVar point, row_var | row_var, `MetaRowVar point ->
	      row_var_eq (snd (Unionfind.find point), row_var)
	  | _, _ -> false in

	let (lfield_env, lrow_var) as lrow = flatten_row lrow in
	let (rfield_env, rrow_var) as rrow = flatten_row rrow in
	  if row_var_eq (lrow_var, rrow_var) then
	    unify_both_closed ((lfield_env, `RowVar None), (rfield_env, `RowVar None))
	  else
	    let lfield_env, lrow_var = unwrap_row lrow in
	    let rfield_env, rrow_var = unwrap_row rrow in
	    let row_var = ITO.fresh_row_variable() in	      
              (* each row can contain fields missing from the other; 
                 thus we call extend_field_env once in each direction *)
	    let rextension =
	      extend_field_env lfield_env rfield_env in
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

let unify (t1, t2) =
  (unify' IntMap.empty (t1, t2);
   debug ("Unified types: " ^ string_of_type t1))

type rec_type_map = (inference_type Unionfind.point) IntMap.t
type rec_row_map = (inference_row Unionfind.point) IntMap.t

type rec_maps = rec_type_map * rec_row_map

(** instantiate env var
    Get the type of `var' from the environment, and rename bound typevars.
 *)
let instantiate : inference_environment -> string -> inference_type = fun env var ->
  try
    let generics, t = Type_basis.lookup var env in
      if generics = [] then
	t
      else
	(
	let _ = debug ("Instantiating assumption: " ^ (string_of_assumption (generics, t))) in

	let tenv, renv, cenv = List.fold_left
	  (fun (tenv, renv, cenv) -> function
	     | `TypeVar var -> IntMap.add var (ITO.fresh_type_variable ()) tenv, renv, cenv
	     | `RowVar var -> tenv, IntMap.add var (ITO.fresh_row_variable ()) renv, cenv
	  ) (IntMap.empty, IntMap.empty, IntMap.empty) generics in
	  
	let rec inst : rec_maps -> inference_type -> inference_type = fun rec_env typ ->
	  let rec_type_env, rec_row_env = rec_env in
	    match typ with
	      | `Not_typed -> failwith "Internal error: `Not_typed' passed to `instantiate'"
	      | `Primitive _  -> typ
	      | `TypeVar var -> failwith "Internal error: (instantiate) TypeVar should be inside a MetaTypeVar"
	      | `MetaTypeVar point ->
		  let t = Unionfind.find point in
		    (match t with
		       | `TypeVar var ->
			   if IntMap.mem var tenv then
			     IntMap.find var tenv
			   else
			     typ
			       (*			`MetaTypeVar (Unionfind.fresh (inst rec_vars t)) *)
		       | `Recursive (var, t) ->
			   debug ("rec (instantiate)1: " ^(string_of_int var));

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
	      | `Recursive (var, t) ->
		  (*assert(false)*)
		  debug ("rec (instantiate)2: " ^(string_of_int var));

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

		  (*`Recursive (var, inst (IntSet.add var rec_vars) t) *)
	      | `List (elem_type) ->
		  `List (inst rec_env elem_type)
	      | `Mailbox (elem_type) ->
		  `Mailbox (inst rec_env elem_type)
	      | `DB -> `DB
	and inst_row : rec_maps -> inference_row -> inference_row = fun rec_env row ->
	  let rec_type_env, rec_row_env = rec_env in
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

	  let row_var' =
	    match row_var with
	      | `MetaRowVar point ->
		  (match Unionfind.find point with
		     | (field_env, `RowVar (Some var)) ->
			 (* assert(StringMap.is_empty env); *)
			 if IntMap.mem var renv then
			   IntMap.find var renv
			 else
			   row_var
		     | (_, `RowVar None) | (_, `MetaRowVar _) -> assert(false)
		     | (field_env, `RecRowVar (var, rec_row)) ->
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
	      | `RowVar (Some _) ->
		  assert(false)
	      | `RecRowVar (var, rec_row) ->
		  assert(false)
	  in
	    field_env', row_var'
	in
	  inst (IntMap.empty, IntMap.empty) t)
  with Not_found ->
    raise (UndefinedVariable ("Variable '"^ var ^"' does not refer to a declaration"))

let rec get_quantifiers : type_var_set -> inference_type -> quantifier list = 
  fun bound_vars -> 
 
    let rec row_generics : type_var_set -> inference_row -> quantifier list = fun bound_vars (field_env, row_var) ->
      let free_field_spec_vars : inference_field_spec -> quantifier list =
	function
	  | `Present t -> get_quantifiers bound_vars t
	  | `Absent -> [] in

      let field_vars = StringMap.fold
	(fun label field_spec vars ->
	   free_field_spec_vars field_spec @ vars
	) field_env [] in


      let row_vars = 
	match row_var with
	  | `RowVar (None) -> []
	  | `RowVar (Some var) when IntSet.mem var bound_vars -> []
	  | `RowVar (Some var) -> [`RowVar var]
	  | `RecRowVar (var, rec_row) ->
	      debug ("rec (row_generics): " ^(string_of_int var));
	      if IntSet.mem var bound_vars then
		[]
	      else
		row_generics (IntSet.add var bound_vars) rec_row
	  | `MetaRowVar point -> row_generics bound_vars (Unionfind.find point)

      in
	field_vars @ row_vars
    in
      function
	| `Not_typed -> raise (Failure "Programming error (TY313)")
	| `Primitive _ as kind -> []
	| `TypeVar var as kind when IntSet.mem var bound_vars -> []
	| `TypeVar var as kind -> [`TypeVar var]
	| `MetaTypeVar point ->
	    get_quantifiers bound_vars (Unionfind.find point)
	      
	| `Function (var, body) ->
            let var_gens = get_quantifiers bound_vars var
            and body_gens = get_quantifiers bound_vars body in
              unduplicate (=) (var_gens @ body_gens)
	| `Record row -> row_generics bound_vars row
	| `Variant row -> row_generics bound_vars row
	| `Recursive (var, body) ->
	    debug ("rec (get_quantifiers): " ^(string_of_int var));
	    if IntSet.mem var bound_vars then
	      []
	    else
	      get_quantifiers (IntSet.add var bound_vars) body
	| `List (elem_type) ->
	    get_quantifiers bound_vars elem_type
	| `Mailbox (elem_type) ->
	    get_quantifiers bound_vars elem_type
	| `DB -> []

(** generalize: 
    Universally quantify any free type variables in the expression.
*)
let generalize : inference_environment -> inference_type -> inference_assumption = 
  fun env t ->
    let vars_in_env = concat_map (free_type_vars -<- snd) (Type_basis.environment_values env) in
    let bound_vars = intset_of_list vars_in_env in
    let quantifiers = get_quantifiers bound_vars t in
      debug ("Generalized: " ^ (string_of_assumption (quantifiers, t)));
      (quantifiers, t)

let rec is_value : 'a expression' -> bool = function
  | Boolean _
  | Integer _
  | Char _
  | String _
  | Float _
  | Variable _
  | Xml_node _ (* ? *)
  | Record_empty _
  | Nil _
  | Abstr _ -> true
  | Variant_injection (_, e, _)
  | Variant_selection_empty (e, _)
  | Database (e, _)
  | Table (e, _, _, _)
  | List_of (e, _) -> is_value e
  | Comparison (a,_,b,_)
  | Concat (a, b, _)
  | For (a, _, b, _)
  | Record_extension (_, a, b, _)
  | Record_selection_empty (a, b, _)
  | Record_selection (_, _, _, a, b, _)
  | Let (_, a, b,_)  -> is_value a && is_value b
  | Variant_selection (a, _, _, b, _, c, _)
  | Condition (a,b,c,_) -> is_value a && is_value b && is_value c
  | Rec (bs, e, _) -> List.for_all (is_value -<- snd) bs && is_value e
  | _ -> false

let rec type_check (env : inference_environment) : (untyped_expression -> inference_expression) = fun expression ->
  try
    debug ("Typechecking expression: " ^ (string_of_expression expression));
    match expression with
  | Define (variable, _, _, pos) -> nested_def pos variable
  | Boolean (value, pos) -> Boolean (value, (pos, `Primitive `Bool, None))
  | Integer (value, pos) -> Integer (value, (pos, `Primitive `Int, None))
  | Float (value, pos) -> Float (value, (pos, `Primitive `Float, None))
  | String (value, pos) -> String (value, (pos, `List (`Primitive `Char), None))
  | Char (value, pos) -> Char (value, (pos, `Primitive `Char, None))
  | Variable (name, pos) -> Variable (name, (pos, instantiate env name, None))
  | Apply (f, p, pos) ->
      let f = type_check env f in
      let p = type_check env p in
      let f_type = type_of_expression f in
      let return_type = ITO.fresh_type_variable () in
      let _ =
	try unify (`Function(type_of_expression p, return_type), f_type)
	with Unify_failure _ -> mistyped_application pos (f, f_type) (p, type_of_expression p)
      in
	Apply (f, p, (pos, return_type, None))
  | Condition (if_, then_, else_, pos) as c ->
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
  | Abstr (variable, body, pos) ->
      let variable_type = ITO.fresh_type_variable () in
      let body_env = (variable, ([], variable_type)) :: env in
      let body = type_check body_env body in
      let type' = `Function (variable_type, type_of_expression body) in
	Abstr (variable, body, (pos, type', None))
  | Let (variable, _, _, pos) when qnamep variable -> invalid_name pos variable "qualified names (containing ':') cannot be bound"
  | Let (variable, value, body, pos) ->
      let value = type_check env value in
      let vtype = (if is_value value then (generalize env (type_of_expression value))
                   else ([], type_of_expression value)) in
      let body = type_check ((variable, vtype) :: env) body in
	Let (variable, value, body, (pos, type_of_expression body, None))
  | Rec (variables, body, pos) ->
      let best_env, vars = type_check_mutually env variables in
      let body = type_check best_env body in
	Rec (vars, body, (pos, type_of_expression body, None))
  | Xml_node (tag, atts, cs, pos) as xml -> 
      let separate = partition (is_special -<- fst) in
      let (special_attrs, nonspecial_attrs) = separate atts in
      let bindings = lname_bound_vars xml in
        (* "event" is always in scope for the event handlers *)
      let attr_env = ("event", ([], `Record(ITO.make_empty_open_row()))) :: env in
      let attr_env = ("jslib", ([], `Record(ITO.make_empty_open_row()))) :: attr_env in
        (* extend the env with each l:name bound variable *)
      let attr_env = fold_right (fun s env -> (s, ([], inference_string_type)) :: env) bindings attr_env in
      let special_attrs = map (fun (name, expr) -> (name, type_check attr_env expr)) special_attrs in
        (* Check that the bound expressions have type 
           <strike>XML</strike> unit. *)
(*      let _ =
	List.iter (fun (_, expr) -> unify(type_of_expression expr, ITO.fresh_type_variable ()(*Kind.xml*))) special_attrs in*)
      let contents = map (type_check env) cs in
      let nonspecial_attrs = map (fun (k,v) -> k, type_check env v) nonspecial_attrs in
(*      let attr_type = if islhref xml then Kind.xml else Kind.string_type in *)
      let attr_type = inference_string_type in
        (* force contents to be XML, attrs to be strings
           unify is for side effect only! *)
      let unified_cs = map (fun node -> unify (type_of_expression node, `List (`Primitive `XMLitem))) contents in
      let unified_atts = map (fun (s, node) -> unify (type_of_expression node, attr_type)) nonspecial_attrs in
      let trimmed_node =
        Xml_node (tag, 
                  nonspecial_attrs,         (* v-- up here I mean *)
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
      let unif_kind = `Record (ITO.make_singleton_open_row (label, `Absent)) in
	unify (type_of_expression record, unif_kind);

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
      let body_var_type = `Variant body_row in
      let variant_type = `Variant (ITO.set_field (case_label, `Present case_var_type) body_row) in
	unify (variant_type, value_type);

	let case_body = type_check ((case_variable, ([], case_var_type)) :: env) case_body in
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
      let mailboxtype = ITO.fresh_type_variable () in 
      let conttype =  `Function (mailboxtype, `Function (exprtype, contrettype)) in
      let body = type_check ((var, ([], conttype)):: env) body in
      let exprtype = exprtype in
	unify (exprtype, type_of_expression body);
        Escape(var, body, (pos, type_of_expression body, None))
  | Database (params, pos) ->
      let params = type_check env params in
        unify (type_of_expression params, `List(`Primitive `Char));
        Database (params, (pos, `DB, None))
  | Table (db, s, query, pos) ->
      let row =
	(List.fold_right
	   (fun col env ->
	      StringMap.add col.Query.name
		(`Present (type_to_inference_type col.Query.col_type)) env)
	   query.Query.result_cols StringMap.empty, `RowVar None) in
      let kind =  `List (`Record row) in
      let db = type_check env db in
	unify (type_of_expression db, `DB);
	unify (kind, `List (`Record (ITO.make_empty_open_row ())));
        Table (db, s, query, (pos, kind, None))
  | Wrong pos ->
      Wrong(pos, ITO.fresh_type_variable(), None)
  | HasType(expr, typ, pos) ->
      let expr = type_check env expr in
	unify(type_of_expression expr, type_to_inference_type typ);
	HasType(expr, typ, (pos, type_of_expression expr, None))
          
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
    type_check_mutually env (defns : (string * untyped_expression) list) =
      let var_env = (map (fun (name, expr) ->
	                      (name, ([], ITO.fresh_type_variable ())))
		       defns) in
      let inner_env = (var_env @ env) in

      let type_check result (name, expr) = 
        let expr = type_check inner_env expr in
	let expr_type = type_of_expression expr in
          match expr_type with
            | `Function _ ->(
		  unify (snd (assoc name var_env), expr_type);
		  (name, expr) :: result)
            | kind -> Errors.letrec_nonfunction (pos_of_expression expr) (expr, kind) in

      let defns = fold_left type_check [] defns in
      let defns = rev defns in

      let env = (alistmap (fun value -> 
			     (generalize env (type_of_expression value))) defns
		 @ env) in
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
    : Kind.environment -> (string * untyped_expression) list -> (Kind.environment * (string * expression) list) =
  fun env defs ->
    let env = environment_to_inference_environment env in
    let new_type_env, new_defs = type_check_mutually env defs in
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

let type_expression : Kind.environment -> untyped_expression -> (Kind.environment * expression) =
  fun env untyped_expression ->
    let env = environment_to_inference_environment env in
    let env', exp' =
      match untyped_expression with
	| Define (variable, value, loc, pos) ->
	    (*let var_type = ITO.fresh_type_variable () in*)
	    let value = type_check env value in
	    let value_type = if is_value value then (generalize env (type_of_expression value))
            else [], type_of_expression value in
              (((variable, value_type) :: env),
    	       Define (variable, value, loc, (pos, type_of_expression value, None)))
	| expr -> let value = type_check env expr in env, value
    in
      inference_environment_to_environment env', inference_expression_to_expression exp'

let type_program : Kind.environment -> untyped_expression list -> (Kind.environment * expression list) =
  fun env exprs ->
    let type_group (env, typed_exprs) : untyped_expression list -> (Kind.environment * expression list) = function
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

(** message typing trick.
    This might be better off somewhere else (but where?).
**)
module RewriteSyntaxU = 
  Rewrite.Rewrite
    (Rewrite.SimpleRewrite
       (struct
          type t = Syntax.untyped_expression
          type rewriter = t -> t option
          let process_children = Syntax.perhaps_process_children
        end))

module RewriteSyntax = 
  Rewrite.Rewrite
    (Rewrite.SimpleRewrite
       (struct
          type t = Syntax.expression
          type rewriter = t -> t option
          let process_children = Syntax.perhaps_process_children
        end))

let add_parameter : RewriteSyntaxU.rewriter = function
  | Abstr (_,_,d) as e -> Some (Abstr ("_MAILBOX_", e, d))
  | Apply (f,a,d)      -> Some (Apply (Apply (f, Variable ("_MAILBOX_", Sugar._DUMMY_POS), Sugar._DUMMY_POS), a, d))
  | _                  -> None
and remove_parameter : RewriteSyntax.rewriter = function
  | Abstr ("_MAILBOX_", (Abstr (f,a,_)), d)              -> Some (Abstr (f,a,d))
  | Apply (Apply (f,Variable ("_MAILBOX_", _),_), a, d) -> Some (Apply (f,a,d))
  | _                                                   -> None

let add_parameter s = fromOption s (RewriteSyntaxU.bottomup add_parameter s)
and remove_parameter s = fromOption s (RewriteSyntax.bottomup remove_parameter s)

module RewriteKind = 
  Rewrite.Rewrite
    (Rewrite.SimpleRewrite
       (struct
          type t = Kind.kind
          type rewriter = t -> t option
          let process_children = Kind.perhaps_process_children
        end))
    
type tvar = [`TypeVar of int]

(* rewrite an unquantified kind type *)
let retype_primfun (var : Kind.kind) : RewriteKind.rewriter = function
  | `Function (j, k) as f -> Some (`Function (var, f))
  | _                     -> None

(* rewrite a quantified kind type *)
let retype_primfun (var : tvar) (quants, kind as k : Kind.assumption) =
  match RewriteKind.bottomup (retype_primfun (var :> Kind.kind)) kind with
    | None -> k
    | Some kind -> ((var :> Kind.quantifier) :: quants, kind)

(* find a suitable tvar name *)
let new_typevar quants =
  let tint = function
    | `TypeVar i
    | `RowVar  i -> i 
  and candidates = Utility.fromTo 0 (1 + List.length quants)
  in 
    (* Create a type variable not already in the list *)
    `TypeVar (List.hd (snd (List.partition
                              (flip mem (List.map tint quants)) 
                              candidates)))

(* Find a suitable type variable and rewrite a quantified kind type *)
let retype_primfun (quants, kind as k) =
  retype_primfun (new_typevar quants) k

(* Finally, a rewriter for type environments.  Ignore spawn, recv and
   self (which should perhaps be syntax tree nodes). *)
let retype_primitives = 
  let specials = ["spawn"; "recv"; "self"] in
    List.map (function
                | name, kind when mem name specials -> (name, kind)
                | name, kind -> name, retype_primfun kind)

let type_program env exprs = 
  let env, exprs = type_program env (List.map add_parameter exprs) in
    env, List.map remove_parameter exprs

and type_expression env e =
  let env, e = type_expression env (add_parameter e) in
    env, remove_parameter e
