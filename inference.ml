(*pp deriving *)
open List

open Utility
open Syntax
open Types
open Errors

(* debug flags *)
let show_typechecking = Settings.add_bool("show_typechecking", false, `User)

let show_recursion = Instantiate.show_recursion

let db_descriptor_type =
  snd (fst (Parse.parse_string Parse.datatype "(driver:String, name:String, args:String)"))

(* extract data from inference_expressions *)
let type_of_expression : expression -> datatype =
  fun exp -> let `T (_, t, _) = expression_data exp in t
let pos_of_expression : expression -> position =
  fun exp -> let `T (pos, _, _) = expression_data exp in pos

type typing_environment = environment * alias_environment

let rec extract_row : typing_environment -> datatype -> row = fun ((env, alias_env) as typing_env) t ->
  match t with
    | `Record row -> row
    | `Variant row -> row
    | `MetaTypeVar point ->
        begin
          match Unionfind.find point with
            | `Body t -> extract_row typing_env t
            | _ -> failwith
                ("Internal error: attempt to extract a row from a datatype that is not a record or variant: " ^ (string_of_datatype t))
        end
    | `Application (s, ts) ->
        let vars, alias = lookup_alias (s, ts) alias_env in
          extract_row typing_env (Instantiate.alias (vars, alias) ts)
    | _ -> failwith
        ("Internal error: attempt to extract a row from a datatype that is not a record or variant: " ^ (string_of_datatype t))

module Env = Env.String

let instantiate = Instantiate.var
let generalise = Generalise.generalise
let register_alias = Types.register_alias

let unify = Unify.datatypes
let unify_rows = Unify.rows

let type_mismatch ~expected ~inferred ~pos ~src msg =
  raise (Type_error (pos, 
                     src ^" has type "^string_of_datatype inferred
                     ^" but is annotated with type "
                     ^ string_of_datatype expected^"\n"^
                       msg))

let constant_type = function
  | Boolean _ -> `Primitive `Bool
  | Integer _ -> `Primitive `Int
  | Float _ -> `Primitive `Float
  | Char _ -> `Primitive `Char
  | String _ -> string_type

let rec type_check : typing_environment -> untyped_expression -> expression =
  fun ((env, alias_env) as typing_env) expression ->
    let unify = unify alias_env
    and unify_rows = unify_rows alias_env in
  try
    Debug.if_set (show_typechecking) (fun () -> "Typechecking expression: " ^ (string_of_expression expression));
    match (expression : Syntax.untyped_expression) with
  | Constant (value, `U pos) -> Constant (value, `T (pos, constant_type value, None))
  | Variable (name, `U pos) ->
      Variable (name, `T (pos, instantiate env name, None))
  | Abs (f, `U pos) ->
      let f = type_check typing_env f
      and arg_type = `Record (make_empty_open_row ()) 
      and result_type = fresh_type_variable ()
      and mb_type = fresh_type_variable () in
        unify (type_of_expression f, 
               `Function (make_tuple_type [arg_type], 
                          mb_type, 
                          result_type));
        let etype =  `Function (arg_type, mb_type, result_type) in
        Abs (f, `T (pos, etype, None))
  | App (f, p, `U pos) ->
      let f = type_check typing_env f
      and p = type_check typing_env p 
      and result_type = fresh_type_variable ()
      and mb_type = instantiate env "_MAILBOX_" 
      in
        (* not really necessary, but might catch some errors *) 
        unify (type_of_expression p, `Record (make_empty_open_row ())); 
        unify (type_of_expression f,
               `Function (type_of_expression p, mb_type, result_type));
        App (f, p, `T (pos, result_type, None))
  | Apply (f, ps, `U pos) ->
      let f = type_check typing_env f in
      let ps = List.map (type_check typing_env) ps in
      let mb_type = instantiate env "_MAILBOX_" in
      let f_type = type_of_expression f in
      let arg_type = make_tuple_type (List.map type_of_expression ps) in
      let return_type = fresh_type_variable () in
        begin
          try
            unify (`Function(arg_type, mb_type, return_type), f_type);
            Apply (f, ps, `T (pos, return_type, None))
          with Unify.Failure _ ->
            mistyped_application pos (f, f_type) (ps, List.map type_of_expression ps) (Some mb_type)
        end
  | Condition (if_, then_, else_, `U pos) ->
      let if_ = type_check typing_env if_ in
      let _ = 
        try unify (type_of_expression if_, `Primitive `Bool)
        with Unify.Failure _ -> 
          mistype
            (pos_of_expression if_)
            (if_, type_of_expression if_)
            (`Primitive `Bool) in
      let then_ = type_check typing_env then_ in
      let else_ = type_check typing_env else_ in
        unify (type_of_expression then_, type_of_expression else_);
        Condition (if_, then_, else_, `T (pos, type_of_expression then_, None)) 
  | Comparison (l, oper, r, `U pos) ->
      let l = type_check typing_env l in
      let r = type_check typing_env r in
	unify (type_of_expression l, type_of_expression r);
        Comparison (l, oper, r, `T (pos, `Primitive `Bool, None))
  | Abstr (variables, body, `U pos) ->
      let mb_type = fresh_type_variable () in
      let mapping = map2 (fun n v -> (string_of_int n, v, fresh_type_variable ())) (fromTo 1 (1 + List.length variables)) variables in
      let body_env = 
        fold_right
          (fun (_,v, vtype) env -> Env.bind env (v, ([], vtype)))
          mapping
          (Env.bind env ("_MAILBOX_", ([], mb_type))) in          
      let body = type_check (body_env, alias_env) body in
      let tuple = make_tuple_type (List.map thd3 mapping) in
      let type' = `Function (tuple, mb_type, type_of_expression body) in
	Abstr (variables, body, `T (pos, type', None))
  | Let (variable, value, body, `U pos) ->
      let value = type_check typing_env value in
      let vtype = (if is_value value then (generalise env (type_of_expression value))
                   else ([], type_of_expression value)) in
      let body = type_check (Env.bind env (variable, vtype), alias_env) body in        
	Let (variable, value, body, `T (pos, type_of_expression body, None))
  | Rec (variables, body, `U pos) ->
      let best_typing_env, vars = type_check_mutually typing_env variables in
      let body = type_check best_typing_env body in
	Rec (vars, body, `T (pos, type_of_expression body, None))
  | Xml_node (tag, atts, cs, `U pos) -> 
      let contents = map (type_check typing_env) cs in
      let atts = map (fun (k,v) -> k, type_check typing_env v) atts in
        List.iter (fun node -> unify (type_of_expression node, xml_type)) contents;
        List.iter (fun (_, node) -> unify (type_of_expression node, string_type)) atts;
        Xml_node (tag, atts, contents, `T (pos, xml_type, None))
  | Record_intro (bs, r, `U pos) ->
      let bs, field_env, absent_field_env =
        StringMap.fold (fun label e (bs, field_env, absent_field_env)  ->
                          let e = type_check typing_env e in
                          let t = type_of_expression e in
                            (StringMap.add label e bs,
                             StringMap.add label (`Present t) field_env,
                             StringMap.add label `Absent absent_field_env))
          bs (StringMap.empty, StringMap.empty, StringMap.empty)
      in
        begin
          match r with
            | None ->
                Record_intro (bs, None, `T (pos, `Record (field_env, Unionfind.fresh `Closed), None))
            | Some r ->
                let r = type_check typing_env r in
                let rtype = type_of_expression r in
                  (* make sure rtype is a record type! *)
                  
                  unify(rtype, `Record (absent_field_env, fresh_row_variable()));
                  
                  let (rfield_env, rrow_var), _ = unwrap_row (extract_row typing_env rtype) in
                    
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
                                                          (Record_intro (bs, None, `T (pos, `Record (field_env, Unionfind.fresh `Closed), None)))^
                                                          " (of type"^string_of_datatype (`Record (field_env, Unionfind.fresh `Closed))^
                                                          ") because the labels overlap")
                                            else
                                              StringMap.add label t field_env') rfield_env field_env in
                    Record_intro (bs, Some r, `T (pos, `Record (field_env', rrow_var), None))                  
        end           
  | Record_selection (label, label_variable, variable, value, body, `U pos) ->
      let value = type_check typing_env value in
      let label_variable_type = fresh_type_variable () in
	unify (type_of_expression value, `Record (make_singleton_open_row (label, `Present (label_variable_type))));

	let value_row = extract_row typing_env (type_of_expression value) in
	let body_env = (Env.bind
                          (Env.bind env (variable, ([], `Record (row_with (label, `Absent) value_row))))
                          (label_variable, ([], label_variable_type))) in
	let body = type_check (body_env, alias_env) body in
	let body_type = type_of_expression body in
	  Record_selection (label, label_variable, variable, value, body, `T (pos, body_type, None))
  | Project (expr, label, `U pos) ->
      let expr = type_check typing_env expr in
      let label_variable_type = fresh_type_variable () in
	unify (type_of_expression expr, `Record (make_singleton_open_row (label, `Present (label_variable_type))));
        Project (expr, label, `T (pos, label_variable_type, None))
  | Erase (value, label, `U pos) ->
      let value = type_check typing_env value in
      let value_row = extract_row typing_env (type_of_expression value) in
        Erase (value, label, `T (pos, `Record (row_with (label, `Absent) value_row), None))
  | Variant_injection (label, value, `U pos) ->
      let value = type_check typing_env value in
      let type' = `Variant (make_singleton_open_row (label, `Present (type_of_expression value))) in
        Variant_injection (label, value, `T (pos, type', None))
  | Variant_selection (value, case_label, case_variable, case_body, variable, body, `U pos) ->
      let value = type_check typing_env value in
      let value_type = type_of_expression value in
      
      let case_var_type = fresh_type_variable() in
      let body_row = make_empty_open_row () in
      let variant_type = `Variant (row_with (case_label, `Present case_var_type) body_row) in
	unify (variant_type, value_type);

	let case_body = type_check ((Env.bind env (case_variable, ([], case_var_type))), alias_env) case_body in

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
	let body_var_type = `Variant (row_with (case_label, `Absent) body_row) in
	let body = type_check ((Env.bind env (variable, ([], body_var_type))), alias_env) body in

	let case_type = type_of_expression case_body in
	let body_type = type_of_expression body in
	  unify (case_type, body_type);
	  Variant_selection (value, case_label, case_variable, case_body, variable, body, `T (pos, body_type, None))
  | Variant_selection_empty (value, `U pos) ->
      let value = type_check typing_env value in
      let new_row_type = `Variant (make_empty_closed_row()) in
        unify(new_row_type, type_of_expression value);
        Variant_selection_empty (value, `T (pos, fresh_type_variable (), None))
  | Nil (`U pos) ->
      Nil (`T (pos, `Application ("List", [fresh_type_variable ()]), None))
  | List_of (elem, `U pos) ->
      let elem = type_check typing_env elem in
	List_of (elem,
		 `T (pos, `Application ("List", [type_of_expression elem]), None))
  | Concat (l, r, `U pos) ->
      let tvar = fresh_type_variable () in
      let l = type_check typing_env l in
	unify (type_of_expression l, `Application ("List", [tvar]));
	let r = type_check typing_env r in
	  unify (type_of_expression r, type_of_expression l);
	  let type' = `Application ("List", [tvar]) in
	    Concat (l, r, `T (pos, type', None))
  | For (expr, var, value, `U pos) ->
      let value_tvar = fresh_type_variable () in
      let expr_tvar = fresh_type_variable () in
      let value = type_check typing_env value in
	unify (type_of_expression value, `Application ("List", [value_tvar]));
	let expr_env = Env.bind env (var, ([], value_tvar)) in
	let expr = type_check (expr_env, alias_env) expr in
	  unify (type_of_expression expr, `Application ("List", [expr_tvar]));
	  let type' = type_of_expression expr in
	    For (expr, var, value, `T (pos, type', None))
  | Call_cc(arg, `U pos) -> 
      (* TBD: Make this a primitive function (need to pass c.c. to prims). *)
      let arg = type_check typing_env arg in
      let contrettype = fresh_type_variable () in
      let anytype = fresh_type_variable () in
      let mailboxtype = 
          instantiate env "_MAILBOX_" in
      let conttype =
        `Function (make_tuple_type [contrettype], mailboxtype, anytype) in
      let argtype = `Function (make_tuple_type [conttype], mailboxtype, contrettype) in
        unify (argtype, type_of_expression arg);
        Call_cc(arg, `T (pos, contrettype, None))
  | Database (params, `U pos) ->
      let params = type_check typing_env params in
        unify (type_of_expression params, db_descriptor_type);
        Database (params, `T (pos, `Primitive `DB, None))
  | TableQuery (ths, query, `U pos) ->
      let row =
	(List.fold_right
	   (fun (expr, alias) env -> 
              match expr with 
                | `F field -> 
                    StringMap.add alias (`Present field.SqlQuery.ty) env
                | _ -> assert(false) (* can't handle other kinds of expressions *))
	   query.SqlQuery.cols StringMap.empty, Unionfind.fresh `Closed) in
      let datatype =  `Application ("List", [`Record row]) in
      let rrow = make_empty_open_row () in
      let wrow = make_empty_open_row () in
      let ths = alistmap (type_check typing_env) ths
      in
        Utility.for_each ths 
          (fun _, th -> 
             unify (type_of_expression th, `Table (`Record rrow, `Record wrow)));
	unify_rows (row, rrow);
        TableQuery (ths, query, `T (pos, datatype, None))
  | TableHandle (db, tableName, (readtype, writetype), `U pos) ->
      let datatype =  `Table (readtype, writetype) in
      let db = type_check typing_env db in
      let tableName = type_check typing_env tableName in
	unify (type_of_expression db, `Primitive `DB);
	unify (type_of_expression tableName, string_type); 
        TableHandle (db, tableName, (readtype, writetype), `T (pos, datatype, None))
  | SortBy(expr, byExpr, `U pos) ->
      (* FIXME: the byExpr is typed freely as yet. It could have any
         orderable type, of which there are at least several. How to
         resolve this? Would kill for type classes. *)
      let byExpr = type_check typing_env byExpr in
      let expr = type_check typing_env expr in
        SortBy(expr, byExpr, `T (pos, type_of_expression expr, None))
  | Wrong (`U pos) ->
      Wrong(`T (pos, fresh_type_variable(), None))
  | HasType(expr, datatype, `U pos) ->
      let expr = type_check typing_env expr in
      let expr_type = type_of_expression expr in
      let inference_datatype = datatype in
        free_alias_check alias_env inference_datatype;
        begin
          try unify(expr_type, inference_datatype);
          with Unify.Failure error -> 
            match error with
              | `Msg msg -> 
                  let _,_,src = position expr in
                    type_mismatch
                      ~expected:inference_datatype
                      ~inferred:expr_type
                      ~src:src
                      ~pos:pos msg
              | `PresentAbsentClash _ ->
                  assert false
        end;
	HasType(expr, datatype, `T (pos, inference_datatype, None))
 with 
   | UndefinedVariable msg
   | UndefinedAlias msg
   | Unify.Failure (`Msg msg) ->
       raise (Type_error(position expression, msg))
   | Unify.Failure (`PresentAbsentClash (label, lrow, rrow)) ->
       raise (Type_error(position expression, 
                         ("Rows\n "^ string_of_row lrow
	                  ^"\nand\n "^ string_of_row rrow
	                  ^"\n could not be unified because they have conflicting fields")))
         (* end "type_check" *)

(** type_check_mutually
    Companion to "type_check"; does mutual type-inference

    [QUESTIONS]
      - what are the constraints on the definitions?
      - do the functions have to be recursive?
*)
and
    type_check_mutually (env, alias_env) (defns : (string * untyped_expression * Types.datatype option) list) =
      let var_env = (fold_right (fun (name, _, t) env' ->
                                   Env.bind env' (name,
                                     (match t with
                                        | Some t -> (generalise env t)
                                        | None -> ([], fresh_type_variable ()))))
		       defns Env.empty) in
      let inner_env = Env.extend env var_env in
      let type_check result (name, expr, t) =
        let expr = type_check (inner_env, alias_env) expr in
        let t' = type_of_expression expr in
          match t' with
            | `Function _ as f  ->
                let t'' = snd (Env.lookup var_env name) in
		  unify alias_env (f, t'');
                  (* [HACK]

                     This allows aliases to persist providing no
                     mailbox types have been instantiated.
                  *)
                  let expr = 
                    if (Types.is_mailbox_free alias_env t') then
                      set_node_datatype (expr, t'')
                    else
                      expr
                  in
		    (name, expr, t) :: result
            | datatype -> Errors.letrec_nonfunction (pos_of_expression expr) (expr, datatype) in

      let defns = fold_left type_check [] defns in
      let defns = rev defns in

      let env = Env.extend env 
        (List.fold_right (fun (name, value,_) env' -> 
		            Env.bind env' (name, (generalise env (type_of_expression value)))) defns Env.empty)
	
      in
        (env, alias_env), defns     

let mutually_type_defs
    ((env, alias_env) : Types.typing_environment)
    (defs : (string * untyped_expression * 'a option) list)
    : (Types.typing_environment * (string * expression * 'c) list) =
  let (new_type_env, new_alias_env), new_defs = type_check_mutually (env, alias_env) defs
  in
    ((new_type_env, new_alias_env), new_defs)

let type_expression : Types.typing_environment -> untyped_expression -> (Types.typing_environment * expression) =
  fun typing_env exp ->
    typing_env, type_check typing_env exp

let type_definition : Types.typing_environment -> untyped_definition -> (Types.typing_environment * definition) =
  fun (env, alias_env) def ->
    let (env', alias_env'), def' =
      match def with
	| Define (variable, value, loc, `U pos) ->
	    let value = type_check (env, alias_env) value in
	    let value_type = if is_value value then 
              (generalise env (type_of_expression value))
            else [], type_of_expression value in
              ((Env.bind env (variable, value_type)), alias_env),
    	    Define (variable, value, loc, `T (pos, type_of_expression value, None))
        | Alias (typename, vars, datatype, `U pos) ->
            (env,
             register_alias (typename, vars, datatype) alias_env),
            Alias (typename, vars, datatype, `T (pos, `Record (make_empty_closed_row ()), None))
        | Alien (language, name, assumption, `U pos)  ->
            let (qs, k) = assumption
            in
              ((Env.bind env (name, (qs, k))), alias_env), Alien (language, name, assumption, `T (pos, k, None))
    in
      (env', alias_env'), def'


let type_program : Types.typing_environment -> untyped_program -> (Types.typing_environment * program) =
  fun typing_env (Program (defs, body)) ->
    let type_group (typing_env, typed_defs) : untyped_definition list ->
      Types.typing_environment * definition list = function
        | [x] -> (* A single node *)
	    let typing_env, def = type_definition typing_env x in 
              typing_env, typed_defs @ [def]
        | xs  -> (* A group of potentially mutually-recursive definitions *)
            let defparts = map (fun (Define x) -> x) xs in
            let defbodies = map (fun (name, Rec ([(_, expr, t)], _, _), _, _) -> 
                                   name, expr, t) defparts in
            let (typing_env : Types.typing_environment), defs = mutually_type_defs typing_env defbodies in
            let defs = (map2 (fun (name, Rec ([(_, _, t)], _, _), location, _) (_, expr, _) ->
                                let ed = expression_data expr in
                                let expr = Rec ([(name, expr, t)], Variable (name, ed), ed) in
                                  Define(name, expr, location, ed))
			  defparts defs) in
              typing_env, typed_defs @ defs

    and bothdefs l r = match l, r with
      | Define (_, Rec _, _, _), Define (_, Rec _, _, _) -> true
      | _ ->  false
    in
    let def_seqs = groupBy bothdefs defs in
    let mutrec_groups = (Callgraph.refine_def_groups def_seqs) in
    let typing_env, defs =
      fold_left type_group (typing_env, []) mutrec_groups in
    let typing_env, body = type_expression typing_env body in
      typing_env, (Program (defs, body))

(* Check for duplicate top-level definitions.  This probably shouldn't
   appear in the type inference module.

   (Duplicate top-level definitions are simply not allowed.)

   In future we should probably allow duplicate top-level definitions, but
   only if we implement the correct semantics!
*)
let check_for_duplicate_defs 
    (type_env, _)
    (defs :  untyped_definition list) =
  let check (env, defined) = function
    | Define (name, _, _, `U position) when StringMap.mem name defined ->
        (env, StringMap.add name (position :: StringMap.find name defined) defined)
    | Define (name, _, _, `U position) when StringSet.mem name env ->
        (env, StringMap.add name [position] defined)
    | Define (name, _, _, _) ->
        (StringSet.add name env, defined)
    | _ -> 
        (env, defined) in 
  let env = Env.domain type_env in
  let _, duplicates = List.fold_left check (env,StringMap.empty) defs in
    if not (StringMap.is_empty duplicates) then
      raise (Errors.MultiplyDefinedToplevelNames duplicates)

let type_program (typing_env : Types.typing_environment) (Program (defs, _) as program) =
  check_for_duplicate_defs typing_env defs;
  Debug.if_set (show_typechecking) (fun () -> "Typechecking program...");
  type_program typing_env program

let type_expression typing_env expression =
  Debug.if_set (show_typechecking) (fun () -> "Typechecking expression...");
  type_expression typing_env expression
