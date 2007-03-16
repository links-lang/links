(** Optimisation passes: rewrites of the AST. *)

open List

open Utility
open Rewrite
open Syntax
open Sql_transform

let optimising = Settings.add_bool("optimising", true, `User)
let show_opt_verbose = Settings.add_bool("show_opt_verbose", false, `User)
let show_optimisation = Settings.add_bool("show_optimisation", false, `User)
let reduce_recs = Settings.add_bool("reduce_recursion", false, `User)

(**
   Check that "Project" and "Erase" do not occur in the syntax tree.
*)
let no_project_erase : RewriteSyntax.rewriter = function
  | Project _ 
  | Erase _ -> assert false
  | _ -> None

(** [pure]

    Checkes whether the evaluation of an expression is known to be free
    from side effects
*)
let pure : expression -> bool = 
  (* Everything is pure except the application of certain primitive
     functions and of any functions which call those.  For now, we'll
     just punt when we see a function application.  Eventually the
     type system will help us out.

     NB: continuation invocation is impure in the sense that we can't
     replace `x = f(3); 4' with `4' if `f' is a continuation.
  *)
  let rec pure default = function 
      (* TBD: annotate ALL prim funcs as to pureness *)
    | Apply((Variable("take", _) | Variable("drop", _)), arg, _)
      -> pure default arg
    | Apply _    -> false
    | TableQuery _ -> false
        (* Is callCC pure? *)
    | e       -> default e
  and all_true l = fold_right (&&) l true in
    reduce_expression pure (all_true -<- snd)


(** Inlining **)

(* Number of nodes in a syntax tree *)
let countNodes e = 
  let count = ref 0 in 
    Syntax.reduce_expression (fun default e -> incr count; default e) (fun _ -> ()) e; 
    !count;;

(* Inline small, non-recursive functions *)
let contains_no_extrefs : Syntax.expression -> bool =
  (=) [] -<- List.filter (not -<- flip List.mem_assoc Library.type_env) -<- freevars

let recursivep : Syntax.expression -> bool = function
  | Rec ([(name, fn, _)], Variable (v, _), _) when v = name 
      -> List.mem name (freevars fn)
  | _ -> false

let size_limit = 150

let is_inline_candidate' = function
  | Define (_, (Rec _ as e), _, _) -> 
      not (recursivep e) && contains_no_extrefs e && countNodes e < size_limit
  | Define (_, e, _, _) when Syntax.is_value e -> contains_no_extrefs e && pure e
  | _ -> false

let find_inline_candidates es : (string * expression * location) list = 
  let is_inline_candidate = function
    | Define (name, rhs, location, _) as e when is_inline_candidate' e -> 
        [name, rhs, location]
    | _ -> []
  in Utility.concat_map is_inline_candidate es

let location_matches location = function
  | Define (_, _, location', _) -> location=location'
  | _ -> false

let perform_value_inlining location name rhs =
  List.map (fun exp ->
    if location_matches location exp then
      Syntax.subst_fast name rhs exp
    else
      exp)

let replaceApplication name var body : RewriteSyntax.rewriter = function
  | Apply (Variable (n, _), p, d) when n = name -> Some (Let (var, p, body, d))
  | _ -> None

let perform_function_inlining location name var rhs = 
  List.map
    (fun exp -> fromOption exp (
       if location_matches location exp then
	 RewriteSyntax.bottomup (replaceApplication name var rhs) exp
       else
	 None))

let inline program = 
  let valuedefp = function
    | _, Rec _, _ -> false
    | _        -> true
  in
  let candidates = find_inline_candidates program in
  let value_candidates, fn_candidates = List.partition valuedefp candidates in
  let program' = 
    List.fold_right 
      (fun (name, rhs, location) program ->
	 perform_value_inlining location name rhs program)
      value_candidates
      program
  in 
  let program'' = 
    List.fold_left 
      (fun program (_, rhs, location)  ->
         match rhs with
           | Rec ([(name, Abstr (v, body, _), _)], _, _) ->
	       perform_function_inlining location name v body program)
      program'
      fn_candidates
  in program''

(* Minimize the amount of recursion expressed in the syntax tree *)
let reduce_recursion : RewriteSyntax.rewriter = function
  | Rec (bindings, cont, data) ->
      let untyped_bindings = List.map (fun (name, expr, _) -> name, expr) bindings in
      let find_definition v = List.find (fun (name, expr, annotation) -> name = v) bindings in
      let recursive_p (name, expr, annot) =  mem name (freevars expr) in
      let cliques = Callgraph.group_and_order_bindings_by_callgraph untyped_bindings in
        begin match map (map find_definition) cliques with
          | [_::_::_] -> None (* one multi-element group.  Everything must be mutually-recursive *)
          | [[x]] when recursive_p x -> None (* One single-element group; element is recursive. *)
          | groups -> (* either a group consisting of a single-element non-recursive element 
                         or multiple groups *)
              Some (List.fold_right
                      (fun group body -> 
                         match group with 
                           | [(name, expr, annot)] when not (mem name (freevars expr)) ->
                               let rhs = match annot with
                                 | Some annot -> HasType (expr, annot, data)
                                 | None -> expr in
                                 Let (name, rhs, body, data)
                           | xs -> Rec (xs, body, data)) groups cont) end
  | _ -> None



(** [uniquify_expression]

    Give unique names to all local bindings.  Local names should be
    distinct from each other and from toplevel names

    After this, we can be much less careful about scope.
*)
let uniquify_expression : RewriteSyntax.rewriter = 
  let rewrite_node = function
    | Abstr (v, b, data) -> 
        let name = gensym ~prefix:v () in
          Some (Abstr (name, Syntax.rename_fast v name b, data))
    | Let (v, e, b, data) -> 
        let name = gensym ~prefix:v () in
          Some (Let (name, e, Syntax.rename_fast v name b, data))
    | Rec (vs, b, data) -> 
        let bindings = List.map (fun (name, _, _) -> (name, gensym ~prefix:name ())) vs in
        let rename = List.fold_right (fun (x, r) expr -> Syntax.rename_fast x r expr) bindings in
          Some(Rec(List.map (fun (n, v, t) -> (List.assoc n bindings, rename v, t)) vs,
                   rename b, data))
    | Record_selection (lab, lvar, var, value, body, data) ->
        let lvar' = gensym ~prefix:lvar ()
        and var'  = gensym ~prefix:var () in
          Some(Record_selection(lab, lvar', var', value, 
                                Syntax.rename_fast var var' (Syntax.rename_fast lvar lvar' body),
                                data))
    | For (b, v, src, data) -> 
        let name = gensym ~prefix:v () in
          Some (For(Syntax.rename_fast v name b, name, src, data))
    | Variant_selection (value, clab, cvar, cbody, var, body, data) ->
        let cvar' = gensym ~prefix:cvar ()
        and var'  = gensym ~prefix:var () in
          Some (Variant_selection (value, clab, 
                                   cvar', Syntax.rename_fast cvar cvar' cbody, 
                                   var',  Syntax.rename_fast var var' body,
                                   data))
    | _ -> None
  (* Note that this will only work bottomup, not topdown, since
     we need to replace bindings from the inside out *)
  in RewriteSyntax.bottomup rewrite_node

(*
(** [inference_rw]
    The type-inference function, in the form of a rewriter
*)
let inference_rw env : RewriteSyntax.rewriter = fun input -> 
  let output = snd (Inference.type_expression env (erase input)) in
    if input = output then None
    else Some output
*)

(** {0 Renaming} *)

(** renaming
    I think this is meant to "remove renamings", that is places where the user
    has written { var x = y; ... }
    It is incomplete, becuase the guard is very conservative. If we used
    capture-avoiding substitution, or ensured binder uniqueness in advance,
    we would be more complete.
*)
let renaming : RewriteSyntax.rewriter = 
  let bound_in var = 
    (* Is a particular name bound inside an expression? *)
    let binds default = function
      | Let (v, _, _, _)
      | Abstr (v, _, _)
      | Define (v, _, _, _)
          when v = var -> true
      | Record_selection (_, v1, v2, _, _, _)
      | Variant_selection (_, _, v1, _, v2, _, _) when var = v1 || var = v2 -> true
      | Rec (bindings, _, _) when List.exists (fun (v,_,_) -> v = var) bindings -> true
      | other -> default other
    and combiner l = fold_left (||) false l 
    in reduce_expression binds (combiner -<- snd) 
  in function 
        (* 1. Don't replace where there's an inner binding of the name `x'.
           2. Don't replace where there's an inner binding of the name `y'.
        *)
  | Let (y, Variable (x,  _), body,  _) when bound_in x body || bound_in y body
      -> None (* Could do better: does the binding shadow? *)
  | Let (y, Variable (x, _), body, _)
      -> Some (Syntax.rename_fast y x body)
  | _ -> None

(** [unused_variables]: Remove all let bindings where the name is not used 
    in the body.

    FIXME: also letrec ("transitively") and record selection
    operators.

    FIXME: needs to take account of variables used within Query
    expressions.
*)

let unused_variables : RewriteSyntax.rewriter = function
    (* This'd be quite a bit more efficient bottom-up, passing the
       list of free variables outwards rather than searching the body
       every time we find a let. *)
  | Let (var, expr, body, _) when pure expr 
                               && not (mem var (freevars body)) -> Some body
  | _ -> None
      (* FIXME: this ignores variables that are hidden inside queries *)



(** [simplify_regex] rewrites an expression as follows:
{
   s ~ let x in b
       let x in s ~ b
}

    (FIXME; Note there are lots of constructors that commute with let,
    and it might be quite wise to do so).

    In a regex match such as the following, the order of eval is unspecified:
    f() ~ /{g()}/
*)

let simplify_regex : RewriteSyntax.rewriter = function
  | Apply (Apply (Variable ("~", _), lhs, _) as a, Let (v, e, rhs, d1), d2)  ->
      Some (Let (v, e, Apply (a, rhs, d1), d2))
  | _ -> None

(** {3 SQL utility values} These values are provided to ease the writing of SQL optimisers. *)

(** extract_tests
    Finds tests that can be pushed to a SQL query. Such tests are of the form '{i if cond then collection else [col] }'.
    ¨param forbidden A list of variables that should not be used in the tests.
    @param bindings Bindings from variables to database fields.
    @param expr The expression from which the test must be extracted.
    @return A tuple (positive, negative, result, origin).
        `positive' and `negative' are query expressions. `result' is the 
        input expression with these tests removed. `origin` is a list of 
        record selections that need to be applied.
 *)
(*TODO: Is there any reason why positive and negative should be kept separate? *)
(*TODO: The calculated expression for the collection extension case
   is not valid *)
let rec extract_tests (bindings:bindings) (expr:expression)
    : (Query.expression list * expression * projection_source list) =
  match expr with
    | Let (_, Variable (_, _), _, _) ->
        failwith "TR115 (renaming declarations should have been removed by earlier optimisations)"
    | Let (variable, value, body, data) ->
        let (condns, body, origin) = extract_tests (`Unavailable variable :: bindings) body in
          (condns, Let (variable, value, body, data), origin)

    | Record_selection (label, label_variable, variable, Syntax.Variable (name, vdata), body, data) ->
        let trace_data = `Selected{field_name=label; field_var=label_variable;
                                   etc_var = variable; source_var = name} in
        let (condns, body, origin) = extract_tests (trace_data :: bindings) body in
          (condns,
	   Record_selection (label, label_variable, variable, 
			     Syntax.Variable (name, vdata), body, data), 
	   origin)
    | Record_selection (label, label_variable, variable, value, body, data) ->
        let condns, body, origin = extract_tests (`Unavailable variable :: bindings) body in
          (condns, Record_selection (label, label_variable, variable, value, body, data), origin)

    | Condition (condition, t, ((Nil _) as e), data)  ->
        let condns, t, origin = extract_tests bindings t in
          (match condition_to_sql condition bindings with
             | Some (sql_condition, new_origin) ->
                 (sql_condition:: condns, t, new_origin @ origin)
             | None ->
                 (condns, Condition(condition, t, e, data), origin))
    | Condition (condition, ((Nil _) as t), e, data) ->
        let condns, e, origin = extract_tests bindings e in
          (match condition_to_sql condition bindings with
             | Some (sql_condition, new_origin) ->
                 (Sql.negation sql_condition:: condns, e, new_origin @ origin)
             | None ->
                 (condns, Condition(condition, t, e, data), origin))
    | For (expr, variable, value, data) ->
        let condns, expr, origin = 
          extract_tests (`Calculated (variable, expr) :: bindings) expr in
          (condns, For (expr, variable, value, data), origin)
    | _ -> ([], expr, [])

(** {3 SQL optimisers} All following values are optimiser functions
    that can be applied to an expression. Generally, they try to push
    as much calculation to the DBMS as possible. {e Beware:} some
    optimisers have dependencies with other optimisers. *)

(** For every collection extension that has a table as source,
    modifies the SQL query associated to that table to only select
    fields that will be used in the body of the extension. {e Beware:}
    this version of the algorithm considers a field as used if a
    record selection operators extracts it from the record
    representing the relation from the database, even if it is not
    used in the selection's body. *)
type fieldset = All
              | Fields of (string list)
(* FIXME: Make sure we don't throw away fields that are used in 
   a SortBy clause. *)
let sql_projections ((env, alias_env):(Inferencetypes.environment * Inferencetypes.alias_environment)) : RewriteSyntax.rewriter =
  let merge_needed : fieldset list -> fieldset =
    let merge2 = function
      | All, _ | _, All -> All
      | Fields x, Fields y -> Fields (x @ y) in
      fold_left (curry merge2) (Fields []) in
  let needed_fields (var:string) : expression -> fieldset =
    let rec visitor (var:string) default : expression -> fieldset = function
        | Variable (name, _) when name = var -> All
        | Apply (apply, Variable (name, _), _) when name = var ->
            (match (
               match apply with
                 | Variable (name, _) -> snd (assoc name env)
                 | Abstr _ ->
                     (match snd (Inference.type_expression (env, alias_env) (erase apply)) with
                        | Abstr (_, _, `T (_, datatype, _)) -> datatype
                        | _ -> failwith "OP442")
                 | _ -> failwith "OP437"
             ) with
               | `Function (`Record (field_env, _), _, _) ->
		   let fields = StringMap.fold (fun label field_spec labels ->
						  match field_spec with
						    | `Present _ -> label :: labels
						    | `Absent -> labels) field_env []
		   in
		     merge_needed [Fields fields]
               | `TypeVar _ -> All
               | _ -> failwith "OP448")
        | Record_selection (label, _, variable, Variable (name, _), body, _) when name = var ->
            (* Note change of variable *)
            merge_needed (Fields [label] :: (visitor var default body) :: [visitor variable default body])
        | Record_selection (_, _, _, Variable (_, _), body, _) -> visitor var default body
        | other -> default other
    in reduce_expression (visitor var) (merge_needed -<- snd) in
  let rewrite = function
    | For (body, variable, TableQuery(th, query, tdata), data) ->
        (match needed_fields variable body with
           | Fields needs ->
               Some (For (body, variable,
                          TableQuery(th, 
                                     (if needs = [] then null_query else project needs) query,
                                     tdata),
                          data))
           | All -> None)
    | _ -> None in
    RewriteSyntax.bottomup rewrite

(** For every collection extension that has a table as source,
    modifies the SQL query associated to that table to add as many
    conditions from the extension's body to the query itself as
    possible. To be considered for optimisation, a condition must be
    of the form '{i if cond then [] else ...}' or '{i if cond then
    ... else []}', where '{i cond}' is a comparison between a
    constant and a variable or between two variables (coming from the
    database). The optimisation will stop looking for conditions in
    the body expression if it encouters any other node than a
    condition, a {i let}, a record selection or a collection
    extension. *)
let sql_selections : RewriteSyntax.rewriter = function 
  | For (expr, variable, TableQuery(th, query, tdata), data) ->
      let condns, expr, origin = extract_tests [`Table_loop (variable, query)] expr in
      let table = TableQuery(th, select condns query, tdata) in
        Some (select_by_origin origin (For (expr, variable, table, data)))
  | _ -> None

let substitute_projections' new_src renamings bindings expr =
  let subst_projection (from, to') : RewriteSyntax.rewriter = function
    | Record_selection (label, label_var, etc_var, Variable (src, d), body, 
                        data) as orig
        when from.Query.renamed = label ->
        Debug.if_set show_optimisation
          (fun () -> "Renaming " ^ from.Query.renamed ^ " to " ^ to' ^ " in " ^ string_of_expression orig);
        (match trace_variable src bindings with
	   | `Table query when mem from.Query.table_renamed (map snd query.Query.tables) ->
               Some(Record_selection(to', label_var, etc_var, 
                                     Variable (new_src, d), body, data))
           | `Table_field (table_as, _) when from.Query.table_renamed = table_as ->
               (* NOTE: I think this case never occurs *)
               Some (Record_selection (to', label_var, etc_var, Variable (new_src, d), body, data))
           | `Unavailable -> Debug.if_set show_optimisation
               (fun () -> src ^ " was unavailable."); None
           | `Earlier(str, _) -> Debug.if_set show_optimisation (fun () -> "Earlier: " ^ str); None
           | _ -> 
               Debug.if_set show_optimisation (fun () -> "NOT renaming after all!");
               Some (Record_selection (label, label_var, etc_var, 
                                       Variable (src, d), body,  (* not new_src? *)
                                       data)))
    | _ -> None
  in
    RewriteSyntax.all (List.map (fun r -> RewriteSyntax.bottomup (subst_projection r)) renamings) expr

let substitute_projections new_src renamings bindings expr = 
 do_rewrite (substitute_projections' new_src renamings bindings) expr

let read_proj = function
    Record_selection(field, _, _, record, 
                     Variable _, _) ->
      Some(record, field)
  | _ -> None

let rec sql_sort = function
  | SortBy(TableQuery(th, query, data1), 
           Abstr(loopVar, sortByExpr, _), _) ->
      (match read_proj sortByExpr with
           Some (Variable(sortByRecVar, _), sortByFld)
             when sortByRecVar = loopVar
               -> Some(TableQuery(th, Query.add_sorting query 
                               (`Asc(Query.owning_table sortByFld query,
				     sortByFld)), data1))
         | _ -> None)
  | _ -> None

let sql_aslist : RewriteSyntax.rewriter =
  function 
    | Apply(Variable("asList", _), th, (`T (pos,_,_) as data)) ->
        let th_type = node_datatype th in
        let th_row = match th_type with
          | `MetaTypeVar point ->
              (match Unionfind.find point with
                 | `Table (`Record th_row, _) -> th_row
                 | _ -> failwith "Internal Error: tables must have table type")
          |  `Table (`Record th_row, _) -> th_row
          | _ -> failwith "Internal Error: tables must have table type"
        in
        let table_alias = gensym ~prefix:"Table_" () in
	let rowFieldToTableCol colName = function
	  | `Present fieldType -> {Query.table_renamed = table_alias; 
                                   Query.name = colName; 
				   Query.renamed = colName; 
                                   Query.col_type = fieldType}
	  | _ -> failwith "Internal Error: missing field in row"
	in
	let fields, _ = th_row in
	let columns = StringMapUtils.zip_with rowFieldToTableCol fields in
        let th_var = match th with
          | Variable(var, _) -> var
          | _ -> gensym ~prefix:"_t" () in
	let select_all = {Query.distinct_only = false;
			  Query.result_cols = List.map inLeft columns;
			  Query.tables = [(`TableVariable th_var,
                                           table_alias)];
			  Query.condition = Query.Boolean true;
			  Query.sortings = [];
			  Query.max_rows = None;
			  Query.offset = Query.Integer (Num.Int 0)} in
        let th_list_type = `Application ("List", [`Record(th_row)]) in
        let table_query = TableQuery ([table_alias, 
                                       Variable (th_var, `T (pos, th_type, None))],
                                      select_all,
                                      `T (pos, th_list_type, None))
        in
          (match th with
             | Variable _ -> Some table_query
             | _ -> Some (Let (th_var, th, table_query, data)))
    | _ -> None

(** check_join
    Inspects an expression for possible collection extension
    operators that can be joined with some outer comprhsn, the
    variable of the outer comprhsn indicated by the `loop_var' param.

    @param forbid Variables that if used in the body of an inner 
    extension prevent the optimisation
    @param db The name of the outter database. Only tables of the 
           same database can be joined.
    @param bindings Bindings of variables to rows or fields from a table.
    @param expr The expression to inspect.
    @return Either None or Some of a tuple containing: 
      * positive join conditions (conditions between fields of the inner 
        and outer table);
      * negative join conditions;
      * the inner query to join;
      * the new Links-AST expression without the joinable collection extensions. 
*)
let rec check_join (loop_var:string) (bindings:bindings) (expr:expression)
    =
  let bindings, expr = sep_assgmts bindings expr in
    match expr with
      | Condition (condition, t, ((Nil _) as e), data)  ->
          (match check_join loop_var bindings t with
	     | Some (condns, th, query, projs, var, t) ->
                 Some (condns, th, query, projs, var, 
                       Condition (condition, t, e, data))
	     | None -> None)
      | Condition (condition, ((Nil _) as t), e, data) ->
          (match check_join loop_var bindings e with
	     | Some (condns, th, query, projs, var, e) ->
                 Some (condns, th, query, projs, var,
                       Condition (condition, t, e, data))
	     | None -> None)
      | For (expr, variable, TableQuery(th, query, _), _) ->
          (* TODO: Test whether both tables come from the same database. *)
          (match extract_tests (`Table_loop (variable, query) :: bindings) expr with
               (*  ([], _, _) -> None *)
	     | (condns, expr, origin) ->
                   Some (condns, th, query, 
                         origin, variable, expr))
      | _ -> None

(** sql_joins
    When a collection extension has a table as source, explore the
    body expression for other collection extensions on tables from the
    same database. If a suitable extension is found, the query of the
    outer extension is modified to query on a join between the union
    of their tables, and the inner extension is removed.
*)
let rec sql_joins : RewriteSyntax.rewriter = 
  function
    | For (body, outer_var, (TableQuery (outer_ths, query, tdata)), data) ->
        let bindings = [`Table_loop (outer_var, query)] in
        (match check_join outer_var bindings body with
           | Some(condns, inner_ths, inner_query, origins, inner_var, body) ->
               let renamings, query = join condns (query, inner_query) in
                 
               (* Replace anything of the form inner_var.field with 
                  outer_var.renamed_field with renamings as given by 
                  the join operator *)
               let body = (substitute_projections outer_var renamings 
                             [`Table_loop(inner_var, inner_query)] body) in
               let expr = For(body,
                              outer_var, 
                              TableQuery (inner_ths @ outer_ths, query, tdata), 
                              data) in
(*                (\* finally, wrap the whole expression in the  *)
(*                   projections returned from check_join; *)
(*                   TBD: Does this need to go somewhere? Inside the loop? *\) *)
(*                let expr = select_by_origin origins expr in *)
                 
                 Some expr

           | None -> None

(*            | None ->  *)
(*                (\* check_join returned None, so perhaps we only have one loop.  *)
(*                   still, try to push the conditions down into SQL.  *)
(*                   HACK ALERT; this shouldn't be a special case. the 1-join *)
(*                   should be handled the same as 2-join, 3-join, etc. *\) *)
(*                let (pos, neg, body, proj_srcs) = extract_tests bindings body in *)
(*                  (\* This positive/negatives business is retarded, I think *\) *)
(*                  if (pos <> [] || neg <> []) then *)
(*                    let query = {query with Query.condition = pos_and_neg (query.Query.condition::pos, neg) } in *)
(*                      Some (For(body, outer_var,  *)
(*                                TableQuery(th, query, tdata), data)) *)
(*                  else None *)
        ) (* match check_join .... with *)
    | _ -> None

let lift_lets : RewriteSyntax.rewriter = function
  | For(loopbody, loopvar, Let(letvar, letval, letbody, letdata), data) 
    -> Some(Let(letvar, letval,
                For(loopbody, loopvar, letbody, data), letdata))
  | For(Let(letvar, letval, letbody, letdata), loopvar, src, data)
      when not (mem loopvar (freevars letval))
        && pure letval
        -> Some(Let(letvar, letval, For(letbody, loopvar, src, data), letdata))
  | Condition(cond, Let(letvar, letval, letbody, letdata), e, data)
      when pure letval
        -> Some(Let(letvar, letval, Condition(cond, letbody, e, data), letdata))
  | Condition(cond, t, Let(letvar, letval, letbody, letdata), data)
      when pure letval
        -> Some(Let(letvar, letval, Condition(cond, t, letbody, data), letdata))
  | Apply(Let(letvar, letval, letbody, letdata), ap_arg, data)
        -> Some(Let(letvar, letval, Apply(letbody, ap_arg, data), letdata))
  | Apply(ap_func, Let(letvar, letval, letbody, letdata), data)
      when pure ap_func
        -> Some(Let(letvar, letval, Apply(ap_func, letbody, data), letdata))
  | SortBy(Let(letvar, letval, letbody, letdata), byExpr, data) ->
      Some (Let(letvar, letval, SortBy(letbody, byExpr, data), letdata))
  | _ -> None


(* return true if the argument is an atom *)
let is_atom = function
  | Variable _ | Integer _ -> true
  | _ -> false

(* if e is not an atom then bind it to a variable by extending the
   continuation k return the new continuation and an atomic expression
   representing e
 *)
let lift_let data e k =
  if is_atom e then
    (fun body -> k body), e
  else
    let x = gensym () in
      (fun body -> k (Let (x, e, body, data))), Variable (x, expression_data e)



(** (1 take/drop optimization).
    Push calls to take and drop that surround queries into the query.
    [N.B. these rewrite rules play fast and loose with the `data'
    component of expression nodes.  Don't assume anything about the
    data after these have run.]
*)

(** [simplify_takedrop]
    The rewritings are as follows:
       take e1 (drop e2 e3) ~>  {x = e2; y = e1; take y (drop x e3)}
    (Not performed if both e1 and e2 are variables or integer literals)
       take e1 e2 ~> {x = e1; take x e2}
       drop e1 e2 ~> {x = e1; drop x e2}
    (Not performed if e1 is a variable or integer literal)
*)
let simplify_takedrop : RewriteSyntax.rewriter = function
  | Apply (Variable ("take"|"drop" as f, d1),
           Record_intro (fields1, None, d2), d3) ->
      let k, e1 = lift_let d3 (StringMap.find "1" fields1) (fun x -> x) in
        begin
          match f, StringMap.find "2" fields1 with
            | "take", Apply (Variable ("drop", d4),
                             Record_intro(fields2, None, d5), d6) ->                
                let k, e2 = lift_let d3 (StringMap.find "1" fields2) k
                in
                  Some(k(Apply (Variable ("take", d1),
                                Record_intro (
                                  ((StringMap.add "1" e1) ->-
                                     (StringMap.add "2"
                                        (Apply (Variable ("drop", d4),
                                                Record_intro (
                                                  ((StringMap.add "1" e2) ->-
                                                     StringMap.add "2" (StringMap.find "2" fields2)) StringMap.empty,
                                                  None, d5), d6)))) StringMap.empty, None, d2), d3)))
            | _ ->
                Some (k (Apply (Variable (f, d1),
                                Record_intro (
                                  ((StringMap.add "1" e1) ->-
                                     (StringMap.add "2" (StringMap.find "2" fields1))) StringMap.empty, None, d2), d3)))
        end
  | _ -> None

(** [push_takedrop] actually pushes [take] and [drop] calls into a query.
    Rewrites as follows: {[
        take e1 (drop e2 (Table (... q ...))) ~> Table (... {q with offset = e2; limit = e1} ...)
    }] where e1 and e2 are variables or integer literals
    {[
        take e1 (Table (... q ...)) ~> Table (... {q with limit  = e1} ...)
        drop e1 (Table (... q ...)) ~> Table (... {q with offset = e1} ...)
    }] where e1 is a variable or integer literal
*)
let push_takedrop : RewriteSyntax.rewriter = 
  let queryize = function
    | Variable (v, _) -> Query.Variable v
    | Integer  (n, _) -> Query.Integer n
    | _ -> failwith "Internal error during take optimization" in 
  function
    | Apply (Variable ("take", _) as f,
             Record_intro(fields1, None, d1), d2) ->
        let e1 = StringMap.find "1" fields1 in
          if is_atom e1 then
            begin
              match StringMap.find "2" fields1 with
                | TableQuery (e2, q, d) ->
	            Some (TableQuery (e2, {q with Query.max_rows = Some (queryize e1)}, d))
                | Apply (Variable ("drop", _),
                         Record_intro (fields2, None, _), _) ->
                    let e2 = StringMap.find "1" fields2 in
                      if is_atom e2 then
                        begin
                          match StringMap.find "2" fields2 with
                            | TableQuery(e3, q, d) ->
                                Some (TableQuery (e3, {q with
                                                         Query.max_rows = Some (queryize e1);
                                                         Query.offset   = queryize e2}, d))
                            | _ -> None
                        end
                      else None
                | For(List_of(expr, ldata) as body, var, src, fordata) when pure(expr) ->
                    Some (For(body, var,
                              Apply (f,
                                     Record_intro (
                                       ((StringMap.add "1" e1) ->-
                                          (StringMap.add "2" src)) StringMap.empty, None, d1), d2), fordata))
                | _ -> None
            end
          else None
    | Apply (Variable ("drop", _) as f,
             Record_intro(fields, None, d1), d2) ->
        let e1 = StringMap.find "1" fields in
          if is_atom e1 then
            begin
              match StringMap.find "2" fields with
                | TableQuery (e2, q, d) ->
  	            Some (TableQuery (e2, {q with Query.offset = queryize e1}, d))
                | For(List_of(expr, ldata) as body, var, src, fordata) when pure(expr) ->
                    Some (For(body, var,
                              Apply (f,
                                     Record_intro (
                                       ((StringMap.add "1" e1) ->-
                                          (StringMap.add "2" src)) StringMap.empty, None, d1), d2), fordata))
                | _ -> None
            end
          else None
    | _ -> None

let remove_trivial_extensions : RewriteSyntax.rewriter = function
  | For (List_of (Variable (v1, _), _), v2, e, _)
      when v1 = v2 -> Some e
  | _ -> None
     
(* Evaluate expressions involving only constants and pure functions at
 * compile time. *)
let fold_constant : RewriteSyntax.rewriter = 
  (* TODO: Also arithmetic, etc. *)
  let constantp = function
    | Boolean _ | Integer _ | Char _ | String _ 
    | Float _ | Nil _ -> true
    | Record_intro (fields, None, _) when StringMap.is_empty fields -> true
    | _ -> false 
  in function 
	(* Is this safe without unboxing? *)
(*    | Comparison (l, op, r, data) when constantp l && constantp r -> Some (Boolean ((assoc op ops) l r, data)) *)
    | Condition (Boolean (true, _), t, _, _)  -> Some t
    | Condition (Boolean (false, _), _, e, _) -> Some e
    | Concat (Nil _, c, _) 
    | Concat (c, Nil _, _) -> Some c
    | Concat (String (l, _), String (r, _), data) -> Some (String (l ^ r, data))
    | _ -> None 

(** Useful for printing the program at specific points of the
    optimisation pipeline.*)
let print_expression msg expr = 
  Debug.if_set show_optimisation
    (fun () -> msg ^ string_of_expression expr);
  None

(** Useful for checking a specific definition at specific points of the
    optimisation pipeline.*)
let print_definition of_name ?msg:msg expr = 
 (match expr with
    | Define (name, value, locn, _) when name = of_name 
        -> Debug.if_set show_optimisation
        (fun () -> fromOption "" msg ^ string_of_expression expr)
    | _ -> ());
  None


let rewriters env = [
  RewriteSyntax.bottomup no_project_erase;  
  RewriteSyntax.bottomup renaming;
  RewriteSyntax.bottomup unused_variables;
  if Settings.get_value reduce_recs then
    RewriteSyntax.topdown reduce_recursion
  else RewriteSyntax.never;
  RewriteSyntax.topdown simplify_regex;
  RewriteSyntax.topdown sql_aslist;
  RewriteSyntax.loop (RewriteSyntax.bottomup lift_lets);
  RewriteSyntax.topdown (sql_sort);
  RewriteSyntax.loop (RewriteSyntax.topdown sql_joins);
  RewriteSyntax.bottomup sql_selections;
(*   RewriteSyntax.bottomup unused_variables; *)
  RewriteSyntax.bottomup (sql_projections env);
(*   inference_rw env; *)
  RewriteSyntax.bottomup fold_constant;
  RewriteSyntax.topdown remove_trivial_extensions;
  RewriteSyntax.topdown (RewriteSyntax.both simplify_takedrop push_takedrop);
]

let run_optimisers : (Inferencetypes.environment * Inferencetypes.alias_environment) -> RewriteSyntax.rewriter
  = RewriteSyntax.all -<- rewriters

let optimise env expr =
  if Settings.get_value optimising then 
    match run_optimisers env expr with
        None -> Debug.if_set show_optimisation (fun () -> "Optimization had no effect"); expr
      | Some expr' -> (Debug.if_set show_optimisation
                         (fun () -> 
                            (if (Settings.get_value show_opt_verbose) then 
                               "Before optimization : " ^ 
                                 Show_stripped_expression.show (strip_data expr) 
                             else "") ^ 
			      "\nAfter optimization  : " ^
                              Show_stripped_expression.show (strip_data expr') 
                         );
	                expr')
  else expr
    
let optimise_program (env, exprs) = 
  map (optimise env) (exprs)

