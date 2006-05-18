(** Optimisation passes: rewrites of the AST. *)

open List

open Utility
open Debug
open Rewrite
open Syntax
open Sql_transform

(* This appears to be ignored *)
let optimising = Settings.add_bool true "optimising"

module RewriteSyntax = 
  Rewrite
    (SimpleRewrite
       (struct
          type t = Syntax.expression
          type rewriter = t -> t option
          let process_children = Syntax.perhaps_process_children
        end))
    
let gensym =
  let counter = ref 0 in
    function str ->
      begin
        incr counter;
        str ^ "_g" ^ string_of_int !counter
      end

(* uniquify_expression

   Give unique names to all local bindings.  Local names should be
   distinct from each other and from toplevel names

   After this, we can be much less careful about scope.
*)
let uniquify_expression : RewriteSyntax.rewriter = 
  (* Rename a variable, entirely ignoring any intervening bindings *)
  let rename_var orig repl e = 
    let rename_one = function
      | Variable (v, data) when v = orig -> Some (Variable (repl, data))
      | _ -> None in
      fromOption e (RewriteSyntax.topdown rename_one e) in
  let rewrite_node = function
    | Abstr (v, b, data) -> 
        let name = gensym v in
          Some (Abstr (name, rename_var v name b, data))
    | Let (v, e, b, data) -> 
        let name = gensym v in
          Some (Let (name, e, rename_var v name b, data))
    | Rec (vs, b, data) -> 
        let bindings = List.map (fun (name, _) -> (name, gensym name)) vs in
        let rename = List.fold_right (uncurry rename_var) bindings in
          Some(Rec(List.map (fun (n, v) -> (List.assoc n bindings, rename v)) vs,
                     rename b, data))
    | Record_selection (lab, lvar, var, value, body, data) ->
        let lvar' = gensym lvar
        and var'  = gensym var in
          Some(Record_selection(lab, lvar', var', value, 
                                rename_var var var' (rename_var lvar lvar' body),
                                data))
    | For (b, v, src, data) -> 
        let name = gensym v in
          Some (For(rename_var v name b, name, src, data))
    | Variant_selection (value, clab, cvar, cbody, var, body, data) ->
        let cvar' = gensym cvar
        and var'  = gensym var in
          Some (Variant_selection (value, clab, 
                                   cvar', rename_var cvar cvar' cbody, 
                                   var',  rename_var var var' body,
                                   data))
    | Escape (v, b, data) -> 
        let name = gensym v in
          Some (Escape (name, rename_var v name b, data))
    | _ -> None
  (* Note that this will only work bottomup, not topdown, since
     we need to replace bindings from the inside out *)
  in RewriteSyntax.bottomup rewrite_node

(* pure

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
  let pure default = function 
    | Apply _    -> false
    | Table _    -> false
    | Database _ -> false
    | Escape _   -> false
    | e       -> default e
  and combiner l = fold_right (&&) l true in
    reduce_expression pure (combiner -<- snd)

(** inference_rw
    The type-inference function, in the form of a rewriter
*)
(*
let inference_rw env : RewriteSyntax.rewriter = fun input -> 
  let output = snd (Inference.type_expression env (erase input)) in
    if input = output then None
    else Some output
*)
let rename_var var repl : RewriteSyntax.rewriter = 
  (* Blindly replace occurrences of a variable with an expression,
     ignoring bindings *)
  let rewrite = function
    | Variable (v, d) when v = var -> Some (Variable (repl, d))
    | Table (db, s, q, data) -> Some (Table (db, s, Query.query_replace_var var (Query.Variable repl)  q, data)) 
    | _                            -> None
  in RewriteSyntax.topdown rewrite

let renaming : RewriteSyntax.rewriter = 
  let bound_in var = 
    (* Is a particular name bound inside an expression? *)
    let binds default = function
      | Let (v, _, _, _)
      | Abstr (v, _, _)
      | Define (v, _, _, _)
      | Escape (v, _, _) when v = var -> true
      | Record_selection (_, v1, v2, _, _, _)
      | Variant_selection (_, _, v1, _, v2, _, _) when var = v1 || var = v2 -> true
      | Rec (bindings, _, _) when mem var (map fst bindings) -> true
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
      -> Some (fromOption body (rename_var y x body))
  | _ -> None

(** Remove all let bindings where the name is not used in the body.

    FIXME: also letrec ("transitively") and record selection
    operators.
*)

let unused_variables : RewriteSyntax.rewriter = function
    (* This'd be quite a bit more efficient bottom-up, passing the
       list of free variables outwards rather than searching the body
       every time we find a let. *)
  | Let (var, expr, body, _) when pure expr 
                               && not (mem var (freevars body)) -> Some body
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
    : (Query.expression list * Query.expression list * expression * projection_source list) =
  match expr with
    | Let (_, Variable (_, _), _, _) ->
        failwith "TR115 (renaming declarations should have been removed by earlier optimisations)"
    | Let (variable, value, body, data) ->
        let (positive, negative, body, origin) = extract_tests (`Unavailable variable :: bindings) body in
          (positive, negative, Let (variable, value, body, data), origin)

    | Record_selection (label, label_variable, variable, Syntax.Variable (name, vdata), body, data) ->
        let trace_data = `Selected{field_name=label; field_var=label_variable;
                                   etc_var = variable; source_var = name} in
        let (positive, negative, body, origin) = extract_tests (trace_data :: bindings) body in
          (positive, negative,
	   Record_selection (label, label_variable, variable, 
			     Syntax.Variable (name, vdata), body, data), 
	   origin)
    | Record_selection (label, label_variable, variable, value, body, data) ->
        let positive, negative, body, origin = extract_tests (`Unavailable variable :: bindings) body in
          (positive, negative, Record_selection (label, label_variable, variable, value, body, data), origin)

    | Condition (condition, t, ((Nil _) as e), data)  ->
        let positive, negative, t, origin = extract_tests bindings t in
          (match condition_to_sql condition bindings with
             | Some (sql_condition, new_origin) ->
                 (sql_condition :: positive, negative, t, new_origin @ origin)
             | None ->
                 (positive, negative, Condition(condition, t, e, data), origin))
    | Condition (condition, ((Nil _) as t), e, data) ->
        let positive, negative, e, origin = extract_tests bindings e in
          (match condition_to_sql condition bindings with
             | Some (sql_condition, new_origin) ->
                 (positive, sql_condition :: negative, e, new_origin @ origin)
             | None ->
                 (positive, negative, Condition(condition, t, e, data), origin))
    | For (expr, variable, value, data) ->
        let positive, negative, expr, origin = extract_tests (`Calculated (variable, expr) :: bindings) expr in
          (positive, negative, For (expr, variable, value, data), origin)
    | _ -> ([], [], expr, [])

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
let sql_projections (env:Kind.environment) : RewriteSyntax.rewriter =
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
                     (match snd (Inference.type_expression env (erase apply)) with
                        | Abstr (_, _, (_, kind, _)) -> kind
                        | _ -> failwith "OP442")
                 | _ -> failwith "OP437"
             ) with
               | `Function (`Record (field_env, _), _) ->
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
        | Record_selection_empty (Variable (name, _), _, _) when name = var -> Fields []
        | other -> default other
    in reduce_expression (visitor var) (merge_needed -<- snd) in
  let rewrite = function
    | For (body, variable, Table (db, s, query, tdata), data) ->
        (match needed_fields variable body with
           | Fields needs ->
               Some (For (body, variable,
                                           Table (db, s, 
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
    constant and a variable or between two variables (comming from the
    database). The optimisation will stop looking for conditions in
    the body expression if it encouters any other node than a
    condition, a {i let}, a record selection or a collection
    extension. *)
let sql_selections : RewriteSyntax.rewriter = function 
  | For (expr, variable, Table (db, s, query, tdata), data) ->
      let positive, negative, expr, origin = extract_tests [`Table_loop (variable, query)] expr in
      let table = Table (db, s, select (positive, negative) query, tdata) in
        Some (select_by_origin origin (For (expr, variable, table, data)))
  | _ -> None

let rec substitute_projections new_src renamings expr bindings =
  let subst_projection (from, to') visit_children expr =
    let expr = visit_children expr in
      match expr with
        | Record_selection(label, label_var, etc_var, Variable(src, d),
			   body, data) 
            when from.Query.renamed = label ->
            (match (trace_variable src bindings) with
	       | `Table query 
		   when mem from.Query.table_renamed (map snd query.Query.tables) ->
                   Record_selection (to', label_var, etc_var, Variable(new_src, d),
                                     visit_children body, data)
               | `Table_field (table_as, _)
                   when from.Query.table_renamed = table_as ->
                   (* NOTE: I think this case never occurs *)
                   Record_selection(to', label_var, etc_var, Variable(new_src, d),
                                    visit_children body, data)
               | _ -> 
                   Record_selection (label, label_var, etc_var, Variable(src, d), body, data))
        | x -> x
  in
    fold_right (fun ren expr ->
                  simple_visit (subst_projection ren) expr
               ) renamings expr

let read_proj = function
    Record_selection(field, field_var, etc_var, record, 
                     Variable(result_var, data1), data2) ->
      Some(record, field)
  | _ -> None

let rec sql_sort = function
  | SortBy(Table(db, dummy, query, data1), 
           Abstr(loopVar, sortByExpr, data2), data3) ->
      (match read_proj sortByExpr with
           Some (Variable(sortByRecVar, data3), sortByFld)
             when sortByRecVar = loopVar
               -> Some(Table(db, dummy, Query.add_sorting query 
                               (`Asc(Query.owning_table sortByFld query, sortByFld)), data1))
         | _ -> None)
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
let rec check_join (loop_var:string) (ref_db:string) (bindings:bindings) (expr:expression)
    =
  let bindings, expr = sep_assgmts bindings expr in
    match expr with
      | Condition (condition, t, ((Nil _) as e), data)  ->
          (match check_join loop_var ref_db bindings t with
	     | Some (positive, negative, query, projs, var, t) ->
                 Some (positive, negative, query, projs, var, Condition (condition, t, e, data))
	     | None -> None)
      | Condition (condition, ((Nil _) as t), e, data) ->
          (match check_join loop_var ref_db bindings e with
	     | Some (positive, negative, query, projs, var, e) ->
                 Some (positive, negative, query, projs, var, Condition (condition, t, e, data))
	     | None -> None)
      | For (expr, variable, Table (_, _, query, _), _) ->
          (* TODO: Test whether both tables come from the same database. *)
          (match extract_tests (`Table_loop (variable, query) :: bindings) expr with
               (*  ([], [], _, _) -> None *)
	     | (positives, negatives, expr, origin) ->
                   Some (positives, negatives, query, 
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
    | For (body, outer_var, (Table (db, s, query, tdata)), data) ->
        let bindings = [`Table_loop (outer_var, query)] in
        (match check_join outer_var "dummy" bindings body with
           | Some (positives, negatives, inner_query, origins, inner_var, body) ->
               let renamings, query = join (positives, negatives) (query, inner_query) in
                 
               (* Replace anything of the form inner_var.field with 
                  outer_var.renamed_field with renamings as given by 
                  the join operator *)
               let body = substitute_projections outer_var renamings body
                 [`Table_loop(inner_var, inner_query)] 
               in
               let expr = For(body,
                              outer_var, 
                              Table (db, s, query, tdata), 
                              data) in
(*                (\* finally, wrap the whole expression in the  *)
(*                   projections returned from check_join; *)
(*                   TBD: Does this need to go somewhere? Inside the loop? *\) *)
(*                let expr = select_by_origin origins expr in *)
                 
                 Some expr

           | None -> 
               (* check_join returned None, so perhaps we only have one loop. 
                  still, try to push the conditions down into SQL. 
                  HACK ALERT; this shouldn't be a special case. the 1-join
                  should be handled the same as 2-join, 3-join, etc. *)
               let (pos, neg, body, proj_srcs) = extract_tests bindings body in
                 (* This positive/negatives business is retarded, I think *)
               debug("extract_tests returned " ^ String.concat " AND " (map Sql.string_of_expression pos) ^ " AND NOT " ^ String.concat " AND " (map Sql.string_of_expression neg));
                 if (pos <> [] || neg <> []) then
                   let query = {query with Query.condition = pos_and_neg (query.Query.condition::pos, neg) } in
                     Some (For(body, outer_var, 
                               Table (db, s, query, tdata), data))
                 else None
        ) (* match check_join .... with *)
    | _ -> None

(*
  RENAMING THE VARIABLE:
  for x in
    for y in
       (x.a, y.b)


  for x in 
    (x.a, x.b)

  RENAMING THE COLUMNS:

  for x in
    for y in
       (x.a, y.a)


  for x in [AS col_37]
    (x.a, x.col_37)

  for x in
    for y in
       if (x.a == y.a) in
          (x.b, y)


   y ~>  (a = x.col_37, b = y.col_38)

  To fix in sql_joins:

    1. rename duplicate columns
    2. rename deleted variable 
    3. handle unprojected record variables
    4. join even without condition?

 *)
	  


(* take/drop optimization.  Push calls to take and drop that surround
   queries into the query.

   [N.B. these rewrite rules play fast and loose with the `data'
    component of expression nodes.  Don't assume anything about the
    data after these have run.]
*)

(*
  offset n limit m corresponds to take m (drop n ...) 
  (but not to drop n (take ...))
*)

(*
   take e1 (drop e2 e3) ~>  {x = e2; y = e1; take y (drop x e3)}
      (Not performed if both e1 and e2 are variables or integer literals)

   take e1 e2 ~> {x = e1; take x e2}
   drop e1 e2 ~> {x = e1; drop x e2}
      (Not performed if e1 is a variable or integer literal)
*)
let simplify_takedrop : RewriteSyntax.rewriter = function
  | Apply (Apply (Variable ("take", _), (Variable _|Integer _), _), 
           Apply (Apply (Variable ("drop", _), ((Variable _| Integer _)), _), _, _), _) -> None
  | Apply (Apply (Variable ("take", d1), e1, d2), 
           Apply (Apply (Variable ("drop", d3), e2, d4), e3, d5), d6) ->
      let x = gensym "" 
      and y = gensym "" in
        Some (Let (x, e2, 
                   Let (y, e1,
                        Apply (Apply (Variable ("take", d1), Variable (y, d1), d2), 
                               Apply (Apply (Variable ("drop", d3), Variable (x, d1), 
                                             d4), e3, d5),
                               d6), d1), d1))
  | Apply (Apply (Variable (("take"|"drop"), _), (Variable _ |Integer _), _), _ , _) -> None
  | Apply (Apply (Variable ("take"|"drop" as f, d1), e1, d2), e2, d3) ->
      let var = gensym "" in
        Some (Let (var, e1, 
                   Apply (Apply (Variable (f, d1), 
                                 Variable (var, d1), d2), e2, d3), d1))
  | _ -> None

(*
  take e1 (drop e2 (Table (... q ...))) ~> Table (... {q with offset = e2; limit = e1} ...)
     where e1 and e2 are variables or integer literals

  take e1 (Table (... q ...)) ~> Table (... {q with limit  = e1} ...)
  drop e1 (Table (... q ...)) ~> Table (... {q with offset = e1} ...)
     where e1 is a variable or integer literal
*)

let push_takedrop : RewriteSyntax.rewriter = 
  let queryize = function
    | Variable (v, _) -> Query.Variable v
    | Integer  (n, _) -> Query.Integer n
    | _ -> failwith "Internal error during take optimization" in 
    function
      | Apply (Apply (Variable ("take", _), (Variable _|Integer _ as e1), _), 
               Apply (Apply (Variable ("drop", _), (Variable _|Integer _ as e2), _), 
                      Table (e, s, q, d5), _), _) ->
          Some (Table (e, s, {q with
                                Query.max_rows = Some (queryize e1);
                                Query.offset   = queryize e2}, d5))
      | Apply (Apply (Variable ("take", _), (Variable _|Integer _ as n), _),
               Table (e, s, q, d4), _) -> 
	  Some (Table (e, s, {q with Query.max_rows = Some (queryize n)}, d4))
      | Apply (Apply (Variable ("drop", _), (Variable _|Integer _ as n), _),
               Table (e, s, q, d4), _) -> 
	  Some (Table (e, s, {q with Query.offset = queryize n}, d4))
      | _ -> None


let trivial_extensions : RewriteSyntax.rewriter = function
  | For (List_of (Variable (v1, _), _), v2, e, _)
      when v1 = v2 -> Some e
  | _ -> None
     
let ops = ["==", (=);
           "<>", (<>);
           "<=", (<=);
           "<",  (<)]

(* Evaluate expressions involving only constants and pure functions at
 * compile time. *)
let fold_constant : RewriteSyntax.rewriter = 
  (* TODO: Also arithmetic, etc. *)
  let constantp = function
    | Boolean _ | Integer _ | Char _ | String _ 
    | Float _ | Record_empty _ | Nil _ -> true
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

let rewriters env = [
  RewriteSyntax.bottomup renaming;
  RewriteSyntax.bottomup unused_variables;
  RewriteSyntax.loop (RewriteSyntax.topdown sql_joins);
  RewriteSyntax.bottomup sql_selections;
  RewriteSyntax.bottomup unused_variables;
  RewriteSyntax.bottomup (sql_projections env);
  RewriteSyntax.topdown (sql_sort);
(*   inference_rw env; *)
  RewriteSyntax.bottomup fold_constant;
  RewriteSyntax.topdown trivial_extensions;
  RewriteSyntax.topdown (RewriteSyntax.both simplify_takedrop push_takedrop);
]

let run_optimisers : Kind.environment -> RewriteSyntax.rewriter
  = RewriteSyntax.all -<- rewriters

let optimise env expr =
  match run_optimisers env expr with
      None -> debug ("Optimization had no effect"); expr
    | Some expr' -> (debug("Before optimization : " ^ show_expression expr ^ 
			     "\nAfter optimization  : " ^ show_expression expr');
		     expr')

(* Not really an optimisation.  This /must/ be run, or the program
   semantics will be completely wrong.
   FIXME: Should we separate *necessary* transformations from optimizations? 
     SQL generation might count as a necessary transformation, too.
*)
let inline_tables expressions = 
  let insert_tables map : RewriteSyntax.rewriter = function
    | Variable (v, _) when mem_assoc v map -> Some (assoc v map)
    | _ -> None in
  let tabledefs, sanstables = 
    either_partition
      (function
         | Define (name, (Table _ as t), _, _) -> Left (name, t)
         | e -> Right e)
      expressions
  in
    map (valOf -<- (RewriteSyntax.either
                      (RewriteSyntax.bottomup (insert_tables tabledefs))
                      RewriteSyntax.always))
      sanstables

let optimise_program (env, exprs) = 
  map (optimise env) (inline_tables exprs)

(* Testing stuff from here on down *)

let parse = Parse.parse_string
let parse_and_type env = List.hd -<- snd -<- Inference.type_program env -<- parse

let strip = Syntax.redecorate (fun _ -> ());;

let test () =
  assert(opt_map strip (RewriteSyntax.bottomup fold_constant (parse_and_type [] "if (true) 3 else 4"))
	 = Some (Integer (Num.Int 3, ())))
  ;
  assert(opt_map 
	   strip (RewriteSyntax.bottomup renaming (parse_and_type [] "{x = 3; y = x; y}"))
	 = Some (Let
		   ("x",
		    Integer (Num.Int 3, ()),
		    Variable ("x", ()), ())))
  ;
  (* tests a bug where all subtrees of a node were reversed in order. *)
  assert (opt_map
	    strip (RewriteSyntax.bottomup (fun x -> Some x) (parse_and_type Library.type_env "2 + 3"))
	    = Some (Syntax.Apply
		      (Syntax.Apply
			 (Syntax.Variable ("+", ()), Syntax.Integer (Num.Int 2, ()), ()),
		       Syntax.Integer (Num.Int 3, ()), ())))
  ;
  assert (opt_map
	    strip (RewriteSyntax.bottomup sql_joins (parse_and_type Library.type_env "{db = database \"Rubbish\"; for x <- (Table \"foo\" with {a : Int, b : Int} from db) in for y <- (Table \"frump\" with {c : Int, d : Int} from db) in if (x.a == y.c) [(x.b, y.d)] else []}"))
	  =
      Some
	(Let
	   ("db", Database (String ("Rubbish", ()), ()),
	    For
	      (List_of
		 (Record_extension
		    ("1",
		     Record_selection
		       ("b", "g27", "g28", Variable ("x", ()),
			Variable ("g27", ()), ()),
		     Record_extension
		       ("2",
			Record_selection
			  ("d", "g29", "g30", Variable ("x", ()),
			   Variable ("g29", ()), ()),
			Record_empty (), ()),
		     ()),
		  ()),
	       "x",
	       Table
		 (Variable ("db", ()), "table \"foo\" with  {a:Int,b:Int}  ",
		  failwith "This is broken.  Optimization tests should use pattern matching, not structural equality.",
		  ()),
	       ()),
	    ())))
  ;
  assert(opt_map strip (RewriteSyntax.bottomup sql_joins (parse_and_type Library.type_env  "{db = database \"Rubbish\"; for x <- (Table \"foo\" with {a : Int, b : Int} from db) in  for y <- (Table \"frump\" with {c : Int, d : Int} from db) in    for z <- (Table \"frozz\" with {e : Int} from db) in if (x.b == z.e && x.a == y.c)         [(x.b, y.d)] else []}"))
         =
      Some
        (Syntax.Let
           ("db", Syntax.Database (Syntax.String ("Rubbish", ()), ()),
            Syntax.For
              (Syntax.Condition
                 (Syntax.Condition
                    (Syntax.Comparison
                       (Syntax.Record_selection
                          ("b", "g97", "g98", Syntax.Variable ("x", ()),
                           Syntax.Variable ("g97", ()), ()),
                        "==",
                        Syntax.Record_selection
                          ("e", "g99", "g100", Syntax.Variable ("x", ()),
                           Syntax.Variable ("g99", ()), ()),
                        ()),
                     Syntax.Comparison
                       (Syntax.Record_selection
                          ("a", "g101", "g102", Syntax.Variable ("x", ()),
                           Syntax.Variable ("g101", ()), ()),
                        "==",
                        Syntax.Record_selection
                          ("c", "g103", "g104", Syntax.Variable ("x", ()),
                           Syntax.Variable ("g103", ()), ()),
                        ()),
                     Syntax.Boolean (false, ()), ()),
                  Syntax.List_of
                    (Syntax.Record_extension
                       ("1",
                        Syntax.Record_selection
                          ("b", "g105", "g106", Syntax.Variable ("x", ()),
                           Syntax.Variable ("g105", ()), ()),
                        Syntax.Record_extension
                          ("2",
                           Syntax.Record_selection
                             ("d", "g107", "g108", Syntax.Variable ("x", ()),
                              Syntax.Variable ("g107", ()), ()),
                           Syntax.Record_empty (), ()),
                        ()),
                     ()),
                  Syntax.Nil (), ()),
               "x",
               Syntax.Table
                 (Syntax.Variable ("db", ()), "table \"foo\" with  {a:Int,b:Int}  ",
                  failwith "This is broken.  Optimization tests should use pattern matching, not structural equality.",
                  ()),
               ()),
            ())))
  ;

