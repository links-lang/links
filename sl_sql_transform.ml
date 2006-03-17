(** Provides utility function to transform database queries.
	@version 2.0
	@author Gilles Dubochet *)

open Num
open List
open Sl_kind
open Sl_sql
open Query

open Sl_utility (* for debug *)

let single_table_from_query (qry:query) : string option =
	match qry.tables with
		| [(_, rename)] -> Some rename
		| _ -> None

(** Restricts the columns to be selected from a table query by
    specifying all columns to keep. Fails if at least one
    of the column to keep was not selected in the original
    query.
    @param projs A list of the columns to keep in the query.
    @param query The query to modify.
    @return The modified query. *)
let rec project (projs:string list) (query:query) : query =
  let rec filter_selects projs result_cols = (
    match result_cols with
      | [] -> if projs <> [] then failwith "ST022"
	       else []
      | col :: result_cols ->
	  if (mem col.renamed projs) then
	    col :: (filter_selects (filter (fun proj -> proj <> col.renamed) projs) result_cols)
	  else (filter_selects (filter (fun proj -> proj <> col.renamed) projs) result_cols)
  ) in {distinct_only = query.distinct_only;
        result_cols = filter_selects projs query.result_cols;
        tables = query.tables;
	condition = query.condition;
	sortings = query.sortings}


type projection_source = {field_name : string; field_var : string;
                          etc_var : string; source_var : string}

(* Types involved in backtracing variable origins *)

(** A variable declaration stub. *)
type binding = [
| `Table_loop of (string * query)
    (** The variable is the looping variable for a loop on a
        table. Second field is the SQL name of the table. *)
| `Selected of projection_source
    (** The variable comes from a record selection operation with {ol
        {li the extracted field,} {li the variable bound to the field,}
        {li the variable bound to the row,} {li the source variable.} } *)
| `Calculated of (string * Sl_syntax.expression)
    (** The variable is calculated (that is, NOT a simple record selection 
        on a variable) *)
| `Unavailable of string
]

(** bindings
    A list of bindings. This list can be used to reconstruct a record
    selection tower. *)
type bindings = binding list

(** The required precondition values to calculate a variable.  *)
type origin = [
  | `Table_field of (string * string) (** The variable is a field of the table loop *)
  | `Table of Query.query (** The variable represents a whole row of the given table *)
  | `Earlier of (string * projection_source list) (** The variable is based on a variable defined before the bindings start, but requires the given list of selections before it can be used. *)
  | `Unavailable (** The variable cannot be used in a query *)
]

let string_of_projection_source proj = 
  "#" ^ proj.field_name ^ "=" ^ proj.field_var ^ "|" ^ proj.etc_var ^ " from " ^ proj.source_var


let string_of_binding = function
  | `Table_loop (variable, query) -> variable ^ " <- " ^ string_of_query query
  | `Selected proj -> string_of_projection_source proj
  | `Calculated (variable, expr) -> variable ^ " is calculated"
  | `Unavailable variable -> variable ^ " is unavailable"
      
let string_of_bindings bindings =
  String.concat "; " (map string_of_binding bindings)

(** Finds a binding for a particular variable name in a binding list.
	@param bindings The binding list to search.
	@param name The name of the variable to lookup.
	@return The binding or `Unbound if nothing could be found. *)
let rec variable_binding (name:string) (bindings:bindings) : [`Unbound | binding] =
  match bindings with
    | [] -> `Unbound
    | (`Table_loop (variable, _) as binding) :: bindings when variable = name -> binding
    | (`Selected s as binding) :: bindings when s.etc_var = name -> binding
    | (`Selected s as binding) :: bindings when s.field_var = name -> binding
    | (`Calculated (variable, _) as binding) :: bindings when variable = name -> binding
    | (`Unavailable variable as binding) :: bindings when variable = name -> binding
    | _ :: bindings -> variable_binding name bindings

exception ColumnNotInQuery of string

(** query_field_for_var
    Given a query and a field name (an AS name), tell me which table
    (using its AS name) and which column (using its REAL name) it
    corresponds to.
*)
let query_field_for_var query field =
  try
  find (fun x -> x <> `Unavailable)
    (map (fun col -> (if col.renamed = field then
                       `Table_field (col.table_renamed, col.name)
                      else
			`Unavailable))
       query.result_cols)
  with Not_found -> raise(ColumnNotInQuery(field))
    
(** trace_variable
    Finds the origin of a given variable according to a given binding.
    @param name The name of the variable to lookup.
    @param bindings The binding list to search.
    @return  *)
(*  TODO: Support calculated variables *)
(*  TODO: Use new variable names for origin trace *)
let rec trace_variable (name:string) (bindings:bindings) : origin =
  let rec field_from_trace trace_list = (
    match trace_list with
      | `Selected s :: _ when s.field_var = name -> `Field s.field_name
      | `Selected _ :: trace_list -> field_from_trace trace_list
      | `Table_row query :: trace_list -> `Row
      | _ -> `Unavailable
  ) in
  let rec selects_from_trace trace_list select_list = (
    match trace_list with
      | [] -> (name, select_list)
      | `Selected s :: trace_list ->
	  selects_from_trace trace_list (s :: select_list)
      | _ -> failwith "TR087"
  ) in
  let rec trace_back name trace_list = (
    match (variable_binding name bindings) with
      | `Table_loop (_, query) ->
          `Table_row query :: trace_list
      | `Selected s as frame ->
	  trace_back s.source_var (frame :: trace_list)
      | `Calculated (_, expr) -> `Unavailable :: trace_list
      | `Unavailable _ -> `Unavailable :: trace_list
      | `Unbound -> `Unbound :: trace_list
  ) in
    match (trace_back name []) with
      | `Table_row query :: trace_list ->
 	  (match (field_from_trace trace_list) with
             | `Field field -> query_field_for_var query field
	     | `Unavailable -> `Table query (* BROKEN? could it truly be unavailable? *)
	     | _ -> `Unavailable)
      | `Unbound :: trace_list -> `Earlier (selects_from_trace trace_list [])
      | `Unavailable :: trace_list -> `Unavailable
      | [] -> `Earlier (name, [])
      | `Selected _ :: trace_list -> failwith "TR105"

(** sep_assgmts
    Given an expression that consists of several assignments (lets or
    record_selections) wrapped around an inner expression, sep_assgmts
    returns a pair of the inner expression with a list that shows
    where each bound variable came from. Currently it only handles
    trivial projections of the form x.a where x is a variable term;
    all other assignments are considered "unavailable".

    @param expr The candidate syntax expression.
    @param bindings The bindings available where the expression occures.
    @return {i true} if the expression can be used, {i false} otherwise. *)
(*  TODO: Support calculated variables *)
let rec sep_assgmts (bindings:bindings) (expr:Sl_syntax.expression) : (bindings * Sl_syntax.expression) =
  match expr with
    | Sl_syntax.Let (variable, Sl_syntax.Variable (name, _), body, _) as expr ->
	failwith "TR115 (renaming declarations should have been removed by earlier optimisations)"
    | Sl_syntax.Let (variable, _, body, _) ->
	(sep_assgmts (`Unavailable variable :: bindings) body)
    | Sl_syntax.Record_selection (label, label_variable, variable, Sl_syntax.Variable (name, _), body, _) as expr ->
	(sep_assgmts (`Selected {field_name = label; field_var = label_variable; etc_var = variable; source_var = name} :: bindings) body)
    | Sl_syntax.Record_selection (label, label_variable, variable, source, body, _) ->
	(sep_assgmts bindings body)
          (* FIXME: This next case is unused. Why is it here? *)
    | Sl_syntax.Record_selection (_, label_variable, variable, _, body, _) ->
	(sep_assgmts (`Unavailable variable :: bindings) body)
    | expr -> (bindings, expr)

let rec is_free var expr = mem var (freevars expr)

(** make_sql
    Converts an expression from the constant/variable sublanguage into
    an SQL expression (with respect to the given environment,
    `bindings')
*)

(* Note: `Hard_value and `Table_value can, I believe, be collapsed. Just use an empty list for second arg, when `Table_value would occur
  `Hard_value(expr, origins)  ->  Some (expr, origins)
  `Table_value(expr)          ->  None (expr, [])
*)
let make_sql bindings expr =
  match expr with
    | Sl_syntax.Boolean (value, _) -> Some (Boolean value, [])
    | Sl_syntax.Integer (value, _) -> Some (Integer value, [])
    | Sl_syntax.Float (value, _) -> Some (Float value, [])
    | Sl_syntax.String (value, _) -> Some (Text value, [])
    | Sl_syntax.Variable (name, _) ->
	(match (trace_variable name bindings) with
	   | `Table_field (table, field) ->
	       Some (Field (table, field), [])
	   | `Earlier (rename, origin) ->
	       Some (Variable rename, origin) (*where origins come from*)
	   | `Table _
	   | `Unavailable ->
	       None)
    | _ -> None

let make_binop_sql oper left_value right_value =
  match oper with
    | "==" -> Binary_op ("=", left_value, right_value)
    | "<=" -> Binary_op ("<=", left_value, right_value)
    | "<<" -> Binary_op ("<", left_value, right_value)
    | "<>" -> Binary_op ("<>", left_value, right_value)
    | "beginswith" -> 
        Binary_op("like", left_value, Binary_op("concat", right_value, Text "%"))
    | "like" -> 
        Binary_op("like", left_value, right_value)
    | _ -> failwith "Internal error: unknown boolean operator in make_binop_sql"
        
(** condition_to_sql
    Converts a Links condition into an SQL condition; should only be
    applied to expressions that have boolean type
    
    @param expr The candidate syntax expression.
    @param bindings The bindings available where the expression occurs.
        A `Table_loop element must be present.
    @return Some(condition, projs) if it can transform the input into SQL.
        `condition' is the condition, now in Query.expression format, 
        and projs is a list of record selection descriptors. These
        projections need to be wrapped around the Query in order that 
        the free vars of the Query should be properly bound. Returns 
        None if it can't create such a result. *)
(*  TODO: Support calculated variables *)
let rec condition_to_sql (expr:Sl_syntax.expression) (bindings:bindings)
    : (Query.expression * projection_source list) option =
  let (bindings, expr) = (sep_assgmts bindings expr) in
    match expr with
      | Sl_syntax.Boolean(true, _) -> Some (Boolean true, [])
      | Sl_syntax.Boolean(false, _) -> Some (Boolean false, [])
      | Sl_syntax.Condition(c, t, e, _) ->
          (* perhaps this is just an AST-traversal with state?? *)
          let csql = condition_to_sql c bindings in
          let tsql = condition_to_sql t bindings in
          let esql = condition_to_sql e bindings in
            if for_all isSome [csql; tsql; esql] then
              let Some (csql, corigins), Some (tsql, torigins), Some (esql, eorigins) = csql, tsql, esql in
              Some (disjunction([conjunction[csql; tsql];
                                 conjunction[negation csql; esql]]),
                    corigins @ torigins @ eorigins (* is this at all right?? *))
            else None
      | Sl_syntax.Variable (var, _) -> 
          (match make_sql bindings expr with
            | Some(expr, origin) -> Some(expr, origin)
            | _ -> failwith("Internal error: unintelligible free var in query expression"))
      | Sl_syntax.Comparison (left_value, oper, right_value, _) ->
          let (left_binds, left_value) = (sep_assgmts bindings left_value) in
          let (right_binds, right_value)=(sep_assgmts bindings right_value) in
            (match (make_sql left_binds left_value,
                    make_sql right_binds right_value) with
               | (Some (lsql, lorigin), Some (rsql, rorigin)) ->
                   Some (make_binop_sql oper lsql rsql,
                         lorigin @ rorigin)
               | _ -> None)
      | _ -> None
      
let dummyData = (Sl_sugar._DUMMY_POS, `Not_typed, None)

(** select_by_origin
    Adds the origin selections before an expression.
    @param origin The origin to add to the expression.
    @param expr The expression to modify.
    @return The query with the added origin. *)
(* TODO: Support conditions on records *)
(*
let rec select_by_origin (origin:(string * string * string * string) list) (expr:Sl_syntax.expression) : Sl_syntax.expression =
  match origin with
    | (field, field_variable, variable, source) :: remaining ->
	Sl_syntax.Record_selection (field, field_variable, variable, 
                                    Sl_syntax.Variable (source, dummyData),
                                    select_by_origin remaining expr, dummyData)
    | [] -> expr
*)

let select_by_origin origin expr = 
  fold_left (fun expr origin -> 
               Sl_syntax.Record_selection(origin.field_name, origin.field_var,
                                          origin.etc_var, 
                                          Sl_syntax.Variable(origin.source_var, dummyData),
                                          expr, dummyData)
            ) expr origin

(** pos_and_neg
    A simple utility that converts (pos, neg) to "pos AND NOT neg"
    One wonders why the (pos, neg) rep'n is used in the first place.
 *)
let pos_and_neg (positives, negatives) =
  conjunction (negation (disjunction negatives) :: positives)

(* TBD: Move this to a simple utility in query.ml *)
(** Adds conditions to a query. If the {! Sl_sql_transform.selectable} method is called on every positive and negative condition provided,
    this function should not fail.
    @param positives Conditions that whould be satisfied by any element of the result.
    @param negatives Conditions that should not be satisfied by any element of the result.
    @param query The original query.
    @return The query with the added conditions.
    @raise Failure A general programming error has occured. Passing conditions that where not selectable with {! Sl_sql_transform.selectable}
    might cause such exceptions. *)
(* TODO: Support conditions on records *)
let rec select ((positives, negatives):(expression list * expression list)) (query:query) : query =
  let where = (
    match query.condition with
      | Boolean true -> pos_and_neg (positives, negatives)
      | Boolean false as where -> where 
      | _ -> Binary_op ("AND", pos_and_neg (positives, negatives), 
			query.condition)
  ) in {distinct_only = query.distinct_only;
        result_cols = query.result_cols;
        tables = query.tables;
        condition = where;
        sortings = query.sortings}
         
(** rename_uniquely
    Takes two lists of column names and produces a list of
    unique names, along with the substitutions required to make them
    distinct. Only the `right` argument needs renamings, since the
    `left` values are already distinct anyway.
*)
let append_uniquely left right : (column list * (column * string) list) =
  let right = map ( fun x -> (x, {x with renamed = Sl_sugar.col_unique_name()}) ) right in
    (left @ map snd right, map (fun (x, y) -> (x, get_renaming y)) right)
  
(** join
    Joins two queries into one, over the given condtions. If the
    {! Sl_sql_transform.selectable} method is called on every positive and
    negative condition provided, this function should not fail.
    @param positives Conditions that whould be satisfied by any element of 
       the result.
    @param negatives Conditions that should not be satisfied by any element 
       of the result.
    @param left_query The first original query.
    @param left_query The second original query.
    @return The join between queries with the added conditions.
    @raise Failure A general programming error has occured. Passing 
        conditions that where not selectable with 
    {! Sl_sql_transform.selectable} might cause such exceptions. *)
let rec join ((positives, negatives):(expression list * expression list))
    ((left, right) : query * query)
    : ((column * string) list * query) =
  if (left.distinct_only <> right.distinct_only) then failwith "TR167"
  else 
    let where = conjunction([left.condition; right.condition]
                            @ positives @ map negation negatives)
    in
      let (columns, col_renamings) = append_uniquely left.result_cols right.result_cols in
      (col_renamings,
       {distinct_only = left.distinct_only;
        result_cols   = columns;
        tables        = left.tables @ right.tables;
        condition     = where;
        sortings      = left.sortings @ right.sortings})

(** Projects the query on the empty set.
    @param query The query to nullify.
    @return The nullified query. *)
let rec null_query (query:query) : query =
  {query with result_cols = []}
