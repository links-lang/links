open Str
open Num
open List

open Utility
open Result
open Query
open Syntax

(** {0 Environment handling} *)

(** bind env var value 
    Extends `env' with a binding of `var' to `value'.
*)
let bind env var value = (var, value) :: env

(** trim_env env
    remove shadowed bindings from `env'
*)
let trim_env = 
  let rec trim names (env:(string * result) list) 
      = match env with
        | [] -> []
        | (k, v) :: rest -> 
            if mem k names then trim names rest
            else (k, v) :: (trim (k :: names) rest) in
    trim []
      
(** Remove toplevel bindings from an environment *)
let remove_toplevel_bindings toplevel env = 
  filter (fun pair -> mem pair toplevel) env

exception RuntimeUndefVar of string

let lookup globals locals name = 
  try 
    (match Utility.lookup name locals with
      | Some v -> v
      | None -> match Utility.lookup name globals with
          | Some v -> v
          | None -> Library.primitive_stub name)
  with Not_found -> 
    raise(RuntimeUndefVar name) (* ("Internal error: variable \"" ^ name ^ "\" not in environment")*)

let bind_rec locals defs =
  (* create bindings for these functions, with no local variables for now *)
  let make_placeholder = (fun env (variable, value) ->
                            (match value with
                               | Syntax.Abstr (var, body, _) ->
                                   bind env variable (`Function (var, locals, () (*globals*), body))
                               | _ -> raise (Runtime_error "TF146"))) in
  let rec_env = trim_env (fold_left make_placeholder [] defs) in
    (* fill in the local variables *)
  let fill_placeholder value =
    match value with
      | `Function (var, fnlocals, _, body) ->
          `Function (var, fnlocals @ (retain (freevars body) rec_env), (), body)
      | _ -> value
  in
    trim_env ((alistmap fill_placeholder rec_env) @ locals)

let rec crack_row : (string -> ((string * result) list) -> (result * (string * result) list)) = fun ref_label -> function
        | [] -> raise (Runtime_error("Internal error: no field '" ^ ref_label ^ "' in record"))
        | (label, result) :: fields when label = ref_label ->
            (result, fields)
        | field :: fields ->
            let selected, remaining = crack_row ref_label fields in
              (selected, field :: remaining)

let untuple : result -> result list = 
  let rec aux n output = function
    | [] -> List.rev output
    | fields ->
        match partition (fst ->- (=)(string_of_int n)) fields with
          | [_,r], rest -> aux (n+1) (r::output) rest
          | _ -> assert false
  in function
    | `Record fields -> aux 1 [] fields
    | _ -> assert false
    

(** [normalise_query] substitutes values for the variables in a query,
    and performs interpolation in LIKE expressions.  *)
let rec normalise_query (globals:environment) (env:environment) (db:database) (qry:query) : query =
  let rec normalise_like_expression (l : Query.like_expr): Query.expression = 
      Text (Sql_transform.like_as_string (env @ globals) l)
  in
  let rec normalise_expression : Query.expression -> Query.expression = function
      | Query.Variable name ->
          (try
             match lookup globals env name with
               | `Bool value -> Query.Boolean value
               | `Int value -> Query.Integer value
               | `Float value -> Query.Float value
               | `List (`Char _::_) as c  
                 -> Query.Text (db # escape_string (charlist_as_string c))
               | `List ([]) -> Query.Text ""
               | r -> failwith("Internal error: variable " ^ name ^ 
                                 " in query "^ Sql.string_of_query qry ^ 
                                 " had unexpected type at runtime: " ^ 
                                 string_of_result r)
           with Not_found-> failwith("Internal error: undefined query variable '"
                                     ^ name ^ "'"))
      | Query.Binary_op (symbol, left, right) ->
          Binary_op (symbol, normalise_expression left, normalise_expression right)
      | Query.Unary_op (symbol, expr) ->
          Unary_op (symbol, normalise_expression expr)
      | Query.LikeExpr (like_expr) -> normalise_like_expression like_expr
      | Query.Query qry ->
          Query {qry with condition = normalise_expression qry.condition}
      | expr -> expr
  in
  let normalise_tables  = map (function 
                                 | `TableName t, alias -> `TableName t, alias
                                 | `TableVariable var, alias ->
                                     (match lookup globals env var with
                                         `Table(_, tableName, _) -> `TableName tableName, alias
                                       | _ -> failwith "Internal Error: table source was not a table!")
                              ) 
  in {qry with
        tables = normalise_tables qry.tables;
        condition = normalise_expression qry.condition;
        offset = normalise_expression qry.offset;
        max_rows = opt_map normalise_expression qry.max_rows}

(** [get_row_field_type field row]: what type has [field] in [row]? 
    TBD: Factor this out.
*)
exception NotFound of string

let row_field_type field : Types.row -> Types.datatype = 
  fun (fields, _) ->
    match StringMap.find field fields with
      | `Present t -> t
      | `Absent -> raise (NotFound field)

let query_result_types (query : query) (table_defs : (string * Types.row) list)
    : (string * Types.datatype) list =
  try 
    let col_type table_alias col_name =
      row_field_type col_name (assoc table_alias table_defs) 
    in
      concat_map (function
                    | Left col -> [col.renamed, col_type col.table_renamed col.name]
                    | Right _ -> []) query.Query.result_cols
  with NotFound field -> failwith ("Field " ^ field ^ " from " ^ 
                                     Sql.string_of_query query ^
                                     " was not found in tables " ^ 
                                     mapstrcat "," fst table_defs ^ ".")

(** 0 Web-related stuff *)
let has_client_context = ref false

let serialize_call_to_client (continuation, name, arg) = 
  Json.jsonize_call continuation name arg

let program_source = ref(Program([], Syntax.unit_expression no_expr_data))

let client_call_impl name cont (args:Result.result list) =
  let callPkg = Utility.base64encode(serialize_call_to_client(cont, name, args)) 
  in
    if (not !has_client_context) then 
      begin
        let start_script = "_invokeClientCall(_start, JSON.parseB64Safe(\"" ^ callPkg ^ "\"))" in
          Library.print_http_response ["Content-type", "text/html"]
            (Js.make_boiler_page ~onload:start_script
               (Js.generate_program_defs (!program_source) [name]))
          ; exit 0
      end
    else begin
      Library.print_http_response ["Content-type", "text/plain"] callPkg;
      exit 0
    end

exception TopLevel of (Result.environment * Result.result)

(* could bundle these together with globals to get a global
   'interpreter state' that we'd then thread through the whole
   interpreter, making it re-entrant. *)
let process_steps = ref 0
let switch_granularity = 5

let rec switch_context globals = 
  if not (Queue.is_empty Library.suspended_processes) then 
    let (cont, value), pid = Queue.pop Library.suspended_processes in
      Library.current_pid := pid;
      apply_cont globals cont value
  else exit 0

and scheduler globals state stepf = 
  incr process_steps;
  if (!process_steps mod switch_granularity == 0) then 
    begin
      process_steps := 0;
      Queue.push (state, !Library.current_pid) Library.suspended_processes;
      switch_context globals
    end
  else
    stepf()
and apply_cont (globals : environment) : continuation -> result -> result = 
  fun cont value ->
    let stepf() = 
      match cont with
        | [] -> (if !Library.current_pid == Library.main_process_pid then
                   raise (TopLevel(globals, value))
	         else switch_context globals)
        | (frame::cont) -> match frame with
	    | (Definition(env, name)) -> 
	        apply_cont (bind env name value) cont value
            | Recv (locals) ->
                (* If there are any messages, take the first one and
                   apply the continuation to it.  Otherwise, suspend
                   the continuation (in the blocked_processes table)
                   and let the scheduler choose a different thread.
                *)
                let mqueue = Hashtbl.find Library.messages !Library.current_pid in
                  if not (Queue.is_empty mqueue) then
                    apply_cont globals cont (Queue.pop mqueue)
                  else 
                    begin
                      Hashtbl.add Library.blocked_processes
                        !Library.current_pid
                        ((Recv locals::cont, value), !Library.current_pid);
                      switch_context globals
                    end
            | FuncArg([], _) -> assert false
            | FuncArg(param::params, locals) ->
	        (* Just evaluate the first parameter; "value" is in
	           fact a function value which will later be applied
	        *)
                interpret globals locals param (FuncApply (locals, value, params, [])::cont)
            | (FuncApplyFlipped(locals, arg)) ->
                apply_cont globals (FuncApply(locals, value, [], []) :: cont) arg
            | ThunkApply locals ->
                begin match value with
                  | `Function ([], fnlocals, (), body) -> 
                      interpret globals (trim_env (fnlocals @ locals)) body cont
                  | `PrimitiveFunction name ->
                      apply_cont globals cont (Library.apply_pfun name [])
                  | `ClientFunction name ->
                      client_call_impl name cont []
                  | `Abs f ->
                      apply_cont globals (FuncApply (locals, f, [], [])::cont) (`Record [])
                  | _ -> raise (Runtime_error ("error applying zero-argument function "^
                                                 string_of_result value))
                end
            | FuncApply(locals, func, unevaluated_args, evaluated_args) ->  
                begin match func, unevaluated_args with
                  | _, arg::args -> 
                      interpret globals locals arg
                        (FuncApply (locals, func, args, value::evaluated_args)::cont)
                  | `Function (vars, fnlocals, (), body), [] ->
                      (* FIXME: functional abstractions no longer capture
                         global variables; remove the fnglobals element here. *)
                      
		      (* Interpret the body in the following
		         environments: locals are augmented with
		         function locals and function binding (binding
		         takes precedence) toplevel environment in
		         which the function was defined takes
		         precedence over the real current globals. Is
		         there a semanticist in the house? *)
                      
                      let locals = trim_env (fnlocals @ locals) in
                      let locals = fold_left2 bind locals vars (List.rev (value::evaluated_args))
                      in
                        interpret globals locals body cont

                  | `PrimitiveFunction name, [] ->
                      apply_cont globals cont (Library.apply_pfun name  (List.rev (value::evaluated_args)))

                  | `ClientFunction name, [] ->
                     client_call_impl name cont (List.rev (value::evaluated_args))
	          | `Continuation (cont), [] ->
                      apply_cont globals cont value
                  | `Abs f, [] -> 
                      apply_cont globals (FuncApply (locals, f, [], [])::cont) 
                        (`Record
                           (snd 
                              (List.fold_right
                                 (fun field (n,tuple) ->
                                    (n+1,
                                     (string_of_int n, field)::tuple))
                                 (value::evaluated_args)
                                 (1, []))))
                  | _ -> raise (Runtime_error ("Applied non-function value: "^
                                                 string_of_result func))
                end
            | (LetCont(locals, variable, body)) ->
	        interpret globals (bind locals variable value) body cont
            | (BranchCont(locals, true_branch, false_branch)) ->
	        (match value with
                   | `Bool true  -> 
	               interpret globals locals true_branch cont
                   | `Bool false -> 
	               interpret globals locals false_branch cont
                   | _ -> raise (Runtime_error("Attempt to test a non-boolean value: "
					         ^ string_of_result value)))
            | (BinopRight(locals, op, rhsExpr)) ->
	        interpret globals locals rhsExpr
                  (BinopApply(locals, op, value) :: cont)
                  (* FIXME: locals aren't needed here *)
            | (BinopApply(locals, op, lhsVal)) ->
	        let result = 
                  begin match op with
                    | `Equal -> bool (Library.equal lhsVal value)
                    | `NotEq -> bool (not (Library.equal lhsVal value))
                    | `LessEq -> bool (Library.less_or_equal lhsVal value)
                    | `Less -> bool (Library.less lhsVal value)
	            | `Union -> 
                        begin match lhsVal, value with
	                  | `List (l), `List (r) -> `List (l @ r)
	                  | _ -> raise(Runtime_error
                                         ("Type error: Concatenation of non-list values: "
					  ^ string_of_result lhsVal ^ " and "
					  ^ string_of_result value))
                        end
	            | `RecExt label -> 
		        begin match lhsVal with
		          | `Record fields -> 
		              `Record ((label, value) :: fields)
		          | _ -> assert false
                        end
	             | `MkTableHandle row ->
			 begin match lhsVal with
			   | `Database (db, params) ->
			       apply_cont globals cont 
                                 (`Table((db, params), charlist_as_string value, row))
			   | _ -> failwith("Runtime type error: argument to table was not a database.")
                         end
                     | `App -> 
                         begin match untuple value with
                           | [] -> 
                               apply_cont globals (ThunkApply locals ::cont) lhsVal
                           | _::_ as v -> 
                               let firsts, last = unsnoc v in
                                 apply_cont globals (FuncApply (locals, lhsVal, [], firsts)::cont) last
                         end
                  end
	        in
	          apply_cont globals cont result
            | UnopApply (locals, op) ->
                (match op with
                     MkColl -> apply_cont globals cont (`List [(value)])
	           | MkVariant(label) -> 
	               apply_cont globals cont (`Variant (label, value))
                   | Result.Abs ->
                       apply_cont globals cont (`Abs value)
                   | VrntSelect(case_label, case_variable, case_body, variable, body) ->
	               (match value with
                          | `Variant (label, value) when label = case_label ->
                              (interpret globals (bind locals case_variable value) case_body cont)
                          | `Variant (_) as value ->
		              (interpret globals (bind locals (valOf variable) value)
		                 (valOf body) cont)
                          | _ -> raise (Runtime_error "TF181"))
	           | MkDatabase ->
                       let result = (let driver = charlist_as_string (links_project "driver" value)
				     and name = charlist_as_string (links_project "name" value)
				     and args = charlist_as_string (links_project "args" value) in
				     let params =
				       (if args = "" then name
					else name ^ ":" ^ args)
				     in
                                       `Database (db_connect driver params)) in
	               apply_cont globals cont result
                   | Result.Erase label ->
                       apply_cont globals cont (`Record (snd (crack_row label (recfields value))))
                   | Result.Project label ->
                       apply_cont globals cont (fst (crack_row label (recfields value)))
                   | QueryOp(query, table_aliases) ->
                       let result = 
                         match value with
                           | `List(tbls) ->
                               let (dbs, table_defs) = 
                                 split(map(function `Table((db, params), _table_name, row) 
                                               -> (db, row)
                                             | _ -> failwith "THX1138") 
                                         tbls) in

                                 if(not (all_equiv (=) dbs)) then
                                   failwith ("Cannot join across different databases");

                                 let table_defs = combine table_aliases table_defs in
                                 let db = hd(dbs) in
                                   (* TBD: factor this stuff out into
                                      a module that processes queries *)
                                 let result_types = query_result_types query table_defs in
       	                         let query_string = Sql.string_of_query (normalise_query globals locals db query) in
                                   prerr_endline("RUNNING QUERY:\n" ^ query_string);
		                   Database.execute_select result_types query_string db
                           | x -> raise (Runtime_error ("TF309 : " ^ string_of_result x))
                       in
                         apply_cont globals cont result
	        )
            | RecSelect (locals, label, label_var, variable, body) ->
	        let field, remaining = crack_row label (recfields value) in
                let new_env = trim_env (bind (bind locals variable
					        (`Record remaining))
				          label_var field) in
                  interpret globals new_env body cont
                    
            | StartCollExtn (locals, variable, expr) -> 
	        (match value with
                   | `List (source_elems) ->
	               (match source_elems with
		            [] -> apply_cont globals cont (`List [])
	                  | (first_elem::other_elems) ->
	                      (* bind 'var' to the first element, save the others for later *)
		              interpret globals (bind locals variable first_elem) expr
		                (CollExtn(locals, variable, expr, [], other_elems) :: cont))
	           | x -> raise (Runtime_error ("TF197 : " ^ string_of_result x)))
	          
            | CollExtn (locals, var, expr, rslts, inputs) ->
                (let new_results = match value with
                     (* Check that value's a collection, and extract its contents: *)
                   | `List (expr_elems) -> expr_elems
                   | r -> raise (Runtime_error ("TF183 : " ^ string_of_result r))
	         in
	           (* Extend rslts with the newest list of results. *)
                   let rslts = (List.rev new_results) :: rslts in
	           match inputs with
		       [] -> (* no more inputs, collect results & continue *)
		         apply_cont globals cont (`List (List.rev (List.concat rslts)))
		     | (next_input_expr::inputs) ->
		         (* Evaluate next input, continuing with given results: *)
		         interpret globals (bind locals var next_input_expr) expr
		           (CollExtn(locals, var, expr, 
				     rslts, inputs) :: cont)
	        )
                  
            | XMLCont (locals, tag, attrtag, children, attrs, elems) ->
                (let new_children = 
                   match attrtag, value with 
                       (* FIXME: multiple attrs resulting from one expr? *)
                     | Some attrtag, (`List (_) as s) -> 
                         [Attr (attrtag, charlist_as_string s)]
                     | None, (`List (elems)) ->
                         (match elems with
                            | [] -> []
                            | `XML _ :: _ ->
                                map xmlitem_of elems
                            | `Char _ :: _ ->
                                [ Result.Text(charlist_as_string value) ]
                            | _ -> failwith("Internal error: unexpected contents in XML construction"))
                     | _ -> failwith("Internal error: unexpected contents in XML construction")
                 in
                 let children = children @ new_children in
                   match attrs, elems with
                     | [], [] -> 
                         let result = listval [xmlnodeval(tag, children)] in
                           apply_cont globals cont result
                     | ((k,v)::attrs), _ -> 
                         interpret globals locals v
                           (XMLCont (locals, tag, Some k, children, attrs, elems) :: cont)
                     | _, (elem::elems) -> 
                         interpret globals locals elem
                           (XMLCont (locals, tag, None, children, attrs, elems) :: cont)
                )
            | IgnoreDef (locals, def) ->
	        interpret_definition globals locals def cont
            | Ignore (locals, expr) ->
	        interpret globals locals expr cont
    in
      scheduler globals (cont, value) stepf

and interpret_definition : environment -> environment -> definition -> continuation -> result =
  fun globals locals def cont ->
    match def with
      | Syntax.Define (name, expr, _, _) -> 
          interpret globals [] expr (Definition (globals, name) :: cont)
      | Syntax.Alien _ ->
          apply_cont globals cont (`Record [])
      | Syntax.Alias _ -> apply_cont globals cont (`Record [])
and interpret : environment -> environment -> expression -> continuation -> result =
fun globals locals expr cont ->
  let eval = interpret globals locals in
  let box_constant = function
    | Boolean b -> bool b
    | Integer i -> int i
    | String s -> string_as_charlist s
    | Float f -> float f
    | Char ch -> char ch in
  match expr with
  | Syntax.Constant (c, _) -> apply_cont globals cont (box_constant c)
  | Syntax.Variable(name, _) -> 
      let value = (lookup globals locals name) in
	apply_cont globals cont value
  | Syntax.Abs (f, _) ->
      eval f (UnopApply (locals, Result.Abs)::cont)
  | Syntax.App (f, p, _) ->
      eval f (BinopRight (locals, `App, p)::cont)
  | Syntax.Abstr (variable, body, _) ->
      apply_cont globals cont (`Function (variable, retain (freevars body) locals, () (*globals*), body))
  | Syntax.Apply (Variable ("recv", _), [], _) ->
      apply_cont globals (Recv (locals) ::cont) (`Record [])
  | Syntax.Apply (fn, [], _) ->
      eval fn (ThunkApply locals::cont)
  | Syntax.Apply (fn, params, _) ->
      eval fn (FuncArg (params, locals)::cont)
  | Syntax.Condition (condition, if_true, if_false, _) ->
      eval condition (BranchCont(locals, if_true, if_false) :: cont)
  | Syntax.Comparison (l, oper, r, _) ->
      eval l (BinopRight(locals, (oper :> Result.binop), r) :: cont)
  | Syntax.Let (variable, value, body, _) ->
      eval value (LetCont(locals, variable, body) :: cont)
  | Syntax.Rec (defs, body, _) ->
      let new_env = bind_rec locals (List.map (fun (n,v,_) -> (n,v)) defs) in
        interpret globals new_env body cont
  | Syntax.Xml_node _ as xml when Forms.islform xml ->
      eval (Forms.xml_transform locals (lookup globals locals) (interpret_safe globals locals) xml) cont
  | Syntax.Xml_node _ as xml when Forms.isinput xml -> 
      eval (Forms.xml_transform locals (lookup globals locals) (interpret_safe globals locals) xml) cont
  | Syntax.Xml_node _ as xml when Forms.islhref xml ->
      eval (Forms.xml_transform locals (lookup globals locals) (interpret_safe globals locals) xml) cont

  | Syntax.Xml_node (tag, [], [], _) -> 
      apply_cont globals cont (listval [xmlnodeval (tag, [])])
  | Syntax.Xml_node (tag, (k, v)::attrs, elems, _) -> 
      eval v (XMLCont (locals, tag, Some k, [], attrs, elems) :: cont)
  | Syntax.Xml_node (tag, [], (child::children), _) -> 
      eval child (XMLCont (locals, tag, None, [], [], children) :: cont)

  | Syntax.Record_intro (fields, None, _) ->
      apply_cont
        globals
        (StringMap.fold (fun label value cont ->
                           BinopRight(locals, `RecExt label, value) :: cont) fields cont)
        (`Record [])
  | Syntax.Record_intro (fields, Some record, _) ->
      eval record (StringMap.fold (fun label value cont ->
                                     BinopRight(locals, `RecExt label, value) :: cont) fields cont)
  | Syntax.Record_selection (label, label_variable, variable, value, body, _) ->
        eval value (RecSelect(locals, label, label_variable, variable, body) :: cont)
  | Syntax.Project (expr, label, _) ->
      eval expr (UnopApply (locals, Result.Project label) :: cont)
  | Syntax.Erase (expr, label, _) ->
      eval expr (UnopApply (locals, Result.Erase label) :: cont)
  | Syntax.Variant_injection (label, value, _) ->
       eval value (UnopApply(locals, MkVariant(label)) :: cont)
  | Syntax.Variant_selection (value, case_label, case_variable, case_body, variable, body, _) ->
      eval value (UnopApply(locals, VrntSelect(case_label, case_variable, case_body, Some variable, Some body)) :: cont)
  | Syntax.Variant_selection_empty (_) ->
      failwith("internal error: attempt to evaluate empty closed case expression")
  | Syntax.Nil _ ->
      apply_cont globals cont (`List [])
  | Syntax.List_of (elem, _) ->
      eval elem (UnopApply(locals, MkColl) :: cont)
  | Syntax.Concat (l, r, _) ->
      eval l (BinopRight(locals, `Union, r) :: cont)

  | Syntax.For (expr, var, value, _) ->
      eval value (StartCollExtn(locals, var, expr) :: cont)
  | Syntax.Database (params, _) ->
      eval params (UnopApply(locals, MkDatabase) :: cont)
	(* FIXME: the datatype should be explicit in the type-erased TableHandle *)
(*   | Syntax.Table (database, s, query, _) -> *)
(*       eval database (UnopApply(locals, QueryOp(query)) :: cont) *)

  | Syntax.TableHandle (database, table_name, (readtype, writetype), _) ->   (* getting type from inferred type *)
      begin
        match readtype with
          | `Record row ->
              eval database (BinopRight(locals, `MkTableHandle row, table_name) :: cont)
          | _ ->
              failwith ("table rows must have record type")
      end

  | Syntax.TableQuery (ths, query, d) ->
      (* [ths] is an alist mapping table aliases to expressions that
         provide the corresponding TableHandles. We evaluate those
         expressions and rely on them coming through to the continuation
         in the same order. That way we can stash the aliases in the
         continuation frame & match them up later. *)
      let aliases, th_exprs = split ths in
        eval (Syntax.list_expr d th_exprs)
          (UnopApply(locals, QueryOp(query, aliases)) :: cont)
  | Syntax.Call_cc(arg, _) ->
      let cc = `Continuation cont in
        eval arg (FuncApplyFlipped(locals, cc) :: cont)
  | Syntax.SortBy (list, byExpr, d) ->
      eval (Apply (Variable ("sortBy", d), [byExpr; list], d)) cont
  | Syntax.Wrong (_) ->
      failwith("Went wrong (pattern matching failed?)")
  | Syntax.HasType(expr, _, _) ->
      eval expr cont
  | Syntax.Placeholder (_, _) -> 
      failwith("Internal error: Placeholder at runtime")


and interpret_safe globals locals expr cont =
  try 
    interpret globals locals expr cont
  with
    | TopLevel s -> snd s
    | Not_found -> failwith "Internal error: Not_found while interpreting."

let run_program (globals : environment) locals (Program (defs, body)) : (environment * result)= 
  try (
    (match defs with
       | [] ->
           interpret globals locals body []
       | def :: defs ->
           interpret_definition globals locals def
             (map (fun def -> IgnoreDef([], def)) defs @ [Ignore([], body)]));
    failwith "boom"
  ) with
    | TopLevel s -> s
    | Not_found -> failwith "Internal error: Not_found while interpreting."

let run_defs (globals : environment) locals defs : environment =
  let env, _ =
    run_program globals locals
      (Program (defs, (Syntax.unit_expression (Syntax.no_expr_data))))
  in
    env

let apply_cont_safe x y z = 
  try apply_cont x y z
  with
    | TopLevel s -> snd s
    | Not_found -> failwith "Internal error: Not_found while interpreting."
