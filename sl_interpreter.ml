open Str
open Num
open List
open Lexing

open Sl_utility
open Sl_kind
open Sl_result
open Sl_sql
open Query
open Sl_syntax
open Sl_database
open Sl_library

(* Environment handling *)

(* Environment handling *)

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

(** bind_rec globals locals [name1, value1; name2, value2; name3, value3; ...]
    Extend `locals' with mutually-recursive bindings for namen to valuen,
    with the values values mutually recursive with respect to one another's 
    names.
*)
let bind_rec globals locals defs =
  (* First, create bindings for these functions, with no local
     variables for now *)
  let make_placeholder env (variable, value) =
    match value with
      | Sl_syntax.Abstr (var, body, _) ->
          bind env variable (`Function (var, [], [], body))
  in
  let new_env = trim_env (fold_left make_placeholder locals defs) in
    (* Then, fill in the local variables with values *)
  let fill_placeholder (label, result) =
    label, (match result with
              | `Function (var, _, _, body) ->
                  `Function (var, (retain (freevars body) new_env), globals, body)
              | _ -> result)
  in
    trim_env (map fill_placeholder new_env) 
      
(* Remove toplevel bindings from an environment *)
let remove_toplevel_bindings toplevel env = 
  filter (fun pair -> mem pair toplevel) env

let rec lookup_qname toplevel locals =
  let lookup_simple_name name = 
    try assoc name locals
    with Not_found -> assoc name toplevel
  in
    function
        | [name] -> lookup_simple_name name
        | namespace :: names -> 
	    (match lookup_simple_name namespace with
                                   | `Environment (url, env) -> lookup_qname env locals names 
                                   | _ -> failwith (namespace ^ " is not a namespace"))

let lookup toplevel locals name = 
  try
    lookup_qname toplevel locals (split_string name ':')
  with Not_found ->
    debug ("Not_found during `lookup' : "  ^ name);
    raise Not_found

let bind_rec globals locals defs =
  (* create bindings for these functions, with no local variables for now *)
  let make_placeholder = (fun env (variable, value) ->
                            (match value with
                               | Sl_syntax.Abstr (var, body, _) ->
                                   bind env variable (`Function (var, [], [] (*globals*), body))
                               | _ -> raise (Runtime_failure "TF146"))) in
  let new_env = trim_env (fold_left make_placeholder locals defs) in
    (* fill in the local variables *)
  let fill_placeholder = (fun (label, result) ->
                            (match result with
                               | `Function (var, _, _, body) ->
                                   label, `Function (var, (retain (freevars body) new_env), globals, body)
                               | _ -> (label, result)))
  in
    trim_env (map fill_placeholder new_env) 



let rec crack_row : (string -> ((string * result) list) -> (result * (string * result) list)) = fun ref_label -> function
        | [] -> raise (Runtime_failure("Internal error: no field '" ^ ref_label ^ "' in record"))
        | (label, result) :: fields when label = ref_label ->
            (result, fields)
        | field :: fields ->
            let selected, remaining = crack_row ref_label fields in
              (selected, field :: remaining)

let rec equal l r =
  match l, r with
    | `Primitive (`Bool l  ), `Primitive (`Bool r  ) -> l = r
    | `Primitive (`Int l   ), `Primitive (`Int r   ) -> eq_num l r
    | `Primitive (`Float l ), `Primitive (`Float r ) -> l = r
    | `Primitive (`Char l  ), `Primitive (`Char r  ) -> l = r
    | `Function _, `Function _ -> serialise_result l = serialise_result r
    | `Record lfields, `Record rfields -> 
        let rec one_equal_all = (fun alls (ref_label, ref_result) ->
                                   match alls with
                                     | [] -> false
                                     | (label, result) :: alls when label = ref_label -> equal result ref_result
                                     | all :: alls -> one_equal_all alls (ref_label, ref_result)) in
          for_all (one_equal_all rfields) lfields && for_all (one_equal_all lfields) rfields
    | `Variant (llabel, lvalue), `Variant (rlabel, rvalue) -> llabel = rlabel && equal lvalue rvalue
    | `Collection (`List, l), `Collection (`List, r) -> length l = length r &&
            fold_left2 (fun result x y -> result && equal x y) true l r
    | l, r ->  failwith ("Comparing "^ string_of_result l ^" with "^ string_of_result r ^" either doesn't make sense or isn't implemented")

let rec less l r =
  match l, r with
    | `Primitive(`Bool l), `Primitive(`Bool r)   -> l < r
    | `Primitive(`Int l), `Primitive(`Int r)     -> lt_num l r
    | `Primitive(`Float l), `Primitive(`Float r) -> l < r
    | `Primitive(`Char l), `Primitive(`Char r) -> l < r
    | `Function _ , `Function _                  -> serialise_result l < serialise_result r
        (* Compare fields in lexicographic order of labels *)
    | `Record lf, `Record rf -> 
        let order = sort (fun x y -> compare (fst x) (fst y)) in
        let lv, rv = map snd (order lf), map snd (order rf) in
        let rec compare_list = function
          | [] -> false
          | (l,r)::_ when less l r -> true
          | (l,r)::_ when less r l -> false
          | _::rest                -> compare_list rest in
          compare_list (combine lv rv)
    | `Collection (`List, l), `Collection (`List, r) ->
        (try for_all2 less l r
         with Invalid_argument msg -> failwith ("Error comparing lists : "^msg))
    | l, r ->  failwith ("Cannot yet compare "^ string_of_result l ^" with "^ string_of_result r)

let less_or_equal l r = equal l r || less l r
        
let rec normalise_query (env:environment) (qry:query) : query =
  let rec normalise_expression (env:environment) (expr:Query.expression) : Query.expression = (
    match expr with
      | Query.Variable name ->
          (try
             match assoc name env with
               | `Primitive(`Bool value) -> Query.Boolean value
               | `Primitive(`Int value) -> Query.Integer value
               | `Primitive(`Float value) -> Query.Float value
               | `Collection (`List, `Primitive(`Char _)::elems) as c  
                 -> Query.Text (Postgresql.escape_string (charlist_as_string c))
               | `Collection (`List, []) -> Query.Text ""
               | r -> failwith("Internal error: variable in query " ^ string_of_query qry ^ " had inappropriate type at runtime; it was " ^ string_of_result r)
           with Not_found -> failwith("internal error: undefined query variable '" ^ name ^ "'"))
            (* UGLY HACK BELOW *)
      | Query.Binary_op ("concat", left, right) ->
          (match normalise_expression env left, normalise_expression env right with
              Query.Text lstr, Query.Text rstr -> Query.Text (lstr ^ rstr))
      | Query.Binary_op (symbol, left, right) ->
          Binary_op (symbol, normalise_expression env left, normalise_expression env right)
      | Query.Unary_op (symbol, expr) ->
          Unary_op (symbol, normalise_expression env expr)
      | Query.Query qry ->
          Query {qry with condition = normalise_expression env qry.condition}
      | _ -> expr)
  in {qry with condition = normalise_expression env qry.condition}

(* should we just use BinOp values in the first place?*)
let binopFromOpString = function
    | "==" -> EqEqOp
    | "<>" -> NotEqOp
    | "<=" -> LessEqOp
    | "<<" -> LessOp
    | "beginswith" -> BeginsWithOp
    | opstr -> raise(Runtime_failure("Evaluating unknown operator "^opstr))

let collect ctype = function elements ->
  match ctype with
    `Set -> `Collection (ctype, unduplicate equal elements)
  | `Bag | `List -> `Collection (ctype, elements)
  | _ -> raise (Runtime_failure "Abstract collection type used concretely")

exception CollExtnWithWeirdSrc
exception TopLevel of (Sl_result.environment * Sl_result.result)

let stepper = ref 0
let switch_granularity = 5

let rec switch_context globals = 
  if not (Queue.is_empty Sl_library.suspended_processes) then 
    let cont, p, pid = Queue.pop Sl_library.suspended_processes in
      Sl_library.current_pid := pid;
      apply_cont globals cont p
  else exit 0
and apply_cont globals : continuation -> result -> result = 
fun cont value ->
(*  prerr_endline ("processes : " ^ (string_of_int (Queue.length Sl_library.suspended_processes)));
  prerr_endline ("blocked processes : " ^ (string_of_int (Hashtbl.length Sl_library.blocked_processes)));*)
  incr stepper;
  if (!stepper mod switch_granularity == 0) then 
    begin
      Queue.push (cont, value, !current_pid) Sl_library.suspended_processes;
      switch_context globals
    end
  else
    (match cont with
       | [] -> (if !Sl_library.current_pid == 0 then raise (TopLevel(globals, value))
		else switch_context globals)
       | (frame::cont) -> match frame with
	   | (Definition(env, name)) -> 
	       apply_cont (bind env name value) cont value
           | Recv (locals) ->
               (*prerr_endline "recv continuation";*)
               (* If there are any messages, apply the continuation and continue.
                  Otherwise, suspend the continuation (in the blocked_processes table)
               *)
(*               prerr_endline "recvcont";*)
               let mqueue = Hashtbl.find Sl_library.messages !Sl_library.current_pid in
                 if not (Queue.is_empty mqueue) then
                   apply_cont globals cont (Queue.pop mqueue)
                 else 
                   begin
                     Hashtbl.add Sl_library.blocked_processes !Sl_library.current_pid (cont, value, !Sl_library.current_pid);
                     switch_context globals
                   end
           | (FuncArg(param, locals)) ->
	       (* just evaluate param; "value" is in fact a function
	          value which will later be applied *)
               interpret globals locals param
	         (FuncApply(value, locals) :: cont)
           | (FuncApply(func, locals)) -> 
               (match func with
                  | `Function (var, fnlocals, fnglobals, body) ->
		      (* Interpret the body in the following environments:
		         locals are augmented with function locals and
		         function binding (binding takes precedence)
		         toplevel environment in which the function
		         was defined takes precedence over the real current
                         globals. Is there a semanticist in the house? *)
(*                 let locals = bind (trim_env (fnlocals @ locals @ fnglobals)) var value in*)
                 let locals = bind (trim_env (fnlocals @ locals)) var value in
                   interpret globals locals body cont
		     
             | `Primitive (`PFunction (name, impl, pargs)) ->
		 (match impl with
		    | Some func ->
                        func (apply_cont globals, cont, value)
		    | None -> 
                        (* this primitive's implementation was
                           deserialized away; should be able to get it
                           from the global env. *)
                        let func = (Sl_library.get_prim name) in
			  func (apply_cont globals, cont, value)
		 )
	     | `Continuation (cont) ->
		 (* Here we throw out the other continuation. *)
		 apply_cont globals cont value
             | _ -> raise (Runtime_failure ("Applied non-function value: " ^
                                              string_of_result func)))
           | (LetCont(locals, variable, body)) ->
	       interpret globals (bind locals variable value) body cont
           | (BranchCont(locals, true_branch, false_branch)) ->
	       (match value with
                  | `Primitive (`Bool true)  -> 
	              interpret globals locals true_branch cont
                  | `Primitive (`Bool false) -> 
	              interpret globals locals false_branch cont
                  | _ -> raise (Runtime_failure("Attempt to test a non-boolean value: "
					        ^ string_of_result value)))
           | (BinopRight(locals, op, rhsExpr)) ->
	       interpret globals locals rhsExpr
                 (BinopApply(locals, op, value) :: cont)
           | (BinopApply(locals, op, lhsVal)) ->
	       let result = 
                 (match op with
                    | EqEqOp -> bool (equal lhsVal value)
                    | NotEqOp -> bool (not (equal lhsVal value))
                    | LessEqOp -> bool (less_or_equal lhsVal value)
                    | LessOp -> bool (less lhsVal value)
                    | BeginsWithOp -> failwith("Beginswith not implemented except when pushable into SQL")
	            | UnionOp(ctype) -> (match lhsVal, value with
		          `Collection (`Set, l),   `Collection (`Set, r)   -> `Collection (`Set, (unduplicate equal (l @ r)))
	                | `Collection (`Bag, l),   `Collection (`Bag, r)   -> `Collection (`Bag, (l @ r))
	                | `Collection (`List, l),  `Collection (`List, r)  -> `Collection (`List, (l @ r))
	                | _ -> raise (Runtime_failure ("Union of non-collection types: " ^ string_of_result lhsVal ^ " and " ^ string_of_result value))
				        )
	            | RecExtOp(label) -> 
		        (match lhsVal with
		           | `Record fields -> 
		               `Record ((label, value) :: fields)
		           | _ -> raise (Runtime_failure "TF077"))
	         )
	       in
	         apply_cont globals cont result
           | UnopApply (locals, op) ->
               (match op with
                    MkColl(coll_type) -> apply_cont globals cont (`Collection (coll_type, [(value)]))
	          | MkVariant(label) -> 
	              apply_cont globals cont (`Variant (label, value))
                  | VrntSelect(case_label, case_variable, case_body, variable, body) ->
	              (match value with
                         | `Variant (label, value) when label = case_label ->
                             (interpret globals (bind locals case_variable value) case_body cont)
                         | `Variant (_) as value ->
		             (interpret globals (bind locals (valOf variable) value)
		                (valOf body) cont)
                         | _ -> raise (Runtime_failure "TF181"))
	          | MkDatabase ->
	              apply_cont globals cont (let args = charlist_as_string value in
                                               let driver, params = parse_db_string args in
                                                 `Database (db_connect driver params))
                  | QueryOp(query, kind) ->
                      let result = 
                        match value with
                          | `Database (db, _) ->
       	                      let query_string = string_of_query (normalise_query locals query) in
		                prerr_endline("RUNNING QUERY:\n" ^ query_string);
		                let result = execute_select kind query_string db in
                                (* debug("    result:" ^ string_of_result result); *)
                                  result
                                (* disable actual queries *)
(*                                   `Collection(`List, []) *)
                          | x -> raise (Runtime_failure ("TF309 : " ^ string_of_result x))
                      in
                        apply_cont globals cont result
	          | SortOp(up) ->
	              apply_cont globals cont
		        (match value with
		           | `Collection(_, results) ->
		               let cmp = (if up then less_to_cmp less
                                          else curry (compose (~-) (uncurry (less_to_cmp less)))) in
		                 listval(List.sort cmp results)
		           | _ -> raise (Runtime_failure "TF223"))
	       )
           | RecSelect (locals, label, label_var, variable, body) ->
	       let field, remaining = crack_row label (recfields value) in
               let new_env = trim_env (bind (bind locals variable
					       (`Record remaining))
				         label_var field) in
                 interpret globals new_env body cont
                   
           | StartCollExtn (locals, variable, expr) -> 
	       (match value with
                  | `Collection (source_coll_type, source_elems) ->
	              (match source_elems with
		           [] -> apply_cont globals cont (`Collection(source_coll_type, []))
	                 | (first_elem::other_elems) ->
	                     (* bind 'var' to the first element, save the others for later *)
		             interpret globals (bind locals variable first_elem) expr
		               (CollExtn(locals, source_coll_type, 
                                         variable, expr, [], other_elems) :: cont))
	          | x -> raise (Runtime_failure ("TF197 : " ^ string_of_result x)))
	         
           | CollExtn (locals, ctype, var, expr, rslts, inputs) ->
               (let new_results = match value with
                    (* Check that value's a collection, and extract its contents: *)
                  | `Collection (expr_coll_type, expr_elems) -> expr_elems
                  | r -> raise (Runtime_failure ("TF183 : " ^ string_of_result r))
	        in
                let join = match ctype with (* FIXME: use this! *)
		    `Set  -> fun (a,b) -> unduplicate equal (a@b)
		    | `Bag | `List -> (fun (a,b) -> a@b)
		    | _ -> failwith("Internal error: comprhnsn src is non-collection") in
	          (* Extend rslts with the newest list of results. *)
                let rslts = rslts @ new_results in
	          match inputs with
		      [] -> (* no more inputs, collect results & continue *)
		        apply_cont globals cont (collect ctype rslts)
		    | (next_input_expr::inputs) ->
		        (* Evaluate next input, continuing with given results: *)
		        interpret globals (bind locals var next_input_expr) expr
		          (CollExtn(locals, ctype, var, expr, 
				    rslts, inputs) :: cont)
	       )
                 
           | XMLCont (locals, tag, attrtag, children, attrs, elems) ->
               (let new_children = 
                  match attrtag, value with 
                      (* FIXME: multiple attrs resulting from one expr? *)
                    | Some attrtag, (`Collection (`List, _) as s) -> 
                        [Attr (attrtag, charlist_as_string s)]
                    | None, (`Collection (`List, elems)) ->
                        (match elems with
                           | [] -> []
                           | `Primitive (`XML x) :: etc ->
                               map xmlitem_of elems
                           | `Primitive (`Char x) :: etc ->
                               [ Sl_result.Text(charlist_as_string value) ]
                           | _ -> raise(Match_failure("",0,0)))
                    | _ -> raise(Match_failure("",0,0))
                in
                let children = children @ new_children in
                  match attrs, elems with
                    | [], [] -> 
                        let result = (`Collection (`List, [`Primitive (`XML (Node (tag, children)))])) in
                          apply_cont globals cont result
                    | ((k,v)::attrs), _ -> 
                        interpret globals locals v (XMLCont (locals, tag, Some k, children, attrs, elems) :: cont)
                    | _, (elem::elems) -> 
                        interpret globals locals elem (XMLCont (locals, tag, None, children, attrs, elems) :: cont)
               )
           | Ignore (locals, expr) ->
	       interpret globals locals expr cont
    )

and
    interpret
    : environment -> environment -> expression -> 
  continuation -> result =
fun globals locals expr cont ->
  let eval = interpret globals locals in
  match expr with
  | Sl_syntax.Define (name, expr, _, _) -> interpret globals [] expr (Definition (globals, name) :: cont)
(*  | Sl_syntax.Define (name, value, _, _) -> failwith "DF 245"*)
  | Sl_syntax.Boolean (value, _) -> apply_cont globals cont (bool value)
  | Sl_syntax.Integer (value, _) -> apply_cont globals cont (int value)
  | Sl_syntax.String (value, _) -> apply_cont globals cont (string_as_charlist value)
  | Sl_syntax.Float (value, _) -> apply_cont globals cont (float value)
  | Sl_syntax.Char (value, _) -> apply_cont globals cont (char value)
  | Sl_syntax.Variable(name, _) -> 
      let varval = (lookup globals locals name) in
	apply_cont globals cont varval
  | Sl_syntax.Abstr (variable, body, _) ->
      apply_cont globals cont (`Function (variable, retain (freevars body) locals, [] (*globals*), body))
  | Sl_syntax.Apply (Variable ("recv", _), Record_empty _, _) ->
      apply_cont globals (Recv (locals) ::cont) (`Record [])
  | Sl_syntax.Apply (fn, param, _) ->
      eval fn (FuncArg(param, locals) :: cont)
  | Sl_syntax.Condition (condition, if_true, if_false, _) ->
      eval condition (BranchCont(locals, if_true, if_false) :: cont)
  | Sl_syntax.Comparison (l, oper, r, _) ->
      eval l (BinopRight(locals, binopFromOpString oper, r) :: cont)
  | Sl_syntax.Let (variable, value, body, _) ->
      eval value (LetCont(locals, variable, body) :: cont)
  | Sl_syntax.Rec (variables, body, _) ->
      let new_env = bind_rec globals locals variables in
        interpret globals new_env body cont
  | Sl_syntax.Xml_node _ as xml when Sl_forms.islform xml ->
      eval (Sl_forms.xml_transform locals xml) cont
  | Sl_syntax.Xml_node _ as xml when Sl_forms.isinput xml -> 
      eval (Sl_forms.xml_transform locals xml) cont
  | Sl_syntax.Xml_node _ as xml when Sl_forms.islhref xml ->
      eval (Sl_forms.xml_transform locals xml) cont

  | Sl_syntax.Xml_node (tag, [], [], _) -> 
      apply_cont globals cont (listval [xmlnodeval (tag, [])])
  | Sl_syntax.Xml_node (tag, (k, v)::attrs, elems, _) -> 
      eval v (XMLCont (locals, tag, Some k, [], attrs, elems) :: cont)
  | Sl_syntax.Xml_node (tag, [], (child::children), _) -> 
      eval child (XMLCont (locals, tag, None, [], [], children) :: cont)

  | Sl_syntax.Record_empty _ -> apply_cont globals cont (`Record [])
  | Sl_syntax.Record_extension (label, value, record, _) ->
      eval record (BinopRight(locals, RecExtOp label, value) :: cont)
  | Sl_syntax.Record_selection (label, label_variable, variable, value, body, (pos, kind, lbl)) ->
        eval value (RecSelect(locals, label, label_variable, variable, body) :: cont)
  | Sl_syntax.Record_selection_empty (value, body, _) ->
      eval value (Ignore(locals, body) :: cont)
  | Sl_syntax.Variant_injection (label, value, _) ->
       eval value (UnopApply(locals, MkVariant(label)) :: cont)
  | Sl_syntax.Variant_selection (value, case_label, case_variable, case_body, variable, body, _) ->
      eval value (UnopApply(locals, VrntSelect(case_label, case_variable, case_body, Some variable, Some body)) :: cont)
  | Sl_syntax.Variant_selection_empty (value, case_label, case_variable, case_body, _) ->
      eval value (UnopApply(locals, VrntSelect(case_label, case_variable, case_body, None, None)) :: cont)
  | Sl_syntax.Collection_empty (coll_type, _) ->
      apply_cont globals cont (`Collection (coll_type, []))
  | Sl_syntax.Collection_single (elem, coll_type, _) ->
      eval elem (UnopApply(locals, MkColl(coll_type)) :: cont)
  | Sl_syntax.Collection_union (l, r, _) ->
      eval l (BinopRight(locals, UnionOp(`Set (*FIXME*)), r) :: cont)

  | Sl_syntax.Collection_extension (expr, var, value, _) as c ->
      eval value (StartCollExtn(locals, var, expr) :: cont)
  | Sl_syntax.Sort (up, list, _) ->
      eval list (UnopApply(locals, SortOp up) :: cont)
  | Sl_syntax.Database (params, _) ->
      eval params (UnopApply(locals, MkDatabase) :: cont)
  | Sl_syntax.Table (database, s, query, (_, kind, _)) ->
      eval database (UnopApply(locals, QueryOp(query, kind)) :: cont)
  | Sl_syntax.Escape (var, body, _) ->
      let locals = (bind locals var (`Continuation cont)) in
      interpret globals locals body cont

let run_program (globals : environment) exprs : (environment * result)= 
  try (
    interpret globals [] (hd exprs) (map (fun expr -> Ignore([], expr)) (tl exprs));
    failwith "boom"
  ) with
    | TopLevel s -> s

let apply_cont_safe x y z = 
  try apply_cont x y z
  with
    | TopLevel s -> snd s


