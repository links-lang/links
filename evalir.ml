open Utility

module Eval = struct
  open Ir

  exception EvaluationError of string
  exception Wrong
  exception TopLevel of (Value.env * Value.t)

  let eval_error fmt = 
    let error msg = raise (EvaluationError msg) in
      Printf.kprintf error fmt

  let db_connect : Value.t -> Value.database * string = fun db ->
    let driver = Value.unbox_string (Value.project "driver" db)
    and name = Value.unbox_string (Value.project "name" db)
    and args = Value.unbox_string (Value.project "args" db) in
    let params =
      (if args = "" then name
       else name ^ ":" ^ args)
    in
      Value.db_connect driver params

   let lookup_var var env =
     match Value.lookup var env with
       | Some v -> Some v
       | None -> Some (Lib.primitive_stub (Lib.primitive_name var))

  let client_call : string -> Value.continuation -> Value.t list -> 'a =
    fun _ _ _ -> assert false

  let apply_prim : string -> Value.t list -> Value.t = Lib.apply_pfun

  module Q =
  struct
    (** Substitutes values for the variables in a query, and performs
        interpolation in LIKE expressions. *)
    let rec normalise_query (env:Value.env) (db:Value.database) 
        (qry:SqlQuery.sqlQuery) : SqlQuery.sqlQuery =

      let normalise_like_expression (l : SqlQuery.like_expr): SqlQuery.like_expr = 
        let quote = Str.global_replace (Str.regexp_string "%") "\\%" in
        let rec nle =
          function
            | `Var x -> `Str (quote (Value.unbox_string (Value.find (int_of_string x) env)))
            | (`Percent | `Str _) as l -> l
            | `Seq ls -> `Seq (List.map nle ls)
        in
          nle l
      in
      let rec normalise_expression : SqlQuery.sqlexpr -> SqlQuery.sqlexpr = function
        | `V name -> begin
            try
              match Value.find (int_of_string name) env with
                | `Bool true -> `True
                | `Bool false -> `False
                | `Int value -> `N value
                | `List (`Char _::_) as c  
                  -> `Str (db # escape_string (Value.unbox_string c))
                | `List ([]) -> `Str ""
                | `Char c -> `Str (String.make 1 c)
                | v -> failwith("Internal error: variable " ^ name ^ 
                                  " in query "^ SqlQuery.string_of_query qry ^ 
                                  " had unexpected type at runtime: " ^ 
                                  Value.string_of_value v)
            with NotFound _ -> failwith ("Internal error: undefined query variable '"^
                                           name^"'")
          end
        | `Op (symbol, left, right) ->
            `Op(symbol, normalise_expression left, normalise_expression right)
        | `Not expr ->
            `Not(normalise_expression expr)
        | `Like(lhs, regex) -> 
            `Like(normalise_expression lhs,
                  normalise_like_expression regex)
        | expr -> expr
      in
      let normalise_tables =
        List.map (function 
               | `TableVar(var, alias) ->
                   (match Value.find (int_of_string var) env with
                        `Table(_, tableName, _) -> `TableName(tableName, alias)
                      | _ -> failwith "Internal Error: table source was not a table!")
               | `TableName (name, alias) -> `TableName(name, alias)
               | `SubQuery _ ->
                   failwith "Not implemented subqueries yet"
            ) 
      in {qry with
            SqlQuery.tabs = normalise_tables qry.SqlQuery.tabs;
            SqlQuery.cond = List.map normalise_expression qry.SqlQuery.cond;
            (* TBD: allow variables as the from/most values, normalise them here. *)
        SqlQuery.from = qry.SqlQuery.from;
        SqlQuery.most = qry.SqlQuery.most}

    exception NoSuchField of string

    let query_result_types (query : SqlQuery.sqlQuery)
        : (string * Types.datatype) list =
      try 
        concat_map
          (function
               (`F field, alias) -> 
                 [alias, field.SqlQuery.ty]
             | (expr, _alias) -> failwith("Internal error: no type info for sql expression " 
                                          ^ SqlQuery.string_of_expression expr))
          query.SqlQuery.cols
      with NoSuchField field ->
        failwith ("Field " ^ field ^ " from " ^ 
                    SqlQuery.string_of_query query)

    let do_query : Value.env -> SqlQuery.sqlQuery -> Value.t = fun env query ->
      let get_database : SqlQuery.sqlQuery -> Value.database = fun query ->
        let vars = concat_map (function
                                 | `TableVar (var, _) -> [int_of_string var]
                                 | _ -> []) query.SqlQuery.tabs in
        let dbs = 
          List.map (fun var -> 
                 match Value.find var env with
                   | `Table((db, _params), _table_name, _row) -> db
                   | _ -> assert false) vars in
          
          assert (dbs <> []);
          
          if(not (all_equiv (=) dbs)) then
            failwith ("Cannot join across different databases");
          
          List.hd(dbs) in

      let db = get_database query in
        (* TBD: factor this stuff out into a module that processes
           queries *)
      let result_types = query_result_types query in
      let query_string = SqlQuery.string_of_query (normalise_query env db query) in
        
        prerr_endline("RUNNING QUERY:\n" ^ query_string);
        let t = Unix.gettimeofday() in
        let result = assert false in
(*        let result = Database.execute_select result_types query_string db in*)
          Debug.print("Query took : " ^ 
                        string_of_float((Unix.gettimeofday() -. t)) ^ "s");
          (*result*) assert false
  end

  let do_query : Value.env -> SqlQuery.sqlQuery -> Value.t = Q.do_query

  let switch_context _ = 
    assert false

  let rec value env : Ir.value -> Value.t = function
    | `Constant `Bool b -> `Bool b
    | `Constant `Int n -> `Int n
    | `Constant `Char c -> `Char c
    | `Constant `String s -> Value.box_string s
    | `Constant `Float f -> `Float f
    | `Variable var ->
        (match lookup_var var env with
           | Some v -> v
           | _      -> eval_error "Variable not found: %d" var)
    | `Extend (fields, r) -> 
        (match opt_app (value env) (`Record []) r with
           | `Record fs ->
               `Record (StringMap.fold 
                          (fun label v fs ->
                             if List.mem_assoc label fs then 
                               eval_error
                                 "Error adding fields: label %s already present"
                                 label
                             else (label,value env v)::fs)
                          fields
                          fs)
           | _          -> eval_error "Error adding fields: non-record")
    | `Project (label, r) ->
        (match value env r with
           | `Record fields when List.mem_assoc label fields ->
               List.assoc label fields
           | _ -> eval_error "Error projecting label %s" label)
    | `Erase (label, r) ->
        (match value env r with
           | `Record fields when List.mem_assoc label fields ->
               `Record (List.remove_assoc label fields)
           | _ -> eval_error "Error erasing label %s" label)
    | `Inject (label, v, t) -> `Variant (label, value env v)
    | `TAbs (_, v) -> value env v
    | `TApp (v, _) -> value env v
    | `XmlNode (tag, attrs, children) ->
        let children =
          List.fold_right
            (fun v children ->
               let v = value env v in
                 List.map Value.unbox_xml (Value.unbox_list v) @ children)
            children [] in
        let children =
          StringMap.fold 
            (fun name v attrs ->
               Value.Attr (name, Value.unbox_string (value env v)) :: attrs)
            attrs children
        in
          Value.box_list [Value.box_xml (Value.Node (tag, children))]
    | `ApplyPure (f, args) ->
        begin
          try (
            ignore (apply [] env (value env f, List.map (value env) args));
            failwith "boom"
          ) with
            | TopLevel (_, v) -> v
        end           
    | `Coerce (v, _) -> value env v
    (* TODO: replace comparisons with primitive functions *)
    | `Comparison _ -> assert false
(*
    | `Comparison (v1, op, v2)  ->
        begin
          match op with
            | `Equal -> Value.box_bool (Lib.equal (value env v1) (value env v2))
            | _ -> assert false
        end
*)
  and apply cont env : Value.t * Value.t list -> Value.t = function
    | `RecFunction (recs, locals, n), ps -> 
        begin
          match lookup n recs with
            | Some (args, body) ->
                (* unfold recursive definitions once *)

                (* extend locals with recs *)
                let locals =
                  List.fold_right
                    (fun (name, _) env ->
                       Value.bind name (`RecFunction (recs, env, name), `Local) env)
                    recs locals in

                (* extend env with locals *)
                let env = Value.shadow env ~by:locals in

                (* extend env with arguments *)
                let env = List.fold_right2 (fun arg p -> Value.bind arg (p, `Local)) args ps env in
                  computation env cont body
            | None -> eval_error "Error looking up recursive function definition"
        end
    | `PrimitiveFunction n, ps -> apply_cont cont env (apply_prim n ps)
    | `ClientFunction name, ps -> client_call name cont ps
    | `Continuation c,     [p] -> apply_cont c env p
    | `Continuation _,      _  ->
        eval_error "Continuation applied to multiple (or zero) arguments"
    | _                        -> eval_error "Application of non-function"
  and apply_cont cont env v : Value.t =
    match cont with
      | [] (* when !Library.current_pid == Library.main_process_pid *) ->
          raise (TopLevel (Value.globals env, v))
(*      | [] -> switch_context env *)
      | (scope, var, locals, comp)::cont ->
          let env = Value.bind var (v, scope) (Value.shadow env ~by:locals) in
            computation env cont comp
  and computation env cont (binders, tailcomp) : Value.t =
(*    Debug.print ("comp: "^Ir.Show_program.show (binders, tailcomp));*)
    match binders with
      | [] -> tail_computation env cont tailcomp
      | b::bs -> match b with
          | `Let ((var, _) as b, (_, tc)) ->
(*               Debug.print ("var: "^string_of_int var); *)
              tail_computation env (((Var.scope_of_binder b, var, env, (bs, tailcomp))::cont) : Value.continuation) tc
          | `Fun ((f, _) as fb, (_, args, body), `Client) ->
(*               Debug.print ("client f: "^string_of_int f); *)
              computation (Value.bind f (`ClientFunction (Var.name_of_binder fb), Var.scope_of_binder fb) env) cont (bs, tailcomp)
          | `Fun ((f, _) as fb, (_, args, body), _) -> 
(*               Debug.print ("f: "^string_of_int f); *)
              computation (Value.bind
                             f
                             (`RecFunction ([f, (List.map fst args, body)], env, f),
                              Var.scope_of_binder fb) env) cont (bs, tailcomp)
          | `Rec defs ->
              let bindings = List.map (fun ((f,_), (_, args, body), _) ->
                                         f, (List.map fst args, body)) defs in
              let env = 
                List.fold_right (fun ((f, _) as fb, _, _) env ->
(*                                    Debug.print ("rec f: "^string_of_int f); *)
                                   Value.bind f
                                     (`RecFunction (bindings, env, f),
                                      Var.scope_of_binder fb)
                                     env) defs env
              in
                computation env cont (bs, tailcomp)
          | `Alien _ 
          | `Alias _       -> (* just skip it *)
              computation env cont (bs, tailcomp)
          | `Module _ -> failwith "Not implemented interpretation of modules yet"
  and tail_computation env cont : Ir.tail_computation -> Value.t = function
    | `Return v      -> apply_cont cont env (value env v)
    | `Apply (f, ps) -> 
        apply cont env (value env f, List.map (value env) ps)
    | `Special s     -> special env cont s
    | `Case (v, cases, default) -> 
        (match value env v with
           | `Variant (label, _) as v ->
               (match StringMap.lookup label cases, default, v with
                  | Some ((var,_), c), _, `Variant (_, v)
                  | _, Some ((var,_), c), v ->
                      computation (Value.bind var (v, `Local) env) cont c
                  | None, _, #Value.t -> eval_error "Pattern matching failed"
                  | _ -> assert false (* v not a variant *))
           | _ -> eval_error "Case of non-variant")
    | `If (c,t,e)    -> 
        computation env cont
          (match value env c with
             | `Bool true     -> t
             | `Bool false    -> e
             | _              -> eval_error "Conditional was not a boolean")
  and special env cont : Ir.special -> Value.t = function
    | `Wrong _                    -> raise Wrong
    | `Database v                 -> `Database (db_connect (value env v))
    | `Query q                    -> do_query env q
    | `Table (db, name, (readtype, _)) -> 
        (match value env db, value env name, readtype with
           | `Database (db, params), name, `Record row ->
               `Table ((db, params), Value.unbox_string name, row)
           | _ -> eval_error "Error evaluating table handle")
    | `CallCC f                   -> 
        apply cont env (value env f, [`Continuation cont])
  let eval : Value.env -> program -> Value.t = 
    fun env -> computation env Value.toplevel_cont (*(assert false : Value.continuation)*)
end

let run_program : Value.env -> Ir.program -> (Value.env * Value.t) =
  fun env program ->
    try (
      ignore 
        (Eval.eval env program);
      failwith "boom"
    ) with
      | Eval.TopLevel (env, v) -> (env, v)
      | NotFound s -> failwith ("Internal error: NotFound "^s^" while interpreting.")

let run_defs : Value.env -> Ir.binding list -> Value.env =
  fun env bs ->
    let env, _ = run_program env (bs, `Return (`Extend (StringMap.empty, None))) in
      env

(* this is used to return the value returned by applying a continuation *)
let apply_cont_safe cont env v = 
  try Eval.apply_cont cont env v
  with
    | Eval.TopLevel s -> snd s
    | NotFound s -> failwith ("Internal error: NotFound "^s^" while interpreting.")

let apply_safe env (f, vs) =
  try Eval.apply [] env (f, vs)
  with
    | Eval.TopLevel s -> snd s
    | NotFound s -> failwith ("Internal error: NotFound "^s^" while interpreting.")
