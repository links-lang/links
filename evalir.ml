module Eval = struct
  open Ir
  open Utility

  exception EvaluationError of string
  exception Wrong
  exception TopLevel of Value.t

  let eval_error fmt = 
    let error msg = raise (EvaluationError msg) in
      Printf.kprintf error fmt

  let db_connect : Value.t -> Result.database * string = fun db ->
    let driver = Value.unbox_string (Value.project "driver" db)
    and name = Value.unbox_string (Value.project "name" db)
    and args = Value.unbox_string (Value.project "args" db) in
    let params =
      (if args = "" then name
       else name ^ ":" ^ args)
    in
      Result.db_connect driver params

  let client_call : string -> Value.continuation -> Value.t list -> 'a =
    fun _ _ _ -> assert false

  let apply_prim : string -> Value.t list -> Value.t =
    fun _ _ -> assert false


  
  module Q =
  struct
    (** Substitutes values for the variables in a query, and performs
        interpolation in LIKE expressions. *)
    let rec normalise_query (env:Value.env) (db:Result.database) 
        (qry:SqlQuery.sqlQuery) : SqlQuery.sqlQuery =

      let normalise_like_expression (l : SqlQuery.like_expr): SqlQuery.like_expr = 
        let quote = Str.global_replace (Str.regexp_string "%") "\\%" in
        let rec nle =
          function
            | `Var x -> `Str (quote (Value.unbox_string (IntMap.find (int_of_string x) env)))
            | (`Percent | `Str _) as l -> l
            | `Seq ls -> `Seq (List.map nle ls)
        in
          nle l
      in
      let rec normalise_expression : SqlQuery.sqlexpr -> SqlQuery.sqlexpr = function
        | `V name -> begin
            try
              match IntMap.find (int_of_string name) env with
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
                   (match IntMap.find (int_of_string var) env with
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
      let get_database : SqlQuery.sqlQuery -> Result.database = fun query ->
        let vars = concat_map (function
                                 | `TableVar (var, _) -> [int_of_string var]
                                 | _ -> []) query.SqlQuery.tabs in
        let dbs = 
          List.map (fun var -> 
                 match IntMap.find var env with
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
        let result = Database.execute_select result_types query_string db in
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
    | `Variable v ->
        (match Value.lookup v env with
           | Some v -> v
           | _      -> eval_error "Variable not found: %d" v)
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
    | `Inject (label, v) -> `Variant (label, value env v)
    | `TApp (v, _) -> value env v
    | `XmlNode (tag,attrs,children) -> 
        let children = 
          List.fold_right 
            (fun v cs -> 
               match value env v with
                 | `XML item -> item :: cs
                 | _ -> eval_error "XML child is not XML")
            children [] in
        let children = (StringMap.fold 
                          (fun name v attrs ->
                             Value.Attr (name, Value.unbox_string (value env v)) :: attrs)
                          attrs children) in
          `XML (Value.Node (tag, children))
    | `ApplyPure (_, args) ->
        (assert false) (*Library.apply_pfun*) (assert false) (List.map (value env) args)
    | `Coerce (v, _) -> value env v
    | `Abs v         -> `Abs (value env v)
    (* Unnecessary *)
    | `Comparison _  -> assert false

  let rec apply cont env : Value.t * Value.t list -> Value.t = function
    | `RecFunction (recs, fnenv, n), ps -> 
        (match lookup n recs with
           | Some (args, body) -> 
               (* unfold recursive definitions once *)
               let locals = 
                 List.fold_right 
                   (fun (name, _) env ->
                      Value.bind name (`RecFunction (recs, env, name)) env)
                   recs fnenv in
               let env = Value.shadow env ~by:locals in
                 (* bind arguments *)
               let env = List.fold_right2 Value.bind args ps env in
                 computation env cont body
           | None -> eval_error "Error looking up recursive function definition")
    | `PrimitiveFunction n, ps -> apply_prim n ps
    | `ClientFunction name, ps -> client_call name cont ps
    | `Continuation c,     [p] -> apply_cont c env p
    | `Continuation _,      _  ->
        eval_error "Continuation applied to multiple (or zero) arguments"
    | `Abs f,               ps -> 
        let argument = 
          `Record (List.map2
                     (fun field n -> string_of_int n, field)
                     ps (Utility.fromTo 1 (1 + List.length ps))) in
          apply cont env (f, [argument])
    | _                        -> eval_error "Application of non-function"
  and apply_cont cont env v : Value.t =
    match cont with
      | [] when !Library.current_pid == Library.main_process_pid ->
          raise (TopLevel v)
      | [] -> switch_context env
      | (var, locals, comp)::cont ->
          let env = Value.bind var v (Value.shadow env ~by:locals) in
            computation env cont comp
  and computation env cont (binders, tailcomp) : Value.t =
    match binders with
      | [] -> tail_computation env cont tailcomp
      | b::bs -> match b with
          | `Let ((_, (var,_)), tc) ->
              tail_computation env (((var, env, (bs, tailcomp))::cont) : Value.continuation) tc
          | `Fun ((_, (name,_)), args, body, _) -> 
              tail_computation (Value.bind name (`RecFunction ([name, (List.map fst args,body)], 
                                                             env, name)) env) cont tailcomp
          | `Rec fs         -> 
              let bindings = List.map (fun ((_, (name,_)), args, body, _) ->
                                         name, (List.map fst args, body)) fs in
              let env = 
                List.fold_right (fun (name,_) env ->
                                   Value.bind name 
                                     (`RecFunction (bindings, env, name))
                                     env) bindings env in
                tail_computation env cont tailcomp
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
                      computation (Value.bind var v env) cont c
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
    | `App (f, p)                 -> apply cont env (value env f,
                                                     Value.untuple (value env p))
    | `Table (db, name, (readtype, _)) -> 
        (match value env db, value env name, readtype with
           | `Database (db, params), name, `Record row ->
               `Table ((db, params), Value.unbox_string name, row)
           | _ -> eval_error "Error evaluating table handle")
    | `CallCC f                   -> 
        apply cont env (value env f, [`Continuation cont])
  let eval : Value.env -> program -> Value.t = 
    fun env -> computation env (assert false : Value.continuation)
end
