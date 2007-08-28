module Eval = struct
  open Ir
  open Utility

  exception EvaluationError of string
  exception Wrong
  exception TopLevel of Value.t


  let eval_error fmt = 
    let error msg = raise (EvaluationError msg) in
      Printf.kprintf error fmt


  let box_string : string -> Value.t
    = fun _ -> assert false
  and unbox_string : Value.t -> string
    = fun _ -> assert false

  let db_connect : Value.t -> Result.database * string
    = fun _ -> assert false

  let client_call : string -> Value.continuation -> Value.t list -> 'a =
    fun _ _ _ -> assert false

  let apply_prim : string -> Value.t list -> Value.t =
    fun _ _ -> assert false

  let untuple : Value.t -> Value.t list =
    fun _ -> assert false

  let do_query : Value.env -> SqlQuery.sqlQuery -> Value.t StringMap.t -> Value.t 
    = fun _ _ _ -> assert false

  let switch_context _ = 
    assert false

  let rec value env : Ir.value -> Value.t = function
    | `Constant Syntax.Boolean b -> `Bool b
    | `Constant Syntax.Integer n -> `Int n
    | `Constant Syntax.Char c -> `Char c
    | `Constant Syntax.String s -> box_string s
    | `Constant Syntax.Float f -> `Float f
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
                             Result.Attr (name, unbox_string (value env v)) :: attrs)
                          attrs children) in
          `XML (Result.Node (tag, children))
    | `ApplyPrim (_, args) ->
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
          | `Let ((var,_), tc) ->
              tail_computation env (((var, env, (bs, tailcomp))::cont) : Value.continuation) tc
          | `Fun ((name,_), args, body, _) -> 
              tail_computation (Value.bind name (`RecFunction ([name, (List.map fst args,body)], 
                                                             env, name)) env) cont tailcomp
          | `Rec fs         -> 
              let bindings = List.map (fun ((name,_), args, body, _) ->
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
    | `Wrong                      -> raise Wrong
    | `Database v                 -> `Database (db_connect (value env v))
    | `TableQuery (map, q)        -> 
          do_query env q (StringMap.map (value env) map)
    | `App (f, p)                 -> apply cont env (value env f,
                                                     untuple (value env p))
    | `TableHandle (db, name, (readtype, _)) -> 
        (match value env db, value env name, readtype with
           | `Database (db, params), name, `Record row ->
               `Table ((db, params), unbox_string name, row)
           | _ -> eval_error "Error evaluating table handle")
    | `CallCC f                   -> 
        apply cont env (value env f, [`Continuation cont])
  let eval : Value.env -> program -> Value.t = 
    fun env -> computation env (assert false : Value.continuation)
end
