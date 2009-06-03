open Notfound

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

   let serialize_call_to_client (continuation, name, arg) = 
     Json.jsonize_call continuation name arg
       
   let client_call : string -> Value.continuation -> Value.t list -> 'a =
     fun name cont args ->
       if not(Settings.get_value Basicsettings.web_mode) then
         failwith "Can't make client call outside web mode.";
       if not(Proc.singlethreaded()) then
         failwith "Remaining procs on server at client call!";
       let call_package = Utility.base64encode (serialize_call_to_client 
                                                  (cont, name, args)) in
         Lib.print_http_response ["Content-type", "text/plain"] call_package;
         exit 0
      
  let apply_prim : string -> Value.t list -> Value.t = Lib.apply_pfun

  (** {0 Scheduling} *)

  (** {1 Scheduler parameters} *)
  (** [switch_granularity]: The number of steps to take before 
      switching threads.  *)
  let switch_granularity = 5

  let atomic = ref false (* Unexplained hack of sjl. *)

  let rec switch_context env = 
    match Proc.pop_ready_proc() with
        Some((cont, value), pid) -> (
          Proc.activate pid;
          apply_cont cont env value)
      | None -> exit 0
          
  and scheduler env state stepf = 
(*     if Proc.singlethreaded() then stepf() else (* No need to schedule if
                                                     there are no threads...*)*)
    let step_ctr = Proc.count_step() in
      if step_ctr mod switch_granularity == 0 then
        begin
          Proc.reset_step_counter();
          Proc.suspend_current state;
          switch_context env
        end
      else
        stepf()

  (** {0 Evaluation} *)
  and value env : Ir.value -> Value.t = function
    | `Constant `Bool b -> `Bool b
    | `Constant `Int n -> `Int n
    | `Constant `Char c -> `Char c
    | `Constant `String s -> Value.box_string s
    | `Constant `Float f -> `Float f
    | `Variable var ->
        begin
          match lookup_var var env with
            | Some v -> v
            | _      -> eval_error "Variable not found: %d" var
        end
    | `Extend (fields, r) -> 
        begin
          match opt_app (value env) (`Record []) r with
            | `Record fs ->
                (* HACK

                   Pre-pending the fields to r in this order shouldn't
                   be necessary but without the List.rev, deriving
                   somehow manages to serialise things in the wrong
                   order on the "Your Shopping Cart" page of the
                   winestore example. *)
                `Record (List.rev
                           (StringMap.fold 
                              (fun label v fs ->
                                 if List.mem_assoc label fs then
                                   (* (label, value env v) :: (List.remove_assoc label fs) *)
                                   eval_error
                                     "Error adding fields: label %s already present" label
                                 else
                                   (label, value env v)::fs)
                              fields
                              []) @ fs)
(*                 `Record (StringMap.fold  *)
(*                            (fun label v fs -> *)
(*                               (label, value env v)::fs) *)
(*                            fields *)
(*                            fs) *)
            | _ -> eval_error "Error adding fields: non-record"
        end
    | `Project (label, r) ->
        begin
          match value env r with
            | `Record fields when List.mem_assoc label fields ->
                List.assoc label fields
            | _ -> eval_error "Error projecting label %s" label
        end
    | `Erase (labels, r) ->
        begin
          match value env r with
            | `Record fields when
                StringSet.for_all (fun label -> List.mem_assoc label fields) labels ->
                `Record (StringSet.fold (fun label fields -> List.remove_assoc label fields) labels fields)
            | _ -> eval_error "Error erasing labels {%s}" (String.concat "," (StringSet.elements labels))
        end
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
            atomic := true;
            ignore (apply [] env (value env f, List.map (value env) args));
            failwith "boom"
          ) with
            | TopLevel (_, v) -> atomic := false; v
        end
    | `Coerce (v, t) -> value env v

  and apply cont env : Value.t * Value.t list -> Value.t = function
    | `RecFunction (recs, locals, n, scope), ps -> 
        begin match lookup n recs with
          | Some (args, body) ->
              (* unfold recursive definitions once *)
              
              (* extend env with locals *)
              let env = Value.shadow env ~by:locals in
                
              (* extend env with recs *)
              let env =
                List.fold_right
                  (fun (name, _) env ->
                     Value.bind name (`RecFunction (recs, locals, name, scope), scope) env)
                  recs env in
                
              (* extend env with arguments *)
              let env = List.fold_right2 (fun arg p -> Value.bind arg (p, `Local)) args ps env in
                computation env cont body
          | None -> eval_error "Error looking up recursive function definition"
        end
    | `PrimitiveFunction "recv", [] ->
        (* If there are any messages, take the first one and
           apply the continuation to it.  Otherwise, suspend
           the continuation (in the blocked_processes table)
           and let the scheduler choose a different thread.
        *)
        begin match Proc.pop_message() with
            Some message -> apply_cont cont env message
          | None -> 
              let recv = Value.tailcomp_to_continuation env 
                           (Lib.prim_appln "recv" [])
              in Proc.block_current (recv@cont, `Record []);
                switch_context env
        end
    | `PrimitiveFunction n, args -> apply_cont cont env (apply_prim n args)
    | `ClientFunction name, args -> client_call name cont args
    | `Continuation c,      [p] -> apply_cont c env p
    | `Continuation _,       _  ->
        eval_error "Continuation applied to multiple (or zero) arguments"
    | _                        -> eval_error "Application of non-function"
  and apply_cont cont env v : Value.t =
    let stepf() = 
      match cont with
        | [] when !atomic || Proc.current_is_main() ->
            raise (TopLevel (Value.globals env, v))
        | [] -> switch_context env
        | (scope, var, locals, comp)::cont ->
            let env = Value.bind var (v, scope) (Value.shadow env ~by:locals) in
              computation env cont comp
    in
      scheduler env (cont, v) stepf

  and computation env cont (binders, tailcomp) : Value.t =
    match binders with
      | [] -> tail_computation env cont tailcomp
      | b::bs -> match b with
          | `Let ((var, _) as b, (_, tc)) ->
              let locals = Value.localise env var in
              let cont' = (((Var.scope_of_binder b, var, locals, (bs, tailcomp))
                           ::cont) : Value.continuation) in
                tail_computation env cont' tc
          | `Fun ((f, _) as fb, (_, args, body), `Client) ->
              let env' = Value.bind f (`ClientFunction (Var.name_of_binder fb),
                                       Var.scope_of_binder fb) env in
                computation env' cont (bs, tailcomp)
          | `Fun ((f, _) as fb, (_, args, body), _) -> 
              let scope = Var.scope_of_binder fb in
              let locals = Value.localise env f in
              let env' = 
                Value.bind f
                  (`RecFunction ([f, (List.map fst args, body)], 
                                 locals, f, scope), scope) env
              in
                computation env' cont (bs, tailcomp)
          | `Rec defs ->
              (* partition the defs into client defs and non-client defs *)
              let client_defs, defs =
                List.partition (function
                                  | (_fb, _lam, (`Client | `Native)) -> true
                                  | _ -> false) defs in
              
              let locals =
                match defs with
                  | [] -> Value.empty_env (Value.get_closures env)
                  | ((f, _), _, _)::_ -> Value.localise env f in

              (* add the client defs to the environments *)
              let env =
                List.fold_left
                  (fun env ((f, _) as fb, _lam, _location) ->
                     let v = `ClientFunction (Var.name_of_binder fb), Var.scope_of_binder fb in
                       Value.bind f v env)
                  env client_defs in

              (* add the server defs to the environment *)
              let bindings = List.map (fun ((f,_), (_, args, body), _) ->
                                         f, (List.map fst args, body)) defs in
              let env =
                List.fold_right
                  (fun ((f, _) as fb, _, _) env ->
                     let scope = Var.scope_of_binder fb in
                       Value.bind f
                         (`RecFunction (bindings, locals, f, scope),
                          scope)
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
    | `Database v                 -> apply_cont cont env (`Database (db_connect (value env v)))
    | `Table (db, name, (readtype, _, _)) -> 
        (match value env db, value env name, readtype with
           | `Database (db, params), name, `Record row ->
               apply_cont cont env (`Table ((db, params), Value.unbox_string name, row))
           | _ -> eval_error "Error evaluating table handle")
    | `Query (range, e, _t) ->
        let range =
          match range with
            | None -> None
            | Some (limit, offset) ->
                Some (Value.unbox_int (value env limit), Value.unbox_int (value env offset)) in
        let result =
          match Query.compile env (range, e) with
            | None -> computation env cont e
            | Some (db, q, t) ->
                let (fieldMap, _), _ = 
                  Types.unwrap_row(TypeUtils.extract_row t) in
                let fields =
                    StringMap.fold
                      (fun name t fields ->
                         match t with
                           | `Present, t -> (name, t)::fields
                           | `Absent, _ -> assert false
                           | `Var _, t -> assert false)
                      fieldMap
                      []
                in
                  Database.execute_select fields q db
        in
          apply_cont cont env result
    | `Update ((xb, source), where, body) ->
        let db, table, read_labels =
          match value env source with
            | `Table ((db, _), table, (fields, _)) ->
                let read_labels =
                  StringMap.fold
                    (fun label _ labels -> StringSet.add label labels)
                    fields
                    StringSet.empty
                in
                  db, table, read_labels
            | _ -> assert false in
        let update_query =
          Query.compile_update env ((Var.var_of_binder xb, table, read_labels), where, body) in
        let () = ignore (Database.execute_command update_query db) in
          apply_cont cont env (`Record [])
    | `Delete ((xb, source), where) ->
        let db, table, read_labels =
          match value env source with
            | `Table ((db, _), table, (fields, _)) ->
                let read_labels =
                  StringMap.fold
                    (fun label _ labels -> StringSet.add label labels)
                    fields
                    StringSet.empty
                in
                  db, table, read_labels
            | _ -> assert false in
        let delete_query =
          Query.compile_delete env ((Var.var_of_binder xb, table, read_labels), where) in
        let () = ignore (Database.execute_command delete_query db) in
          apply_cont cont env (`Record [])
    | `CallCC f                   -> 
        apply cont env (value env f, [`Continuation cont])
  let eval : Value.env -> program -> Value.t = 
    fun env -> computation env Value.toplevel_cont
end

let run_program_with_cont : Value.continuation -> Value.env -> Ir.program ->
                            (Value.env * Value.t) =
  fun cont env program ->
    try (
      ignore 
        (Eval.computation env cont program);
      failwith "boom"
    ) with
      | Eval.TopLevel (env, v) -> (env, v)
      | NotFound s -> failwith ("Internal error: NotFound " ^ s ^ 
                                  " while interpreting.")

let run_program : Value.env -> Ir.program -> (Value.env * Value.t) =
  fun env program ->
    try (
      ignore 
        (Eval.eval env program);
      failwith "boom"
    ) with
      | Eval.TopLevel (env, v) -> (env, v)
      | NotFound s -> failwith ("Internal error: NotFound " ^ s ^ 
                                  " while interpreting.")

let run_defs : Value.env -> Ir.binding list -> Value.env =
  fun env bs ->
    let env, _value = 
      run_program env (bs, `Return(`Extend(StringMap.empty, None))) in
      env

(** [apply_cont_toplevel cont env v] applies a continuation to a value
    and returns the result. Finishing the main thread normally comes
    here immediately. *)
let apply_cont_toplevel cont env v = 
  try Eval.apply_cont cont env v
  with
    | Eval.TopLevel s -> snd s
    | NotFound s -> failwith ("Internal error: NotFound " ^ s ^ 
                                " while interpreting.")

let apply_toplevel env (f, vs) =
  try Eval.apply [] env (f, vs)
  with
    | Eval.TopLevel s -> snd s
    | NotFound s -> failwith ("Internal error: NotFound " ^ s ^ 
                                " while interpreting.")
