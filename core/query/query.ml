open Utility
open CommonTypes
open Var

module Q = QueryLang

let internal_error message =
  Errors.internal_error ~filename:"query/query.ml" ~message

let rec tail_of_t : Q.t -> Q.t = fun v ->
  let tt = tail_of_t in
    match v with
      | Q.For (_, _gs, _os, Q.Singleton (Q.Record fields)) -> Q.Record fields
      | Q.For (_tag, _gs, _os, Q.If (_, t, Q.Concat [])) -> tt (Q.For (_tag, _gs, _os, t))
      | _ -> (* Debug.print ("v: "^string_of_t v); *) assert false

let rec freshen_for_bindings : Var.var Env.Int.t -> Q.t -> Q.t =
  fun env v ->
    let ffb = freshen_for_bindings env in
      match v with
      | Q.For (tag, gs, os, b) ->
        let gs', env' =
          List.fold_left
            (fun (gs', env') (x, source) ->
              let y = Var.fresh_raw_var () in
                ((y, ffb source)::gs', Env.Int.bind x y env'))
            ([], env)
            gs
        in
        Q.For (tag, List.rev gs', List.map (freshen_for_bindings env') os, freshen_for_bindings env' b)
      | Q.If (c, t, e) -> Q.If (ffb c, ffb t, ffb e)
      | Q.Table _ as t -> t
      | Q.Singleton v -> Q.Singleton (ffb v)
      | Q.Database db -> Q.Database db
      | Q.Concat vs -> Q.Concat (List.map ffb vs)
      | Q.Dedup t -> Q.Dedup (ffb t)
      | Q.Prom t -> Q.Prom (ffb t)
      | Q.Record fields -> Q.Record (StringMap.map ffb fields)
      | Q.Variant (name, v) -> Q.Variant (name, ffb v)
      | Q.XML xmlitem -> Q.XML xmlitem
      | Q.Project (v, name) -> Q.Project (ffb v, name)
      | Q.Erase (v, names) -> Q.Erase (ffb v, names)
      | Q.Apply (u, vs) -> Q.Apply (ffb u, List.map ffb vs)
      | Q.Closure _ as c ->
        (* we don't attempt to freshen closure bindings *)
        c
      | Q.Case (u, cl, d) -> Q.Case (ffb u, StringMap.map (fun (x,y) -> (x, ffb y)) cl, opt_app (fun (x,y) -> Some (x, ffb y)) None d)
      | Q.Primitive f -> Q.Primitive f
      | Q.Var (x, ts) as v ->
        begin
          match Env.Int.find_opt x env with
          | None -> v (* Var (x, ts) *)
          | Some y -> Q.Var (y, ts)
        end
      | Q.Constant c -> Q.Constant c

(* simple optimisations *)
let reduce_and (a, b) =
  match a, b with
    | Q.Constant (Constant.Bool true), x
    | x, Q.Constant (Constant.Bool true)
    | (Q.Constant (Constant.Bool false) as x), _
    | _, (Q.Constant (Constant.Bool false) as x) -> x
    | _ -> Q.Apply  (Q.Primitive "&&", [a; b])

let reduce_or (a, b) =
  match a, b with
    | (Q.Constant (Constant.Bool true) as x), _
    | _, (Q.Constant (Constant.Bool true) as x)
    | Q.Constant (Constant.Bool false), x
    | x, Q.Constant (Constant.Bool false) -> x
    | _ -> Q.Apply  (Q.Primitive "||", [a; b])

let reduce_not a =
  match a with
    | Q.Constant (Constant.Bool false) -> Q.Constant (Constant.Bool true)
    | Q.Constant (Constant.Bool true)  -> Q.Constant (Constant.Bool false)
    | _                                -> Q.Apply  (Q.Primitive "not", [a])

let rec reduce_eq (a, b) =
  let bool x = Q.Constant (Constant.Bool x) in
  let eq_constant =
    function
      | (Constant.Bool a  , Constant.Bool b)   -> bool (a = b)
      | (Constant.Int a   , Constant.Int b)    -> bool (a = b)
      | (Constant.Float a , Constant.Float b)  -> bool (a = b)
      | (Constant.Char a  , Constant.Char b)   -> bool (a = b)
      | (Constant.String a, Constant.String b) -> bool (a = b)
      | (a, b)                 -> Q.Apply (Q.Primitive "==", [Q.Constant a; Q.Constant b])
  in
    match a, b with
      | (Q.Constant a, Q.Constant b) -> eq_constant (a, b)
      | (Q.Variant (s1, a), Q.Variant (s2, b)) ->
        if s1 <> s2 then
          Q.Constant (Constant.Bool false)
        else
          reduce_eq (a, b)
      | (Q.Record lfields, Q.Record rfields) ->
        List.fold_right2
          (fun (_, v1) (_, v2) e ->
            reduce_and (reduce_eq (v1, v2), e))
          (StringMap.to_alist lfields)
          (StringMap.to_alist rfields)
          (Q.Constant (Constant.Bool true))
      | (a, b) -> Q.Apply (Q.Primitive "==", [a; b])

let reduce_concat vs =
  let vs =
    concat_map
      (function
        | Q.Concat vs -> vs
        | v -> [v])
      vs
  in
    match vs with
      | [v] -> v
      | vs -> Q.Concat vs

let rec reduce_where_then (c, t) =
  match t with
    (* optimisation *)
    | Q.Constant (Constant.Bool true) -> t
    | Q.Constant (Constant.Bool false) -> Q.Concat []

    | Q.Concat vs ->
      reduce_concat (List.map (fun v -> reduce_where_then (c, v)) vs)
    | Q.For (_, gs, os, body) ->
      Q.For (None, gs, os, reduce_where_then (c, body))
    | Q.If (c', t', Q.Concat []) ->
      reduce_where_then (reduce_and (c, c'), t')
    | _ ->
      Q.If (c, t, Q.Concat [])

let reduce_for_body (gs, os, body) =
  match body with
    | Q.For (_, gs', os', body') -> Q.For (None, gs @ gs', os @ os', body')
    | _                          -> Q.For (None, gs, os, body)

let rec reduce_for_source : Q.t * (Q.t -> Q.t) -> Q.t =
  fun (source, body) ->
    let rs = fun source -> reduce_for_source (source, body) in
      match source with
        | Q.Singleton v -> body v
        | Q.Concat vs ->
          reduce_concat (List.map rs vs)
        | Q.If (c, t, Q.Concat []) ->
          reduce_for_source
            (t, fun v -> reduce_where_then (c, body v))
        | Q.For (_, gs, os, v) ->
          (* NOTE:

              We are relying on peculiarities of the way we manage
              the environment in order to avoid having to
              augment it with the generator bindings here.

              In particular, we rely on the fact that if a variable
              is not found on a lookup then we return the eta
              expansion of that variable rather than complaining that
              it isn't bound in the environment.

          *)
          reduce_for_body (gs, os, rs v)
        | Q.Table (_,_,_, row) ->
          (* we need to generate a fresh variable in order to
              correctly handle self joins *)
          let x = Var.fresh_raw_var () in
          let ty_elem = Types.Record (Types.Row row) in
            reduce_for_body ([(x, source)], [], body (Q.Var (x, ty_elem)))
      | v -> Q.query_error "Bad source in for comprehension: %s" (Q.string_of_t v)

let rec reduce_if_body (c, t, e) =
  match t with
    | Q.Record then_fields ->
      begin match e with
        | Q.Record else_fields ->
          assert (StringMap.equal (fun _ _ -> true) then_fields else_fields);
          Q.Record
            (StringMap.fold
                (fun name t fields ->
                  let e = StringMap.find name else_fields in
                    StringMap.add name (reduce_if_body (c, t, e)) fields)
                then_fields
                StringMap.empty)
        (* NOTE: this relies on any record variables having
            been eta-expanded by this point *)
        | _ -> Q.query_error "Mismatched fields"
      end
    | _ ->
      begin
        match t, e with
          | Q.Constant (Constant.Bool true), _ ->
            reduce_or (c, e)
          | _, Q.Constant (Constant.Bool false) ->
            reduce_and (c, t)
          | _ ->
            Q.If (c, t, e)
      end

let reduce_if_condition (c, t, e) =
  match c with
    | Q.Constant (Constant.Bool true) -> t
    | Q.Constant (Constant.Bool false) -> e
    | Q.If (c', t', _) ->
      reduce_if_body
        (reduce_or (reduce_and (c', t'),
                    reduce_and (reduce_not c', t')),
          t,
          e)
    | _ ->
      if Q.is_list t then
        if e = Q.nil then
          reduce_where_then (c, t)
        else
          reduce_concat [reduce_where_then (c, t);
                          reduce_where_then (reduce_not c, e)]
      else
        reduce_if_body (c, t, e)

module Eval =
struct
  let env_of_value_env policy value_env =
    let open Q in
    { venv = value_env; qenv = Env.Int.empty; policy }

  let empty_env policy =
    let open Q in
    { venv = Value.Env.empty; qenv = Env.Int.empty; policy }

  let (++) e1 e2 =
    let open Q in
    if (e1.policy <> e2.policy) then
      raise (internal_error "Trying to append environments with different query policies")
    else
      let venv = Value.Env.shadow e1.venv ~by:e2.venv in
      let qenv = Env.Int.extend e1.qenv e2.qenv in
      { policy = e1.policy; venv; qenv }

  let lookup_fun env (f, fvs) =
    let open Q in
    match Tables.lookup Tables.fun_defs f with
    | Some (finfo, (xs, body), z, location) ->
      Some
        begin
          (* TODO(dhil): This is a bit of a round-about way to obtain
             the binder name. *)
        match Var.(name_of_binder (make_binder f finfo)) with
        | "concatMap" ->
          Q.Primitive "ConcatMap"
        | "map" ->
          Q.Primitive "Map"
        | "empty" ->
          Q.Primitive "Empty"
        | "sortByBase" ->
          Q.Primitive "SortBy"
        | _ ->
          begin
            match location with
            | Location.Server | Location.Unknown ->
                let env' =
                  match z, fvs with
                  | None, None       -> Value.Env.empty
                  | Some z, Some fvs -> Value.Env.bind z (fvs, Scope.Local) Value.Env.empty
                  | _, _ -> assert false in
                Closure ((xs, body), env_of_value_env env.policy env')
            | Location.Client ->
              raise (Errors.runtime_error ("Attempt to use client function: " ^
                Js.var_name_binder (Var.make_binder f finfo) ^ " in query"))
          end
      end
    | None -> None

  let find_fun env (f, fvs) =
    match lookup_fun env (f, fvs) with
    | Some v -> v
    | None ->
      raise (internal_error ("Attempt to find undefined function: " ^
        string_of_int f))

  let rec expression_of_value : Q.env -> Value.t -> Q.t = fun env v ->
    let open Q in
    match v with
      | `Bool b   -> Constant (Constant.Bool b)
      | `Int i    -> Constant (Constant.Int i)
      | `Char c   -> Constant (Constant.Char c)
      | `Float f  -> Constant (Constant.Float f)
      | `String s -> Constant (Constant.String s)
      | `Table t -> Table t
      | `Database db -> Database db
      | `List vs ->
          Concat (List.map (fun v -> Singleton (expression_of_value env v)) vs)
      | `Record fields ->
          Q.Record
            (List.fold_left
               (fun fields (name, v) -> StringMap.add name (expression_of_value env v) fields)
               StringMap.empty
               fields)
      | `Variant (name, v) -> Variant (name, expression_of_value env v)
      | `XML xmlitem -> XML xmlitem
      | `FunctionPtr (f, fvs) -> find_fun env (f, fvs)
      | `PrimitiveFunction (f,_) -> Primitive f
      | v ->
          raise (internal_error (Printf.sprintf
              "Cannot convert value %s to expression" (Value.string_of_value v)))

  let lookup env var =
    let open Q in
    let val_env = env.venv in
    let exp_env = env.qenv in
    match lookup_fun env (var, None) with
    | Some v -> v
    | None ->
      begin
        match Value.Env.lookup var val_env, Env.Int.find_opt var exp_env with
        | None, Some v -> v
        | Some v, None -> expression_of_value env v
        | Some _, Some v -> v (*query_error "Variable %d bound twice" var*)
        | None, None ->
          begin
            try expression_of_value env (Lib.primitive_stub (Lib.primitive_name var)) with
            | NotFound _ ->
                raise (internal_error ("Variable " ^ string_of_int var ^ " not found"));
          end
      end

  let reduce_artifacts = function
  | Q.Apply (Q.Primitive "stringToXml", [u]) ->
    Q.Singleton (Q.XML (Value.Text (Q.unbox_string u)))
  | Q.Apply (Q.Primitive "AsList", [xs]) -> xs
  | u -> u

  let check_policies_compatible env_policy block_policy =
    if env_policy != block_policy
    then
      let error = Printf.sprintf
          "Incompatible query evaluation annotations. Expected %s, got %s."
          (QueryPolicy.show env_policy) (QueryPolicy.show block_policy) in
      raise (Errors.runtime_error error)

  let rec xlate env : Ir.value -> Q.t = let open Ir in function
    | Constant c -> Q.Constant c
    | Variable var ->
        begin
          match lookup env var with
            | Q.Var (x, field_types) ->
                (* eta-expand record variables *)
                Q.eta_expand_var (x, field_types)
            | Q.Primitive "Nil" -> Q.nil
            (* We could consider detecting and eta-expand tables here.
               The only other possible sources of table values would
               be `Special or built-in functions that return table
               values. (Currently there are no pure built-in functions
               that return table values.)

               Currently eta-expansion happens later, in the SQL
               module.

               On second thoughts, we *never* need to explicitly
               eta-expand tables, as it is not possible to call
               "AsList" directly. The "asList" function in the prelude
               is defined as:

               fun asList(t) server {for (x <-- t) [x]}
            *)
            | v ->
              (* In order to maintain the invariant that each
                 bound variable is unique we freshen all for-bound
                 variables in v here.

                 This is necessary in order to ensure that each
                 instance of a table in a self-join is given a
                 distinct alias, as the alias is generated from the
                 name of the variable binding the table.

                 We are assuming that any closure-bound variables will
                 be eliminated anyway.
              *)
              (* Debug.print ("env v: "^string_of_int var^" = "^string_of_t v); *)
              freshen_for_bindings (Env.Int.empty) v
        end
    | Extend (ext_fields, r) ->
      begin
        match opt_app (xlate env) (Q.Record StringMap.empty) r with
          | Q.Record fields ->
            Q.Record (StringMap.fold
                       (fun label v fields ->
                         if StringMap.mem label fields then
                           Q.query_error
                             "Error adding fields: label %s already present"
                             label
                         else
                           StringMap.add label (xlate env v) fields)
                       ext_fields
                       fields)
          | _ -> Q.query_error "Error adding fields: non-record"
      end
    | Project (label, r) -> Q.Project (xlate env r, label)
    | Erase (labels, r) -> Q.Erase (xlate env r, labels)
    | Inject (label, v, _) -> Q.Variant (label, xlate env v)
    | TAbs (_, v) -> xlate env v
    | TApp (v, _) -> xlate env v

    | XmlNode (tag, attrs, children) ->
        (* TODO: deal with variables in XML *)
        let children =
          List.fold_right
            (fun v children ->
               let v = xlate env v in
               List.map Q.unbox_xml (Q.unbox_list v) @ children)
            children [] in
        let children =
          StringMap.fold
            (fun name v attrs ->
               Value.Attr (name, Q.unbox_string (xlate env v)) :: attrs)
            attrs children
        in
          Q.Singleton (Q.XML (Value.Node (tag, children)))

    | ApplyPure (f, ps) ->
        reduce_artifacts (Q.Apply (xlate env f, List.map (xlate env) ps))
    | Closure (f, _, v) ->
      let open Q in
      let (_finfo, (xs, body), z_opt, _location) = Tables.find Tables.fun_defs f in
      let z = OptionUtils.val_of z_opt in
      (* Debug.print ("Converting evalir closure: " ^ Var.show_binder (f, _finfo) ^ " to query closure"); *)
      (* yuck! *)
      let env' = Q.bind (empty_env env.policy) (z, xlate env v) in
      Q.Closure ((xs, body), env')
      (* (\* Debug.print("looking up query closure: "^string_of_int f); *\) *)
      (* begin *)
      (*   match value env (`Variable f) with *)
      (*   | `Closure ((z::xs, body), closure_env) -> *)
      (*     (\* Debug.print("binding query closure parameter: "^string_of_int z); *\) *)
      (*     (\* partially apply the closure to bind the closure *)
      (*        environment *\) *)
      (*     `Closure ((xs, body), bind closure_env (z, value env v)) *)
      (*   | _ -> *)
      (*     failwith "ill-formed closure in query compilation" *)
      (* end *)
    | Coerce (v, _) -> xlate env v

  and computation env (binders, tailcomp) : Q.t =
    let open Ir in
    match binders with
      | [] -> tail_computation env tailcomp
      | b::bs ->
          begin
            match b with
              | Let (xb, (_, tc)) ->
                  let x = Var.var_of_binder xb in
                    computation (Q.bind env (x, tail_computation env tc)) (bs, tailcomp)
              | Fun {fn_location = Location.Client; _} ->
                  Q.query_error "Client function"
              | Fun {fn_binder = b; _} ->
                 let f = Var.var_of_binder b in
                 (* This should never happen now that we have closure conversion*)
                 raise (internal_error
                          ("Function definition in query: " ^ string_of_int f ^
                             ". This should have been closure-converted."))
              | Rec _ ->
                  Q.query_error "Recursive function"
              | Alien _ -> (* just skip it *)
                  computation env (bs, tailcomp)
              | Module _ -> raise (internal_error "Not implemented modules yet")
          end
  and tail_computation env : Ir.tail_computation -> Q.t = let open Ir in function
    | Return v -> xlate env v
    | Apply (f, args) ->
        reduce_artifacts (Q.Apply (xlate env f, List.map (xlate env) args))
    | Special (Ir.Query (None, policy, e, _)) ->
        let open Q in
        check_policies_compatible env.policy policy;
        computation env e
    | Special (Ir.Table (db, name, keys, (readtype, _, _))) as _s ->
       (*  WR: this case is because shredding needs to access the keys of tables
           but can we avoid it (issue #432)? *)
       (* Copied almost verbatim from evalir.ml, which seems wrong, we should probably call into that. *)
       begin
         match xlate env db, xlate env name, xlate env keys, (TypeUtils.concrete_type readtype) with
         | Q.Database (db, params), name, keys, Types.Record (Types.Row row) ->
            let unboxed_keys =
              List.map
                (fun key ->
                  List.map Q.unbox_string (Q.unbox_list key))
                (Q.unbox_list keys)
            in
            Q.Table ((db, params), Q.unbox_string name, unboxed_keys, row)
         | _ -> Q.query_error "Error evaluating table handle"
       end
    | Special _s ->
      (* FIXME:

         There's no particular reason why we can't allow
         database declarations in query blocks. (However, we do still
         have the problem that we currently have no way of enforcing
         that only one database be used inside a query block - see
         SML#.)  *)
      raise (Errors.runtime_error "special not allowed in query block")
    | Case (v, cases, default) ->
        let v' = xlate env v in
        let cases' = StringMap.map (fun (x,y) -> (x, computation env y)) cases in
        let default' = opt_app (fun (x,y) -> Some (x, computation env y)) None default in
        Q.Case (v', cases', default')
    | If (c, t, e) ->
      let c = xlate env c in
      let t = computation env t in
      let e = computation env e in
        Q.If (c, t, e)

  let rec norm env : Q.t -> Q.t =
    function
    | Q.Record fl -> Q.Record (StringMap.map (norm env) fl)
    | Q.Concat xs -> reduce_concat (List.map (norm env) xs)
    | Q.Project (r, label) ->
      let rec project (r, label) =
        match r with
          | Q.Record fields ->
            assert (StringMap.mem label fields);
            StringMap.find label fields
          | Q.If (c, t, e) ->
            Q.If (c, project (t, label), project (e, label))
          | Q.Var (_x, Types.Record row) ->
            let field_types =  Q.field_types_of_row row in
            assert (StringMap.mem label field_types);
            Q.Project (r, label)
          | _ -> Q.query_error ("Error projecting from record: %s") (Q.string_of_t r)
      in
        project (norm env r, label)
    | Q.Erase (r, labels) ->
        let rec erase (r, labels) =
          match r with
          | Q.Record fields ->
            assert (StringSet.for_all
                      (fun label -> StringMap.mem label fields) labels);
            Q.Record
              (StringMap.fold
                 (fun label v fields ->
                   if StringSet.mem label labels then
                     fields
                   else
                     StringMap.add label v fields)
                 fields
                 StringMap.empty)
          | Q.If (c, t, e) ->
            Q.If (c, erase (t, labels), erase (e, labels))
          | Q.Var (_x, Types.Record row) ->
            let field_types = Q.field_types_of_row row in
            assert (StringSet.subset labels (Q.labels_of_field_types field_types));
            Q.Erase (r, labels)
          | _ -> Q.query_error "Error erasing from record"
      in
        erase (norm env r, labels)
    | Q.Variant (label, v) -> Q.Variant (label, norm env v)
    | Q.Apply (f, xs) -> apply env (norm env f, List.map (norm env) xs)
    | Q.If (c, t, e) ->
        reduce_if_condition (norm env c, norm env t, norm env e)
    | Q.Case (v, cases, default) ->
      let rec reduce_case (v, cases, default) =
        match v with
        | Q.Variant (label, v) as w ->
           begin
             match StringMap.lookup label cases, default with
             | Some (b, c), _ ->
                let x = Var.var_of_binder b in
                norm (Q.bind env (x, v)) c
             | None, Some (b, c) ->
                let z = Var.var_of_binder b in
                norm (Q.bind env (z, w)) c
             | None, None -> Q.query_error "Pattern matching failed"
           end
        | Q.If (c, t, e) ->
           Q.If
             (c,
              reduce_case (t, cases, default),
              reduce_case (e, cases, default))
        |  _ -> assert false
      in
      reduce_case (norm env v, cases, default)
    | v -> v

  and apply env : Q.t * Q.t list -> Q.t = function
    | Q.Closure ((xs, body), closure_env), args ->
      (* Debug.print ("Applying closure"); *)
      (* Debug.print ("body: " ^ Ir.show_computation body); *)
      (* Debug.print("Applying query closure: " ^ show_t (`Closure ((xs, body), closure_env))); *)
      (* Debug.print("args: " ^ mapstrcat ", " show_t args); *)
        let env = env ++ closure_env in
        let env = List.fold_right2 (fun x arg env ->
            Q.bind env (x, arg)) xs args env in
        (* Debug.print("Applied"); *)
          norm_comp env body
    | Q.Primitive "Cons", [x; xs] ->
        reduce_concat [Q.Singleton x; xs]
    | Q.Primitive "Concat", ([_xs; _ys] as l) ->
        reduce_concat l
    | Q.Primitive "ConcatMap", [f; xs] ->
        begin
          match f with
            | Q.Closure (([x], body), closure_env) ->
                let env = env ++ closure_env in
                  reduce_for_source
                    (xs, fun v -> norm_comp (Q.bind env (x, v)) body)
            | _ -> assert false
        end
    | Q.Primitive "Map", [f; xs] ->
        begin
          match f with
            | Q.Closure (([x], body), closure_env) ->
                let env = env ++ closure_env in
                  reduce_for_source
                    (xs, fun v -> Q.Singleton (norm_comp (Q.bind env (x, v)) body))
            | _ -> assert false
        end
    | Q.Primitive "SortBy", [f; xs] ->
        begin
          match xs with
            | Q.Concat [] -> Q.Concat []
            | _ ->
                let gs, os', body =
                  match xs with
                    | Q.For (_, gs, os', body) -> gs, os', body
                    | Q.Concat (_::_)
                    | Q.Singleton _
                    | Q.Table _ ->
                        (* I think we can omit the `Table case as it
                           can never occur *)
                        (* eta-expand *)
                        Q.eta_expand_list xs
                    | _ -> assert false in
                let xs = Q.For (None, gs, os', body) in
                  begin
                    match f with
                      | Q.Closure (([x], os), closure_env) ->
                          let os =
                            let env = Q.bind (env ++ closure_env) (x, tail_of_t xs) in
                              let o = norm_comp env os in
                                match o with
                                  | Q.Record fields ->
                                      List.rev (StringMap.fold (fun _ o os -> o::os) fields [])
                                  | _ -> assert false
                          in
                            Q.For (None, gs, os @ os', body)
                      | _ -> assert false
                  end
        end
    | Q.Primitive "not", [v] ->
      reduce_not (v)
    | Q.Primitive "&&", [v; w] ->
      reduce_and (v, w)
    | Q.Primitive "||", [v; w] ->
      reduce_or (v, w)
    | Q.Primitive "==", [v; w] ->
      reduce_eq (v, w)
    | Q.Primitive f, args ->
        Q.Apply (Q.Primitive f, args)
    | Q.If (c, t, e), args ->
        reduce_if_condition (c, apply env (t, args), apply env (e, args))
    | Q.Apply (f, args), args' ->
        apply env (f, args @ args')
    | t, _ -> Q.query_error "Application of non-function: %s" (Q.string_of_t t)

  and norm_comp env c = norm env (computation env c)

  let eval policy env e =
(*    Debug.print ("e: "^Ir.show_computation e); *)
    Debug.debug_time "Query.eval" (fun () ->
      norm_comp (env_of_value_env policy env) e)
end

(* convert a regexp to a like if possible *)
let rec likeify v =
  let open Q in
  let quote = Str.global_replace (Str.regexp_string "%") "\\%" in
  let append x y = Apply (Primitive "^^",  [x; y]) in
  let str x = Q.Constant (Constant.String x) in
    match v with
      | Variant ("Repeat", pair) ->
          begin
            match Q.unbox_pair pair with
              | Variant ("Star", _), Variant ("Any", _) ->
                  Some (str "%")
              | _ -> None
          end
      | Variant ("Simply", Constant (Constant.String s)) ->
          Some (str (quote s))
      | Variant ("Simply", Project (v, field)) ->
          Some (Project (v, field))
      | Variant ("Quote", Variant ("Simply", v)) ->
         let rec string =
            function
              | Constant (Constant.String s) -> Some (str (quote s))
              | Singleton (Constant (Constant.Char c)) ->
                  Some (str (string_of_char c))
              | Project (v, field) ->
                  Some (Project (v, field))
              | Apply (Primitive "intToString", [Constant (Constant.Int x)]) ->
                  Some (str (string_of_int x))
              | Concat vs ->
                  let rec concat =
                    function
                      | [] -> Some (str "")
                      | v::vs ->
                          begin
                            match string v with
                              | None -> None
                              | Some s ->
                                  begin
                                    match concat vs with
                                      | None -> None
                                      | Some s' ->
                                          Some (append s s')
                                  end
                          end
                  in
                    concat vs
              | _ -> None
          in
            string v
      | Variant ("Seq", rs) ->
          let rec seq =
            function
              | [] -> Some (str "")
              | r::rs ->
                  begin
                    match likeify r with
                      | None -> None
                      | Some s ->
                          begin
                            match seq rs with
                              | None -> None
                              | Some s' -> Some (append s s')
                          end
                  end
          in
            seq (unbox_list rs)
      | Variant ("StartAnchor", _) -> Some (str "")
      | Variant ("EndAnchor", _) -> Some (str "")
      | e ->
          Debug.print ("Could not likeify: " ^ (string_of_t e));
          assert false

let rec select_clause : Sql.index -> bool -> Q.t -> Sql.select_clause =
  fun index unit_query v ->
  (*  Debug.print ("select_clause: "^string_of_t v); *)
  let open Q in
  match v with
    | Concat _ -> assert false
    | For (_, [], _, body) ->
        select_clause index unit_query body
    | For (_, (x, Table (_db, table, _keys, _row))::gs, os, body) ->
        let body = select_clause index unit_query (For (None, gs, [], body)) in
        let os = List.map (base index) os in
          begin
            match body with
              | (_, fields, tables, condition, []) ->
                  (Sql.All, fields, Sql.TableRef(table, x)::tables, condition, os)
              | _ -> assert false
          end
    | If (c, body, Concat []) ->
      (* Turn conditionals into where clauses. We might want to do
         this earlier on.  *)
      let c = base index c in
      let (_, fields, tables, c', os) = select_clause index unit_query body in
      let c = Sql.smart_and c c' in
      (Sql.All, fields, tables, c, os)
    | Table (_db, table, _keys, (fields, _, _)) ->
      (* eta expand tables. We might want to do this earlier on.  *)
      (* In fact this should never be necessary as it is impossible
         to produce non-eta expanded tables. *)
      let var = Sql.fresh_table_var () in
      let fields =
        Sql.Fields
        (List.rev
            (StringMap.fold
              (fun name _ fields ->
                (Sql.Project (var, name), name)::fields)
              fields
              []))
      in
        (Sql.All, fields, [Sql.TableRef(table, var)], Sql.Constant (Constant.Bool true), [])
    | Singleton _ when unit_query ->
      (* If we're inside an Sql.Empty or a Sql.Length it's safe to ignore
         any fields here. *)
      (* We currently detect this earlier, so the unit_query stuff here
         is redundant. *)
      (Sql.All, Sql.Fields [], [], Sql.Constant (Constant.Bool true), [])
    | Singleton (Record fields) ->
      let fields =
        Sql.Fields
          (List.rev
            (StringMap.fold
              (fun name v fields ->
                (base index v, name)::fields)
              fields
              []))
      in
        (Sql.All, fields, [], Sql.Constant (Constant.Bool true), [])
    | _ -> assert false
and clause : Sql.index -> bool -> Q.t -> Sql.query =
  fun index unit_query v -> Sql.Select(select_clause index unit_query v)
and base : Sql.index -> Q.t -> Sql.base = fun index ->
  let open Q in
  function
    | If (c, t, e) ->
      Sql.Case (base index c, base index t, base index e)
    | Apply (Primitive "tilde", [s; r]) ->
      begin
        match likeify r with
          | Some r ->
            Sql.Apply ("LIKE", [base index s; base index r])
          | None ->
              begin
                let r =
                  (* HACK:
                     this only works if the regexp doesn't include any variables bound by the query
                  *)
                  Sql.Constant (Constant.String (Regex.string_of_regex (Linksregex.Regex.ofLinks (value_of_expression r))))
                in
                  Sql.Apply ("RLIKE", [base index s; r])
              end
        end
    | Apply (Primitive "Empty", [v]) ->
        Sql.Empty (unit_query v)
    | Apply (Primitive "length", [v]) ->
        Sql.Length (unit_query v)
    | Apply (Primitive f, vs) ->
        Sql.Apply (f, List.map (base index) vs)
    | Project (Var (x, _field_types), name) ->
        Sql.Project (x, name)
    | Constant c -> Sql.Constant c
    | Primitive "index" ->
        (* This is the only place the index parameter is ever materially used. *)
        Sql.RowNumber index
    | e ->
      Debug.print ("Not a base expression: " ^ Q.show e);
      assert false

and unit_query v =
  let prepare_clauses : Q.t -> Q.t list =
    function
      | Q.Concat vs -> vs
      | v -> [v]
  in
  (* queries passed to Empty and Length
     (where we don't care about what data they return)
  *)
  Sql.Union (Sql.All, List.map (clause [] true) (prepare_clauses v), 0)
and sql_of_query v =
  clause [] false v

(* The following code is specific to nested queries *)
(* The index parameter is essentially a free variable in the query
   that can only be replaced by Sql.RowNumber index.
   It would be nice to be able to remove this parameter and just
   substitute the SQL RowNumber expression when we generate SQL.
   Then the following nesting-specific code could live somewhere else, such as
   evalNestedQuery. *)

type let_clause = Var.var * Q.t * Var.var * Q.t
type let_query = let_clause list


let gens_index (gs : (Var.var * Q.t) list)   =
  let all_fields t =
    let field_types = Q.table_field_types t in
    Q.labels_of_field_types field_types
  in
 (* Use keys if available *)
  let key_fields t =
    match t with
      (_, _, (ks::_), _) -> StringSet.from_list ks
    |    _ -> all_fields t
  in
  let table_index get_fields (x, source) =
    let t = match source with Q.Table t -> t | _ -> assert false in
    let labels = get_fields t in
      List.rev
        (StringSet.fold
           (fun name ps -> (x, name) :: ps)
           labels
           [])
  in concat_map (table_index key_fields) gs

let outer_index gs_out = gens_index gs_out
let inner_index z gs_in =
  (* it's just a dynamic index! *)
  (z, "2") :: gens_index gs_in

let extract_gens =
  function
    | Q.For (_, gs, _, _) -> gs
    | _ -> assert false

let let_clause : let_clause -> Sql.query =
  fun (q, outer, t, inner) ->
    let gs_out = extract_gens outer in
    let gs_in = extract_gens inner in
    let q_outer = clause (outer_index gs_out) false outer in
    let (_fDist, result,tables,where,os) = select_clause (inner_index t gs_in) false inner in
    let tablename = Sql.string_of_subquery_var q in
    let q_inner = Sql.Select(Sql.All,result,Sql.TableRef(tablename,t)::tables,where,os) in
    Sql.With (tablename, q_outer, q_inner)

let sql_of_let_query : let_query -> Sql.query =
  fun cs ->
    Sql.Union (Sql.All, List.map (let_clause) cs, 0)

let update : ((Ir.var * string) * Q.t option * Q.t) -> Sql.query =
  fun ((_, table), where, body) ->
    let open Sql in
    let upd_where =
      OptionUtils.opt_map (base []) where in
    let upd_fields =
      Q.unbox_record body
      |> StringMap.map (base [])
      |> StringMap.to_alist in
    Update { upd_table = table; upd_fields; upd_where }

let delete : ((Ir.var * string) * Q.t option) -> Sql.query =
  fun ((_, table), where) ->
    let open Sql in
    let del_where = OptionUtils.opt_map (base []) where in
    Delete { del_table = table; del_where }

let compile_update : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option * Ir.computation) -> Sql.query =
  fun db env ((x, table, field_types), where, body) ->
    let tyx = Types.make_record_type field_types in
    let env = Q.bind (Eval.env_of_value_env QueryPolicy.Flat env) (x, Q.Var (x, tyx)) in
(*      let () = opt_iter (fun where ->  Debug.print ("where: "^Ir.show_computation where)) where in*)
    let where = opt_map (Eval.norm_comp env) where in
(*       Debug.print ("body: "^Ir.show_computation body); *)
    let body = Eval.norm_comp env body in
    let q = update ((x, table), where, body) in
      Debug.print ("Generated update query: " ^ (db#string_of_query q));
      q

let compile_delete : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option) -> Sql.query =
  fun db env ((x, table, field_types), where) ->
    let tyx = Types.make_record_type field_types in
    let env = Q.bind (Eval.env_of_value_env QueryPolicy.Flat env) (x, Q.Var (x, tyx)) in
    let where = opt_map (Eval.norm_comp env) where in
    let q = delete ((x, table), where) in
      Debug.print ("Generated update query: " ^ (db#string_of_query q));
      q

let insert table_name field_names rows =
  let rows = List.map (List.map (Q.expression_of_base_value ->- base [])) rows in
  Sql.(Insert {
      ins_table = table_name;
      ins_fields = field_names;
      ins_records = rows })