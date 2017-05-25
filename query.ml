(*pp deriving *)
open Utility

type t =
    [ `For of (Var.var * t) list * t list * t
    | `If of t * t * t
    | `Table of Value.table
    | `Singleton of t | `Concat of t list
    | `Record of t StringMap.t | `Project of t * string | `Erase of t * StringSet.t
    | `Variant of string * t
    | `XML of Value.xmlitem
    | `Apply of string * t list
    | `Closure of (Ir.var list * Ir.computation) * env
    | `Primitive of string
    | `Var of (Var.var * Types.datatype StringMap.t) | `Constant of Constant.constant ]
and env = Value.env * t Env.Int.t
    deriving (Show)

let unbox_xml =
  function
    | `XML xmlitem -> xmlitem
    | _ -> failwith ("failed to unbox XML")

let unbox_pair =
  function
    | `Record fields ->
        let x = StringMap.find "1" fields in
        let y = StringMap.find "2" fields in
          x, y
    | _ -> failwith ("failed to unbox pair")

let rec unbox_list =
  function
    | `Concat vs -> concat_map unbox_list vs
    | `Singleton v -> [v]
    | _ -> failwith ("failed to unbox list")

let unbox_string =
  function
    | `Constant (`String s) -> s
    | (`Concat _ | `Singleton _) as v ->
        implode
          (List.map
             (function
                | `Constant (`Char c) -> c
                | _ -> failwith ("failed to unbox string"))
             (unbox_list v))
    | _ -> failwith ("failed to unbox string")

(** Returns which database was used if any.

   Currently this assumes that at most one database is used.
*)
let used_database v : Value.database option =
  let rec generators =
    function
      | [] -> None
      | (_x, source)::gs ->
          begin
            match used source with
              | None -> generators gs
              | Some db -> Some db
          end
  and used =
    function
      | `For (gs, _, _body) -> generators gs
      | `Table ((db, _), _, _, _) -> Some db
      | _ -> None in
  let rec comprehensions =
    function
      | [] -> None
      | v::vs ->
          begin
            match used v with
              | None -> comprehensions vs
              | Some db -> Some db
          end
  in
    match v with
      | `Concat vs -> comprehensions vs
      | v -> used v

module S =
struct
  (** [pt]: A printable version of [t] *)
  type pt =
    [ `For of (Var.var * pt) list * pt list * pt
    | `If of pt * pt * pt
    | `Table of Value.table
    | `Singleton of pt | `Concat of pt list
    | `Record of pt StringMap.t | `Project of pt * string | `Erase of pt * StringSet.t
    | `Variant of string * pt
    | `XML of Value.xmlitem
    | `Apply of string * pt list
    | `Lam of Ir.var list * Ir.computation
    | `Primitive of string
    | `Var of (Var.var * Types.datatype StringMap.t) | `Constant of Constant.constant ]
      deriving (Show)

  let rec pt_of_t : t -> pt = fun v ->
    let bt = pt_of_t in
      match v with
        | `For (gs, os, b) ->
            `For (List.map (fun (x, source) -> (x, bt source)) gs,
                  List.map bt os,
                  bt b)
        | `If (c, t, e) -> `If (bt c, bt t, bt e)
        | `Table t -> `Table t
        | `Singleton v -> `Singleton (bt v)
        | `Concat vs -> `Concat (List.map bt vs)
        | `Record fields -> `Record (StringMap.map bt fields)
        | `Variant (name, v) -> `Variant (name, bt v)
        | `XML xmlitem -> `XML xmlitem
        | `Project (v, name) -> `Project (bt v, name)
        | `Erase (v, names) -> `Erase (bt v, names)
        | `Apply (f, vs) -> `Apply (f, List.map bt vs)
        | `Closure ((xs, e), _) -> `Lam (xs, e)
        | `Primitive f -> `Primitive f
        | `Var v -> `Var v
        | `Constant c -> `Constant c

  let t = Show_pt.show -<- pt_of_t
end
let string_of_t = S.t

let rec tail_of_t : t -> t = fun v ->
  let tt = tail_of_t in
    match v with
      | `For (_gs, _os, `Singleton (`Record fields)) -> `Record fields
      | `For (_gs, _os, `If (_, t, `Concat [])) -> tt (`For (_gs, _os, t))
      | _ -> (* Debug.print ("v: "^string_of_t v); *) assert false

(** Return the type associated with an expression *)
(* Inferring the type of an expression is straightforward because all
   variables are annotated with their types. *)
let rec type_of_expression : t -> Types.datatype = fun v ->
  let te = type_of_expression in
  let record fields : Types.datatype =
    Types.make_record_type (StringMap.map te fields)
  in
    match v with
      | `Concat (v::_) -> te v
      | `For (_, _os, body) -> te body
      | `Singleton (`Record fields) -> record fields
      | `If (_, t, _) -> te t
      | `Table (_, _, _, row) -> `Record row
      | `Constant (`Bool   _) -> Types.bool_type
      | `Constant (`Int    _) -> Types.int_type
      | `Constant (`Char   _) -> Types.char_type
      | `Constant (`Float  _) -> Types.float_type
      | `Constant (`String _) -> Types.string_type
      | `Project (`Var (_, field_types), name) -> StringMap.find name field_types
      | `Apply ("Empty", _) -> Types.bool_type (* HACK *)
      | `Apply (f, _) -> TypeUtils.return_type (Env.String.lookup Lib.type_env f)
      | e -> Debug.print("Can't deduce type for: " ^ Show_t.show e); assert false

let default_of_base_type =
  function
    | `Bool   -> `Constant (`Bool false)
    | `Int    -> `Constant (`Int 42)
    | `Char   -> `Constant (`Char '?')
    | `Float  -> `Constant (`Float 0.0)
    | `String -> `Constant (`String "")
    | _       -> assert false

let rec value_of_expression = fun v ->
  let ve = value_of_expression in
  let value_of_singleton = fun s ->
    match s with
      | `Singleton v -> ve v
      | _ -> assert false
  in
    match v with
      | `Constant (`Bool b) -> `Bool b
      | `Constant (`Int i) -> `Int i
      | `Constant (`Char c) -> `Char c
      | `Constant (`Float f) -> `Float f
      | `Constant (`String s) -> Value.box_string s
      | `Table t -> `Table t
      | `Concat vs -> `List (List.map value_of_singleton vs)
      | `Variant (name, v) -> `Variant (name, ve v)
      | `XML xmlitem -> `XML xmlitem
      | `Record fields ->
          `Record (List.rev (StringMap.fold (fun name v fields ->
                                               (name, ve v)::fields)
                               fields []))
      | _ -> assert false

let rec freshen_for_bindings : Var.var Env.Int.t -> t -> t =
  fun env v ->
    let ffb = freshen_for_bindings env in
      match v with
      | `For (gs, os, b) ->
        let gs', env' =
          List.fold_left
            (fun (gs', env') (x, source) ->
              let y = Var.fresh_raw_var () in
                ((y, ffb source)::gs', Env.Int.bind env' (x, y)))
            ([], env)
            gs
        in
          `For (List.rev gs', List.map (freshen_for_bindings env') os, freshen_for_bindings env' b)
      | `If (c, t, e) -> `If (ffb c, ffb t, ffb e)
      | `Table t -> `Table t
      | `Singleton v -> `Singleton (ffb v)
      | `Concat vs -> `Concat (List.map ffb vs)
      | `Record fields -> `Record (StringMap.map ffb fields)
      | `Variant (name, v) -> `Variant (name, ffb v)
      | `XML xmlitem -> `XML xmlitem
      | `Project (v, name) -> `Project (ffb v, name)
      | `Erase (v, names) -> `Erase (ffb v, names)
      | `Apply (f, vs) -> `Apply (f, List.map ffb vs)
      | `Closure c ->
        (* we don't attempt to freshen closure bindings *)
        `Closure c
      | `Primitive f -> `Primitive f
      | `Var (x, ts) ->
        begin
          match Env.Int.find env x with
          | None -> `Var (x, ts)
          | Some y -> `Var (y, ts)
        end
      | `Constant c -> `Constant c

let labels_of_field_types field_types =
  StringMap.fold
    (fun name _ labels' ->
      StringSet.add name labels')
    field_types
    StringSet.empty

let record_field_types (t : Types.datatype) : Types.datatype StringMap.t =
  let (field_spec_map, _, _) = TypeUtils.extract_row t in
  StringMap.map (function
                  | `Present t -> t
                  | _ -> assert false) field_spec_map

let table_field_types (_, _, _, (fields, _, _)) =
  StringMap.map (function
                  | `Present t -> t
                  | _ -> assert false) fields

let rec field_types_of_list =
  function
    | `Concat (v::_) -> field_types_of_list v
    | `Singleton (`Record fields) -> StringMap.map type_of_expression fields
    | `Table table -> table_field_types table
    | _ -> assert false

	  (* takes a normal form expression and returns true iff it has list type *)
let is_list =
  function
    | `For _
    | `Table _
    | `Singleton _
    | `Concat _
    | `If (_, _, `Concat []) -> true
    | _ -> false


	  (* TODO: Clean up and unify with Queryshredding.Eval *)

module Eval =
struct
  exception DbEvaluationError of string

  let nil = `Concat []



  let eval_error fmt =
    let error msg = raise (DbEvaluationError msg) in
      Printf.kprintf error fmt

  let env_of_value_env value_env = (value_env, Env.Int.empty)
  let (++) (venv, eenv) (venv', eenv') =
    Value.Env.shadow venv ~by:venv', Env.Int.extend eenv eenv'

  let lookup_fun (f, fvs) =
    match Tables.lookup Tables.fun_defs f with
    | Some (finfo, (xs, body), z, location) ->
      Some
      begin
        match Var.name_of_binder (f, finfo) with
        | "concatMap" ->
          `Primitive "ConcatMap"
        | "map" ->
          `Primitive "Map"
        | "empty" ->
          `Primitive "Empty"
        | "sortByBase" ->
          `Primitive "SortBy"
        | _ ->
          begin
            match location with
            | `Server | `Unknown ->
                let env =
                  match z, fvs with
                  | None, None       -> Value.Env.empty
                  | Some z, Some fvs -> Value.Env.bind z (fvs, `Local) Value.Env.empty
                  | _, _ -> assert false in
                `Closure ((xs, body), env_of_value_env env)
            | `Client ->
              failwith ("Attempt to use client function: " ^ Js.var_name_binder (f, finfo) ^ " in query")
            | `Native ->
              failwith ("Attempt to use native function: " ^ Var.Show_binder.show (f, finfo) ^ " in query")
          end
      end
    | None -> None

  let find_fun (f, fvs) =
    match lookup_fun (f, fvs) with
    | Some v -> v
    | None ->
      failwith ("Attempt to find undefined function: " ^ string_of_int f)

  let rec expression_of_value : Value.t -> t =
    function
      | `Bool b -> `Constant (`Bool b)
      | `Int i -> `Constant (`Int i)
      | `Char c -> `Constant (`Char c)
      | `Float f -> `Constant (`Float f)
      | `String s -> `Constant (`String s)
      | `Table t -> `Table t
      | `List vs ->
          `Concat (List.map (fun v -> `Singleton (expression_of_value v)) vs)
      | `Record fields ->
          `Record
            (List.fold_left
               (fun fields (name, v) -> StringMap.add name (expression_of_value v) fields)
               StringMap.empty
               fields)
      | `Variant (name, v) -> `Variant (name, expression_of_value v)
      | `XML xmlitem -> `XML xmlitem
      | `FunctionPtr (f, fvs) ->
        (* Debug.print ("Converting function pointer: " ^ string_of_int f ^ " to query closure"); *)
        find_fun (f, fvs)
      | `PrimitiveFunction (f,_) -> `Primitive f
          (*     | `ClientFunction f ->  *)
          (*     | `Continuation cont ->  *)
      | _ -> failwith "Cannot convert value to expression"

  let bind (val_env, exp_env) (x, v) =
    (val_env, Env.Int.bind exp_env (x, v))

  let lookup (val_env, exp_env) var =
    match lookup_fun (var, None) with
    | Some v -> v
    | None ->
      begin
        match Value.Env.lookup var val_env, Env.Int.find exp_env var with
        | None, Some v -> v
        | Some v, None -> expression_of_value v
        | Some _, Some v -> v (*eval_error "Variable %d bound twice" var*)
        | None, None ->
          begin
            try expression_of_value (Lib.primitive_stub (Lib.primitive_name var)) with
            | NotFound _ -> failwith ("Variable " ^ string_of_int var ^ " not found");
          end
      end

  let eta_expand_var (x, field_types) =
    `Record
      (StringMap.fold
         (fun name _t fields ->
            StringMap.add name (`Project (`Var (x, field_types), name)) fields)
         field_types
         StringMap.empty)

  let eta_expand_list xs =
    let x = Var.fresh_raw_var () in
    let field_types = field_types_of_list xs in
      ([x, xs], [], `Singleton (eta_expand_var (x, field_types)))

  let rec value env : Ir.value -> t = function
    | `Constant c -> `Constant c
    | `Variable var ->
        begin
          match lookup env var with
            | `Var (x, field_types) ->
                (* eta-expand record variables *)
                eta_expand_var (x, field_types)
            | `Primitive "Nil" -> nil
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
    | `Extend (ext_fields, r) ->
      begin
        match opt_app (value env) (`Record StringMap.empty) r with
          | `Record fields ->
            `Record (StringMap.fold
                       (fun label v fields ->
                         if StringMap.mem label fields then
                           eval_error
                             "Error adding fields: label %s already present"
                             label
                         else
                           StringMap.add label (value env v) fields)
                       ext_fields
                       fields)
          | _ -> eval_error "Error adding fields: non-record"
      end
    | `Project (label, r) ->
      let rec project (r, label) =
        match r with
          | `Record fields ->
            assert (StringMap.mem label fields);
            StringMap.find label fields
          | `If (c, t, e) ->
            `If (c, project (t, label), project (e, label))
          | `Var (x, field_types) ->
            assert (StringMap.mem label field_types);
            `Project (`Var (x, field_types), label)
          | _ -> eval_error "Error projecting from record"
      in
        project (value env r, label)
    | `Erase (labels, r) ->
      let rec erase (r, labels) =
        match r with
          | `Record fields ->
            assert (StringSet.for_all
                      (fun label -> StringMap.mem label fields) labels);
            `Record
              (StringMap.fold
                 (fun label v fields ->
                   if StringSet.mem label labels then
                     fields
                   else
                     StringMap.add label v fields)
                 fields
                 StringMap.empty)
          | `If (c, t, e) ->
            `If (c, erase (t, labels), erase (e, labels))
          | `Var (x, field_types) ->
            assert (StringSet.subset labels (labels_of_field_types field_types));
            `Erase (`Var (x, field_types), labels)
          | _ -> eval_error "Error erasing from record"
      in
        erase (value env r, labels)
    | `Inject (label, v, _) -> `Variant (label, value env v)
    | `TAbs (_, v) -> value env v
    | `TApp (v, _) -> value env v

    | `XmlNode (tag, attrs, children) ->
        (* TODO: deal with variables in XML *)
        let children =
          List.fold_right
            (fun v children ->
               let v = value env v in
                 List.map unbox_xml (unbox_list v) @ children)
            children [] in
        let children =
          StringMap.fold
            (fun name v attrs ->
               Value.Attr (name, unbox_string (value env v)) :: attrs)
            attrs children
        in
          `Singleton (`XML (Value.Node (tag, children)))

    | `ApplyPure (f, ps) ->
        apply env (value env f, List.map (value env) ps)
    | `Closure (f, v) ->
      let (_finfo, (xs, body), z_opt, _location) = Tables.find Tables.fun_defs f in
      let z = OptionUtils.val_of z_opt in
      (* Debug.print ("Converting evalir closure: " ^ Var.Show_binder.show (f, _finfo) ^ " to query closure"); *)
      (* yuck! *)
      let env' = bind (Value.Env.empty, Env.Int.empty) (z, value env v) in
      `Closure ((xs, body), env')
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
    | `Coerce (v, _) -> value env v

  and apply env : t * t list -> t = function
    | `Closure ((xs, body), closure_env), args ->
      (* Debug.print ("Applying closure"); *)
      (* Debug.print ("body: " ^ Ir.Show_computation.show body); *)
      (* Debug.print("Applying query closure: " ^ Show_t.show (`Closure ((xs, body), closure_env))); *)
      (* Debug.print("args: " ^ mapstrcat ", " Show_t.show args); *)
        let env = env ++ closure_env in
        let env = List.fold_right2 (fun x arg env ->
            bind env (x, arg)) xs args env in
        (* Debug.print("Applied"); *)
          computation env body
    | `Primitive "AsList", [xs] ->
        xs
    | `Primitive "Cons", [x; xs] ->
        reduce_concat [`Singleton x; xs]
    | `Primitive "Concat", [xs; ys] ->
        reduce_concat [xs; ys]
    | `Primitive "ConcatMap", [f; xs] ->
        begin
          match f with
            | `Closure (([x], body), closure_env) ->
                let env = env ++ closure_env in
                  reduce_for_source
                    (xs, fun v -> computation (bind env (x, v)) body)
            | _ -> assert false
        end
    | `Primitive "Map", [f; xs] ->
        begin
          match f with
            | `Closure (([x], body), closure_env) ->
                let env = env ++ closure_env in
                  reduce_for_source
                    (xs, fun v -> `Singleton (computation (bind env (x, v)) body))
            | _ -> assert false
        end
    | `Primitive "SortBy", [f; xs] ->
        begin
          match xs with
            | `Concat [] -> `Concat []
            | _ ->
                let gs, os', body =
                  match xs with
                    | `For (gs, os', body) -> gs, os', body
                    | `Concat (_::_)
                    | `Singleton _
                    | `Table _ ->
                        (* I think we can omit the `Table case as it
                           can never occur *)
                        (* eta-expand *)
                        eta_expand_list xs
                    | _ -> assert false in
                let xs = `For (gs, os', body) in
                  begin
                    match f with
                      | `Closure (([x], os), closure_env) ->
                          let os =
                            let env = env ++ closure_env in
                              let o = computation (bind env (x, tail_of_t xs)) os in
                                match o with
                                  | `Record fields ->
                                      List.rev (StringMap.fold (fun _ o os -> o::os) fields [])
                                  | _ -> assert false
                          in
                            `For (gs, os @ os', body)
                      | _ -> assert false
                  end
        end
    | `Primitive "not", [v] ->
      reduce_not (v)
    | `Primitive "&&", [v; w] ->
      reduce_and (v, w)
    | `Primitive "||", [v; w] ->
      reduce_or (v, w)
    | `Primitive "==", [v; w] ->
      reduce_eq (v, w)
    | `Primitive f, args ->
        `Apply (f, args)
    | `If (c, t, e), args ->
        reduce_if_condition (c, apply env (t, args), apply env (e, args))
    | `Apply (f, args), args' ->
        `Apply (f, args @ args')
    | _ -> eval_error "Application of non-function"
  and computation env (binders, tailcomp) : t =
    match binders with
      | [] -> tail_computation env tailcomp
      | b::bs ->
          begin
            match b with
              | `Let (xb, (_, tc)) ->
                  let x = Var.var_of_binder xb in
                    computation (bind env (x, tail_computation env tc)) (bs, tailcomp)
              | `Fun (_, _, _, (`Client | `Native)) ->
                  eval_error "Client function"
              | `Fun ((f, _), _, _, _) ->
                (* This should never happen now that we have closure conversion*)
                failwith ("Function definition in query: " ^ string_of_int f)
              | `Rec _ ->
                  eval_error "Recursive function"
              | `Alien _ -> (* just skip it *)
                  computation env (bs, tailcomp)
              | `Module _ -> failwith "Not implemented modules yet"
          end
  and tail_computation env : Ir.tail_computation -> t = function
    | `Return v -> value env v
    | `Apply (f, args) ->
        apply env (value env f, List.map (value env) args)
    | `Special (`Query (None, e, _)) -> computation env e
    | `Special _s ->
      (* FIXME:

         There's no particular reason why we can't allow
         table declarations in query blocks.

         Same goes for database declarations. (However, we do still
         have the problem that we currently have no way of enforcing
         that only one database be used inside a query block - see
         SML#.)  *)
      failwith "special not allowed in query block"
    | `Case (v, cases, default) ->
      let rec reduce_case (v, cases, default) =
        match v with
          | `Variant (label, v) as w ->
            begin
              match StringMap.lookup label cases, default with
                | Some ((x, _), c), _ ->
                  computation (bind env (x, v)) c
                | None, Some ((z, _), c) ->
                  computation (bind env (z, w)) c
                | None, None -> eval_error "Pattern matching failed"
            end
          | `If (c, t, e) ->
            `If
              (c,
               reduce_case (t, cases, default),
               reduce_case (e, cases, default))
          |  _ -> assert false
      in
        reduce_case (value env v, cases, default)
    | `If (c, t, e) ->
      let c = value env c in
      let t = computation env t in
      let e = computation env e in
        reduce_if_condition (c, t, e)
  and reduce_concat vs =
    let vs =
      concat_map
        (function
          | `Concat vs -> vs
          | v -> [v])
        vs
    in
      match vs with
        | [v] -> v
        | vs -> `Concat vs
  and reduce_for_source : t * (t -> t) -> t =
    fun (source, body) ->
      let rs = fun source -> reduce_for_source (source, body) in
        match source with
          | `Singleton v -> body v
          | `Concat vs ->
            reduce_concat (List.map rs vs)
          | `If (c, t, `Concat []) ->
            reduce_for_source
              (t, fun v -> reduce_where_then (c, body v))
          | `For (gs, os, v) ->
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
          | `Table table ->
            let field_types = table_field_types table in
            (* we need to generate a fresh variable in order to
               correctly handle self joins *)
            let x = Var.fresh_raw_var () in
              (* Debug.print ("fresh variable: " ^ string_of_int x); *)
              reduce_for_body ([(x, source)], [], body (`Var (x, field_types)))
          | v -> eval_error "Bad source in for comprehension: %s" (string_of_t v)
  and reduce_for_body (gs, os, body) =
    match body with
      | `For (gs', os', body') -> `For (gs @ gs', os @ os', body')
      | _                      -> `For (gs, os, body)
  and reduce_if_condition (c, t, e) =
    match c with
      | `Constant (`Bool true) -> t
      | `Constant (`Bool false) -> e
      | `If (c', t', _) ->
        reduce_if_body
          (reduce_or (reduce_and (c', t'),
                      reduce_and (reduce_not c', t')),
           t,
           e)
      | _ ->
        if is_list t then
          if e = nil then
            reduce_where_then (c, t)
          else
            reduce_concat [reduce_where_then (c, t);
                           reduce_where_then (reduce_not c, e)]
        else
          reduce_if_body (c, t, e)
  and reduce_where_then (c, t) =
    match t with
      (* optimisation *)
      | `Constant (`Bool true) -> t
      | `Constant (`Bool false) -> `Concat []

      | `Concat vs ->
        reduce_concat (List.map (fun v -> reduce_where_then (c, v)) vs)
      | `For (gs, os, body) ->
        `For (gs, os, reduce_where_then (c, body))
      | `If (c', t', `Concat []) ->
        reduce_where_then (reduce_and (c, c'), t')
      | _ ->
        `If (c, t, `Concat [])
  and reduce_if_body (c, t, e) =
    match t with
      | `Record then_fields ->
        begin match e with
          | `Record else_fields ->
            assert (StringMap.equal (fun _ _ -> true) then_fields else_fields);
            `Record
              (StringMap.fold
                 (fun name t fields ->
                   let e = StringMap.find name else_fields in
                     StringMap.add name (reduce_if_body (c, t, e)) fields)
                 then_fields
                 StringMap.empty)
          (* NOTE: this relies on any record variables having
             been eta-expanded by this point *)
          | _ -> eval_error "Mismatched fields"
        end
      | _ ->
        begin
          match t, e with
            | `Constant (`Bool true), _ ->
              reduce_or (c, e)
            | _, `Constant (`Bool false) ->
              reduce_and (c, t)
            | _ ->
              `If (c, t, e)
        end
  (* simple optimisations *)
  and reduce_and (a, b) =
    match a, b with
      | `Constant (`Bool true), x
      | x, `Constant (`Bool true)
      | (`Constant (`Bool false) as x), _
      | _, (`Constant (`Bool false) as x) -> x
      | _ -> `Apply ("&&", [a; b])
  and reduce_or (a, b) =
    match a, b with
      | (`Constant (`Bool true) as x), _
      | _, (`Constant (`Bool true) as x)
      | `Constant (`Bool false), x
      | x, `Constant (`Bool false) -> x
      | _ -> `Apply ("||", [a; b])
  and reduce_not a =
    match a with
      | `Constant (`Bool false) -> `Constant (`Bool true)
      | `Constant (`Bool true)  -> `Constant (`Bool false)
      | _                       -> `Apply ("not", [a])
  and reduce_eq (a, b) =
    let bool x = `Constant (`Bool x) in
    let eq_constant =
      function
        | (`Bool a  , `Bool b)   -> bool (a = b)
        | (`Int a   , `Int b)    -> bool (a = b)
        | (`Float a , `Float b)  -> bool (a = b)
        | (`Char a  , `Char b)   -> bool (a = b)
        | (`String a, `String b) -> bool (a = b)
        | (a, b)                 -> `Apply ("==", [`Constant a; `Constant b])
    in
      match a, b with
        | (`Constant a, `Constant b) -> eq_constant (a, b)
        | (`Variant (s1, a), `Variant (s2, b)) ->
          if s1 <> s2 then
            `Constant (`Bool false)
          else
            reduce_eq (a, b)
        | (`Record lfields, `Record rfields) ->
          List.fold_right2
            (fun (_, v1) (_, v2) e ->
              reduce_and (reduce_eq (v1, v2), e))
            (StringMap.to_alist lfields)
            (StringMap.to_alist rfields)
            (`Constant (`Bool true))
        | (a, b) -> `Apply ("==", [a; b])

  let eval env e =
(*    Debug.print ("e: "^Ir.Show_computation.show e); *)
    computation (env_of_value_env env) e
end

(* Introducing ordering indexes in order to support a list
   semantics. *)
module Order =
struct
  type gen = Var.var * t
  type context = gen list

(* TODO:

      - add a setting for selecting unordered queries
      - more refined generation of ordered queries
        - make use of unique keys to
          cut down on the number of order indexes
        - share order indexes when possible
        - remove duplicate fields from the output
  *)

  (* The following abstraction should allow us to customise the
     concrete choice of order indexes.

     In particular, we might use primary keys for generators.

     Including tail generators gives:

     table t

     a deterministic semantics, in the sense that asList (table t)
     will return the same list of rows throughout a query.

     In general this is unlikely to be necessary, as the programmer
     can supply an orderby clause when needed.

     If we ignore tail generators, then this corresponds to
     interpretting asList (table t) non-deterministically, that is,
     each invocation of asList (table t) may return a different
     permutation of the rows. (In practice, it's not clear whether
     real databases actually take advantage of the freedom to
     reorganise rows within a single query, but I believe it is
     allowed.)

     Given the non-deterministic interpration of asList, the
     normalisation procedure becomes technically unsound as it may
     duplicate instances of asList. If we wanted to restore soundness
     then we could do so by keeping track of which tables get
     duplicated and ensuring that we order by all occurences of them -
     e.g. by converting tail generators to non-tail generators. *)

  type order_index = [ `Val of t | `Gen of gen | `TailGen of gen
                     | `DefVal of Types.primitive | `DefGen of gen | `DefTailGen of gen
                     | `Branch of int ]

  (* TODO:

     We should probably represent 'defaultness' using a boolean flag
     rather than wiring it into a single polymorphic variant type. *)

  (* We might implement an optimisation to remove duplicate
     expressions from the output - in particular, the expressions we
     order by will often already be present in the output. Perhaps,
     though, it would make sense to apply such an optimisation more
     generally on any query. For instance:

     for (x <-- t) [(x.a, x.a)]

     might be translated as:

     select x.a from t as a

     followed by a post-processing phase that creates two copies of
     x.a.

     Another optimisation would be to remove duplicate expressions
     from the output order by clause. More ambitiously, this could be
     further generalised to handle examples such as the following:

     for (x <-- t) orderby (x.a, -x.a) [x]

     which is equivalent to:

     for (x <-- t) orderby (x.a) [x] *)

  type orders = order_index list

  type query_tree = [ `Node of orders * (int * query_tree) list
                    | `Leaf of (context * t) * orders ]

  type path = int list

  type preclause = (path * (context * t)) * query_tree
  type clause = context * t * orders

  let gen : (Var.var * t) -> t list =
    function
      | (x, `Table t) ->
        let field_types = table_field_types t in
          List.rev
            (StringMap.fold
               (fun name _t es ->
                 `Project (`Var (x, field_types), name) :: es
               ) field_types [])
      | _ -> assert false

  let base_type_of_expression t =
    match type_of_expression t with
      | `Primitive p -> p
      | _ -> assert false

  let default_of_base_value = default_of_base_type -<- base_type_of_expression

  (* convert orders to a list of expressions

     - represent generators by projecting all fields
     - ignore tail generators
  *)
  let long_orders : orders -> t list =
    let long =
      function
        | `Val t        -> [t]
        | `Gen g        -> gen g
        | `TailGen _    -> []
        | `DefVal t     -> [default_of_base_type t]
        | `DefGen g     -> List.map default_of_base_value (gen g)
        | `DefTailGen _ -> []
        | `Branch i     -> [`Constant (`Int i)]
    in
      concat_map long

  let lift_vals = List.map (fun o -> `Val o)
  let lift_gens = List.map (fun g -> `Gen g)
  let lift_tail_gens = List.map (fun g -> `TailGen g)

  let rec query : context -> t -> t -> query_tree =
    fun gs cond ->
      function
        | `Concat vs ->
          let cs = queries gs cond vs in
            `Node ([], cs)
        | `If (cond', v, `Concat []) ->
          query gs (Eval.reduce_and (cond, cond')) v
        | `For (gs', os, `Concat vs) ->
          let os' = lift_vals os @ lift_gens gs' in
          let cs = queries (gs @ gs') cond vs in
            `Node (os', cs)
        | `For (gs', os, body) ->
          `Leaf ((gs @ gs',
                  Eval.reduce_where_then (cond, body)),
                 lift_vals os @ lift_gens gs @ lift_tail_gens gs')
        | `Singleton r ->
          `Leaf ((gs, Eval.reduce_where_then (cond, `Singleton r)), [])
        | _ -> assert false
  and queries : context -> t -> t list -> (int * query_tree) list =
    fun gs cond vs ->
      let _, cs =
        List.fold_left
          (fun (i, cs) v ->
            let c = query gs cond v in
              (i+1, (i, c)::cs))
          (1, [])
          vs
      in
        List.rev cs

  (* convert all order indexes to default values *)
  let rec mask : query_tree -> query_tree =
    let dv =
      List.map
        (function
          | `Val t -> `DefVal (base_type_of_expression t)
          | `Gen g -> `DefGen g
          | `TailGen g -> `DefTailGen g
          | _ -> assert false)
    in
      function
        | `Node (os, cs) -> `Node (dv os, mask_children cs)
        | `Leaf (x, os)  -> `Leaf (x, dv os)
  and mask_children : (int * query_tree) list -> (int * query_tree) list =
    fun cs ->
      List.map (fun (branch, tree) -> (branch, mask tree)) cs

  (* decompose a query tree into a list of preclauses
     (path, query, tree) *)
  let rec decompose : query_tree -> preclause list =
    function
      | `Leaf (q, os) -> [(([], q), `Leaf (q, os))]
      | `Node (os, cs) ->
        List.map
          (fun ((path, q), cs) ->
            ((path, q), `Node (os, cs)))
          (decompose_children [] cs)
  and decompose_children prefix : (int * query_tree) list
      -> ((int list * (context * t)) * (int * query_tree) list) list =
    function
      | [] -> []
      | (branch, tree) :: cs ->
        let xs = decompose tree in
        let m = mask tree in
        let ms = mask_children cs in
          List.map
            (fun ((path, q), tree) ->
              ((branch :: path, q), prefix @ (branch, tree) :: ms))
            xs
          @ decompose_children (prefix @ [(branch, m)]) cs

  (* compute the order indexes for the specified query tree along a
     path *)
  let rec flatten_at path active : query_tree -> orders =
    function
        | `Leaf (_, os) -> os
        | `Node (os, cs) ->
          if active then
            let branch = List.hd path in
            let path   = List.tl path in
              os @ `Branch branch :: flatten_at_children branch path active cs
          else
            os @ `Branch 0 :: flatten_at_children 0 [] active cs
  and flatten_at_children branch path active =
    function
      | [] -> []
      | ((branch', tree) :: cs) ->
        if active then
          if branch == branch' then
            (flatten_at path true tree) @ (flatten_at_children branch path false cs)
          else
            (flatten_at path false tree) @ (flatten_at_children branch path true cs)
        else
          (flatten_at path false tree) @ (flatten_at_children branch path false cs)

  (* flatten a query tree as a list of subqueries *)
  let flatten_tree q =
    List.map
      (fun ((path, (gs, body)), tree) ->
        (gs, body, flatten_at path true tree))
      (decompose q)

  let query : t -> clause list =
    fun v ->
      let q = query [] (`Constant (`Bool true)) v in
      let ss = flatten_tree q in
        ss

  (* FIXME:

     Be more careful about ensuring that the order index field names
     do not clash with existing field names *)
  let query_of_clause pick_orders (gs, body, os) =
    let orders = pick_orders os in
    let rec add_indexes fields i =
      function
        | []      -> fields
        | o :: os ->
          add_indexes
            (StringMap.add ("order_" ^ string_of_int i) o fields)
            (i+1)
            os in
    let rec order =
      function
        | `Singleton (`Record fields) ->
          `Singleton (`Record (add_indexes fields 1 orders))
        | `If (c, body, `Concat []) ->
          `If (c, order body, `Concat [])
        | _ -> assert false in
    let body' = order body in
      match gs with
        | [] -> body'
        | _  -> `For (gs, [], body')

  let index_length : (orders -> t list) -> clause list -> int =
    fun pick_orders ->
      function
        | (_, _, os) :: _ -> List.length (pick_orders os)
        | [] -> assert false

  let ordered_query v =
    let ss = query v in
    let n = index_length long_orders ss in
    let vs = List.map (query_of_clause long_orders) ss in
      vs, n
end

    (* TODO: Unify this with Queryshredding.ShreddedSql *)

module Sql =
struct
  type query =
    [ `UnionAll of query list * int
    | `Select of (base * string) list * (string * Var.var) list * base * base list ]
  and base =
    [ `Case of (base * base * base)
    | `Constant of Constant.constant
    | `Project of Var.var * string
    | `Apply of string * base list
    | `Empty of query
    | `Length of query ]
      deriving (Show)

  (* Table variables that are actually used are always bound in a for
     comprehension. In this case the IR variable from the for
     comprehension is used to generate the table variable.

     e.g. if the IR variable is 1485 then the table variable is t1485
  *)
  let fresh_table_var : unit -> Var.var = Var.fresh_raw_var
  let string_of_table_var var = "t" ^ string_of_int var

  (* Because of limitations of SQL we sometimes need to generate dummy
     table variables. These have the prefix "dummy" and have their own
     name source. *)
  let dummy_counter = ref 0
  let reset_dummy_counter () = dummy_counter := 0
  let fresh_dummy_var () =
    incr dummy_counter;
    "dummy" ^ string_of_int (!dummy_counter)

  module Arithmetic :
  sig
    val is : string -> bool
    val gen : (string * string * string) -> string
  end =
  struct
    let builtin_ops =
      StringMap.from_alist
        [ "+",   Some "+"  ;
          "+.",  Some "+"  ;
          "-",   Some "-"  ;
          "-.",  Some "-"  ;
          "*",   Some "*"  ;
          "*.",  Some "*"  ;
          "/",   None      ;
          "^",   None      ;
          "^.",  None      ;
          "/.",  Some "/"  ;
          "mod", Some "%"  ;
	  (* FIXME: The SQL99 || operator is supported in PostgreSQL and
	     SQLite but not in MySQL, where it denotes the logical or
	     operator *)
	  "^^",  Some "||" ]

    let is x = StringMap.mem x builtin_ops
    let sql_name op = val_of (StringMap.find op builtin_ops)
    let gen (l, op, r) =
      match op with
        | "/" -> "floor("^l^"/"^r^")"
        | "^" -> "floor(pow("^l^","^r^"))"
        | "^." -> "pow("^l^","^r^")"
        | _ -> "("^l^sql_name op^r^")"
  end

  module SqlFuns :
  sig
    val is : string -> bool
    val name : string -> string
  end =
  struct
    let funs =
      StringMap.from_alist
        [ "toUpper",  "upper";
          "toLower",  "lower";
          "ord",      "ord";
          "chr",      "char";
          "random",   "rand" ]

    let is f = StringMap.mem f funs
    let name f = StringMap.find f funs
  end

  let order_by_clause n =
    if n == 0 then
      ""
    else
      let rec order i n =
        if i > n then
          []
        else
          ("order_" ^ string_of_int i) :: order (i+1) n
      in
        " order by " ^ String.concat "," (order 1 n)

  (* For `Empty and `Length we don't care about the actual data
     returned. This allows these operators to take lists that have any
     element type at all. *)

  let rec string_of_query db ignore_fields q =
    let sq = string_of_query db ignore_fields in
    let sb = string_of_base db false in
    let string_of_fields fields =
      if ignore_fields then
        "0 as dummy" (* SQL doesn't support empty records! *)
      else
        match fields with
          | [] -> "0 as dummy" (* SQL doesn't support empty records! *)
          | fields ->
            mapstrcat ","
              (fun (b, l) ->
                "(" ^ sb b ^ ") as "^ db#quote_field l) (* string_of_label l) *)
              fields
    in
      match q with
        | `UnionAll ([], _) -> assert false
        | `UnionAll ([q], n) -> sq q ^ order_by_clause n
        | `UnionAll (qs, n) ->
          mapstrcat " union all " (fun q -> "(" ^ sq q ^ ")") qs ^ order_by_clause n
        | `Select (fields, [], `Constant (`Bool true), _os) ->
            let fields = string_of_fields fields in
              "select " ^ fields
        | `Select (fields, [], condition, _os) ->
            let fields = string_of_fields fields in
              "select * from (select " ^ fields ^ ") as " ^ fresh_dummy_var () ^ " where " ^ sb condition
        | `Select (fields, tables, condition, os) ->
            let tables = mapstrcat "," (fun (t, x) -> t ^ " as " ^ (string_of_table_var x)) tables in
            let fields = string_of_fields fields in
            let orderby =
              match os with
                | [] -> ""
                | _ -> " order by " ^ mapstrcat "," sb os in
            let where =
              match condition with
                | `Constant (`Bool true) -> ""
                | _ ->  " where " ^ sb condition
            in
              "select " ^ fields ^ " from " ^ tables ^ where ^ orderby
  and string_of_base db one_table b =
    let sb = string_of_base db one_table in
      match b with
        | `Case (c, t, e) ->
            "case when " ^ sb c ^ " then " ^sb t ^ " else "^ sb e ^ " end"
        | `Constant c -> Constant.string_of_constant c
        | `Project (var, label) ->
            if one_table then
              db#quote_field label
            else
              string_of_table_var var ^ "." ^ (db#quote_field label)
        | `Apply (op, [l; r]) when Arithmetic.is op
            -> Arithmetic.gen (sb l, op, sb r)
        | `Apply (("intToString" | "stringToInt" | "intToFloat" | "floatToString"
                  | "stringToFloat"), [v]) -> sb v
        | `Apply ("floatToInt", [v]) -> "floor("^sb v^")"

        (* optimisation *)
        | `Apply ("not", [`Empty q]) -> "exists (" ^ string_of_query db true q ^ ")"

        | `Apply ("not", [v]) -> "not (" ^ sb v ^ ")"
        | `Apply (("negate" | "negatef"), [v]) -> "-(" ^ sb v ^ ")"
        | `Apply ("&&", [v; w]) -> "(" ^ sb v ^ ")" ^ " and " ^ "(" ^ sb w ^ ")"
        | `Apply ("||", [v; w]) -> "(" ^ sb v ^ ")" ^ " or " ^ "(" ^ sb w ^ ")"
        | `Apply ("==", [v; w]) -> "(" ^ sb v ^ ")" ^ " = " ^ "(" ^ sb w ^ ")"
        | `Apply ("<>", [v; w]) -> "(" ^ sb v ^ ")" ^ " <> " ^ "(" ^ sb w ^ ")"
        | `Apply ("<", [v; w]) -> "(" ^ sb v ^ ")" ^ " < " ^ "(" ^ sb w ^ ")"
        | `Apply (">", [v; w]) -> "(" ^ sb v ^ ")" ^ " > " ^ "(" ^ sb w ^ ")"
        | `Apply ("<=", [v; w]) -> "(" ^ sb v ^ ")" ^ " <= " ^ "(" ^ sb w ^ ")"
        | `Apply (">=", [v; w]) -> "(" ^ sb v ^ ")" ^ " >= " ^ "(" ^ sb w ^ ")"
        | `Apply ("RLIKE", [v; w]) -> "(" ^ sb v ^ ")" ^ " RLIKE " ^ "(" ^ sb w ^ ")"
        | `Apply ("LIKE", [v; w]) -> "(" ^ sb v ^ ")" ^ " LIKE " ^ "(" ^ sb w ^ ")"
        | `Apply (f, args) when SqlFuns.is f -> SqlFuns.name f ^ "(" ^ String.concat "," (List.map sb args) ^ ")"
        | `Apply (f, args) -> f ^ "(" ^ String.concat "," (List.map sb args) ^ ")"
        | `Empty q -> "not exists (" ^ string_of_query db true q ^ ")"
        | `Length q -> "select count(*) from (" ^ string_of_query db true q ^ ") as " ^ fresh_dummy_var ()

  let string_of_query db range q =
    let range =
      match range with
        | None -> ""
        | Some (limit, offset) -> " limit " ^ string_of_int limit^" offset "^ string_of_int offset
    in
      string_of_query db false q ^ range

  let prepare_clauses : t -> t list =
    function
      | `Concat vs -> vs
      | v -> [v]

  let rec clause : Value.database -> t -> query = fun db v ->
(*    Debug.print ("clause: "^string_of_t v); *)
    match v with
      | `Concat _ -> assert false
      | `For ([], _, body) ->
          clause db body
      | `For ((x, `Table (_db, table, _, _row))::gs, os, body) ->
          let body = clause db (`For (gs, [], body)) in
          let os = List.map (base db) os in
            begin
              match body with
                | `Select (fields, tables, condition, []) ->
                    `Select (fields, (table, x)::tables, condition, os)
                | _ -> assert false
            end
      | `If (c, body, `Concat []) ->
        (* Turn conditionals into where clauses. We might want to do
           this earlier on.  *)
        let c = base db c in
        let body = clause db body in
          begin
            match body with
              | `Select (fields, tables, c', os) ->
                let c =
                  match c, c' with
                    (* optimisations *)
                    | `Constant (`Bool true), c
                    | c, `Constant (`Bool true) -> c
                    | `Constant (`Bool false), _
                    | _, `Constant (`Bool false) -> `Constant (`Bool false)
                    (* default case *)
                    | c, c' -> `Apply ("&&", [c; c'])
                in
                  `Select (fields, tables, c, os)
              | _ -> assert false
          end
      | `Table (_db, table, _keys, (fields, _, _)) ->
        (* eta expand tables. We might want to do this earlier on.  *)
        (* In fact this should never be necessary as it is impossible
           to produce non-eta expanded tables. *)
        let var = fresh_table_var () in
        let fields =
          List.rev
            (StringMap.fold
               (fun name _ fields ->
                 (`Project (var, name), name)::fields)
               fields
               [])
        in
          `Select (fields, [(table, var)], `Constant (`Bool true), [])
      | `Singleton (`Record fields) ->
        let fields =
          List.rev
            (StringMap.fold
               (fun name v fields ->
                 (base db v, name)::fields)
               fields
               [])
        in
          `Select (fields, [], `Constant (`Bool true), [])

      | `Singleton _ ->
        (* If we're inside an `Empty or a `Length it's safe to
           ignore any fields here. Otherwise this line should be
           unreachable. *)
        `Select ([], [], `Constant (`Bool true), [])
      | _ -> assert false
  and base : Value.database -> t -> base = fun db ->
    function
      | `If (c, t, e) ->
        `Case (base db c, base db t, base db e)
      | `Apply ("tilde", [s; r]) ->
        begin
          match likeify r with
            | Some r ->
              `Apply ("LIKE", [base db s; `Constant (`String r)])
            | None ->
              let r =
                    (* HACK:

                       this only works if the regexp doesn't include any variables bound by the query
                    *)
                    `Constant (`String (Regex.string_of_regex (Linksregex.Regex.ofLinks (value_of_expression r))))
                  in
                    `Apply ("RLIKE", [base db s; r])
          end
      | `Apply ("Empty", [v]) ->
          `Empty (outer_query db v)
      | `Apply ("length", [v]) ->
          `Length (outer_query db v)
      | `Apply (f, vs) ->
          `Apply (f, List.map (base db) vs)
      | `Project (`Var (x, _field_types), name) ->
          `Project (x, name)
      | `Constant c -> `Constant c
      | v -> failwith ("Bad base value: " ^ string_of_t v)

  (* convert a regexp to a like if possible *)
  and likeify v =
    let quote = Str.global_replace (Str.regexp_string "%") "\\%" in
      match v with
        | `Variant ("Repeat", pair) ->
            begin
              match unbox_pair pair with
                | `Variant ("Star", _), `Variant ("Any", _) -> Some ("%")
                | _ -> None
            end
        | `Variant ("Simply", `Constant (`String s)) -> Some (quote s)
        | `Variant ("Quote", `Variant ("Simply", v)) ->
            (* TODO:

               detect variables and convert to a concatenation operation
               (this needs to happen in RLIKE compilation as well)
            *)
           let rec string =
              function
                | `Constant (`String s) -> Some s
                | `Singleton (`Constant (`Char c)) -> Some (string_of_char c)
                | `Concat vs ->
                    let rec concat =
                      function
                        | [] -> Some ""
                        | v::vs ->
                            begin
                              match string v with
                                | None -> None
                                | Some s ->
                                    begin
                                      match concat vs with
                                        | None -> None
                                        | Some s' -> Some (s ^ s')
                                    end
                            end
                    in
                      concat vs
                | _ -> None
            in
              opt_map quote (string v)
        | `Variant ("Seq", rs) ->
            let rec seq =
              function
                | [] -> Some ""
                | r::rs ->
                    begin
                      match likeify r with
                        | None -> None
                        | Some s ->
                            begin
                              match seq rs with
                                | None -> None
                                | Some s' -> Some (s^s')
                            end
                    end
            in
              seq (unbox_list rs)
        | `Variant ("StartAnchor", _) -> Some ""
        | `Variant ("EndAnchor", _) -> Some ""
        | _ -> assert false
  and outer_query db v =
    `UnionAll (List.map (clause db) (prepare_clauses v), 0)

  let ordered_query db range v =
    (* Debug.print ("v: "^string_of_t v); *)
    reset_dummy_counter ();
    let vs, n = Order.ordered_query v in
    (* Debug.print ("concat vs: "^string_of_t (`Concat vs)); *)
    let q = `UnionAll (List.map (clause db) vs, n) in
      string_of_query db range q

  let update db ((_, table), where, body) =
    reset_dummy_counter ();
    let base = (base db) ->- (string_of_base db true) in
    let where =
      match where with
        | None -> ""
        | Some where ->
            " where (" ^ base where ^ ")" in
    let fields =
      match body with
        | `Record fields ->
            String.concat ","
              (List.map
                 (fun (label, v) -> db#quote_field label ^ " = " ^ base v)
                 (StringMap.to_alist fields))
        | _ -> assert false
    in
      "update "^table^" set "^fields^where

  let delete db ((_, table), where) =
    reset_dummy_counter ();
    let base = base db ->- (string_of_base db true) in
    let where =
      match where with
        | None -> ""
        | Some where ->
            " where (" ^ base where ^ ")"
    in
      "delete from "^table^where
end

let compile : Value.env -> (int * int) option * Ir.computation -> (Value.database * string * Types.datatype) option =
  fun env (range, e) ->
    (* Debug.print ("e: "^Ir.Show_computation.show e); *)
    let v = Eval.eval env e in
      (* Debug.print ("v: "^string_of_t v); *)
      match used_database v with
        | None -> None
        | Some db ->
            let t = type_of_expression v in
            let q = Sql.ordered_query db range v in
              Debug.print ("Generated query: "^q);
              Some (db, q, t)

let compile_update : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option * Ir.computation) -> string =
  fun db env ((x, table, field_types), where, body) ->
    let env = Eval.bind (Eval.env_of_value_env env) (x, `Var (x, field_types)) in
(*      let () = opt_iter (fun where ->  Debug.print ("where: "^Ir.Show_computation.show where)) where in*)
    let where = opt_map (Eval.computation env) where in
(*       Debug.print ("body: "^Ir.Show_computation.show body); *)
    let body = Eval.computation env body in
    let q = Sql.update db ((x, table), where, body) in
      Debug.print ("Generated update query: "^q);
      q

let compile_delete : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option) -> string =
  fun db env ((x, table, field_types), where) ->
    let env = Eval.bind (Eval.env_of_value_env env) (x, `Var (x, field_types)) in
    let where = opt_map (Eval.computation env) where in
    let q = Sql.delete db ((x, table), where) in
      Debug.print ("Generated update query: "^q);
      q
