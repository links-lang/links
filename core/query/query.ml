open Utility
open CommonTypes
open Var

exception DbEvaluationError of string

let internal_error message =
  Errors.internal_error ~filename:"query/query.ml" ~message

let runtime_type_error error =
  internal_error
    ("Runtime type error: " ^ error ^ ".\n" ^
     "This should not happen if the type system / checker is correct. " ^
     "Please file a bug report.")

let query_error fmt =
  let error msg = raise (DbEvaluationError msg) in
    Printf.kprintf error fmt

let use_keys_in_shredding
  = Settings.(flag ~default:true "use_keys_in_shredding"
              |> synopsis "Use keys in query shredding"
              |> convert parse_bool
              |> sync)

module Lang =
struct

  type base_type = | Bool | Char | Float | Int | String

  type tag = int
      [@@deriving show]

  type t =
      | For       of tag option * (Var.var * t) list * t list * t
      | If        of t * t * t
      | Table     of Value.table
      | Database  of (Value.database * string)
      | Singleton of t
      | Concat    of t list
      | Record    of t StringMap.t
      | Project   of t * string
      | Erase     of t * StringSet.t
      | Variant   of string * t
      | XML       of Value.xmlitem
      | Apply     of t * t list
      | Closure   of (Ir.var list * Ir.computation) * env
      | Case      of t * (binder * t) StringMap.t * (binder * t) option
      | Primitive of string
      | Var       of Var.var * Types.datatype StringMap.t
      | Constant  of Constant.t
  and env = { venv: Value.env; qenv: t Env.Int.t; policy: QueryPolicy.t }
      [@@deriving show]

  let nil = Concat []

  module S =
  struct
    (** [pt]: A printable version of [t] *)
    type pt =
      | For       of (Var.var * pt) list * pt list * pt
      | If        of pt * pt * pt
      | Table     of Value.table
      | Singleton of pt
      | Concat    of pt list
      | Record    of pt StringMap.t
      | Project   of pt * string
      | Erase     of pt * StringSet.t
      | Variant   of string * pt
      | XML       of Value.xmlitem
      | Apply     of pt * pt list
      | Lam       of Ir.var list * Ir.computation
      | Case      of pt * (binder * pt) StringMap.t * (binder * pt) option
      | Primitive of string
      | Var       of (Var.var * Types.datatype StringMap.t)
      | Constant  of Constant.t
        [@@deriving show]
  end

  let rec pt_of_t : t -> S.pt = fun v ->
    let bt = pt_of_t in
      match (v : t) with
        | For (_, gs, os, b) ->
            S.For (List.map (fun (x, source) -> (x, bt source)) gs,
                  List.map bt os,
                  bt b)
        | If (c, t, e) -> S.If (bt c, bt t, bt e)
        | Table t -> S.Table t
        | Singleton v -> S.Singleton (bt v)
        | Concat vs -> S.Concat (List.map bt vs)
        | Record fields -> S.Record (StringMap.map bt fields)
        | Variant (name, v) -> S.Variant (name, bt v)
        | XML xmlitem -> S.XML xmlitem
        | Project (v, name) -> S.Project (bt v, name)
        | Erase (v, names) -> S.Erase (bt v, names)
        | Apply (u, vs) -> S.Apply (bt u, List.map bt vs)
        | Case (u, cl, d) -> S.Case (bt u, StringMap.map (fun (x,y) -> (x, bt y)) cl, opt_app (fun (x,y) -> Some (x, bt y)) None d)
        | Closure ((xs, e), _) -> S.Lam (xs, e)
        | Primitive f -> S.Primitive f
        | Var (v, t) -> S.Var (v, t)
        | Constant c -> S.Constant c
        | Database _ -> assert false

  let string_of_t = S.show_pt -<- pt_of_t

  let rec tail_of_t : t -> t = fun v ->
    let tt = tail_of_t in
      match v with
        | For (_, _gs, _os, Singleton (Record fields)) -> Record fields
        | For (_tag, _gs, _os, If (_, t, Concat [])) -> tt (For (_tag, _gs, _os, t))
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
        | Concat [] -> Types.make_list_type(Types.unit_type)
        | Concat (v::_) -> te v
        | For (_, _, _os, body) -> te body
        | Singleton t -> Types.make_list_type (te t)
        | Record fields -> record fields
        | If (_, t, _) -> te t
        | Table (_, _, _, row) -> `Record row
        | Constant (Constant.Bool   _) -> Types.bool_type
        | Constant (Constant.Int    _) -> Types.int_type
        | Constant (Constant.Char   _) -> Types.char_type
        | Constant (Constant.Float  _) -> Types.float_type
        | Constant (Constant.String _) -> Types.string_type
        | Project (Var (_, field_types), name) -> StringMap.find name field_types
        | Apply (Primitive "Empty", _) -> Types.bool_type (* HACK *)
        | Apply (Primitive f, _) -> TypeUtils.return_type (Env.String.find f Lib.type_env)
        | e -> Debug.print("Can't deduce type for: " ^ show e); assert false

  let default_of_base_type =
    function
      | Primitive.Bool   -> Constant (Constant.Bool false)
      | Primitive.Int    -> Constant (Constant.Int 42)
      | Primitive.Char   -> Constant (Constant.Char '?')
      | Primitive.Float  -> Constant (Constant.Float 0.0)
      | Primitive.String -> Constant (Constant.String "")
      | _                -> assert false

  let rec value_of_expression = fun v ->
    let ve = value_of_expression in
    let value_of_singleton = fun s ->
      match s with
        | Singleton v -> ve v
        | _ -> assert false
    in
      match v with
        | Constant (Constant.Bool   b) -> `Bool b
        | Constant (Constant.Int    i) -> `Int i
        | Constant (Constant.Char   c) -> `Char c
        | Constant (Constant.Float  f) -> `Float f
        | Constant (Constant.String s) -> Value.box_string s
        | Table t -> `Table t
        | Concat vs -> `List (List.map value_of_singleton vs)
        | Variant (name, v) -> `Variant (name, ve v)
        | XML xmlitem -> `XML xmlitem
        | Record fields ->
            `Record (List.rev (StringMap.fold (fun name v fields ->
                                                 (name, ve v)::fields)
                                 fields []))
        | _ -> assert false

  let rec expression_of_base_value : Value.t -> t = function
    | `Bool b -> Constant (Constant.Bool b)
    | `Char c -> Constant (Constant.Char c)
    | `Float f -> Constant (Constant.Float f)
    | `Int i -> Constant (Constant.Int i)
    | `String s -> Constant (Constant.String s)
    | `Record fields ->
        let fields =
          fields
          |> List.map (fun (k, v) -> (k, expression_of_base_value v))
          |> StringMap.from_alist in
        Record fields
    | other ->
        raise (internal_error ("expression_of_base_value undefined for " ^
          Value.string_of_value other))

  let rec freshen_for_bindings : Var.var Env.Int.t -> t -> t =
    fun env v ->
      let ffb = freshen_for_bindings env in
        match v with
        | For (tag, gs, os, b) ->
          let gs', env' =
            List.fold_left
              (fun (gs', env') (x, source) ->
                let y = Var.fresh_raw_var () in
                  ((y, ffb source)::gs', Env.Int.bind x y env'))
              ([], env)
              gs
          in
            For (tag, List.rev gs', List.map (freshen_for_bindings env') os, freshen_for_bindings env' b)
        | If (c, t, e) -> If (ffb c, ffb t, ffb e)
        | Table _ as t -> t
        | Singleton v -> Singleton (ffb v)
        | Database db -> Database db
        | Concat vs -> Concat (List.map ffb vs)
        | Record fields -> Record (StringMap.map ffb fields)
        | Variant (name, v) -> Variant (name, ffb v)
        | XML xmlitem -> XML xmlitem
        | Project (v, name) -> Project (ffb v, name)
        | Erase (v, names) -> Erase (ffb v, names)
        | Apply (u, vs) -> Apply (ffb u, List.map ffb vs)
        | Closure _ as c ->
          (* we don't attempt to freshen closure bindings *)
          c
        | Case (u, cl, d) -> Case (ffb u, StringMap.map (fun (x,y) -> (x, ffb y)) cl, opt_app (fun (x,y) -> Some (x, ffb y)) None d)
        | Primitive f -> Primitive f
        | Var (x, ts) as v ->
          begin
            match Env.Int.find_opt x env with
            | None -> v (* Var (x, ts) *)
            | Some y -> Var (y, ts)
          end
        | Constant c -> Constant c

  let table_field_types (_, _, _, (fields, _, _)) =
    StringMap.map (function
                    | `Present t -> t
                    | _ -> assert false) fields

  let unbox_xml =
    function
      | XML xmlitem -> xmlitem
      | _ -> raise (runtime_type_error "failed to unbox XML")

  let unbox_pair =
    function
      | Record fields ->
          let x = StringMap.find "1" fields in
          let y = StringMap.find "2" fields in
            x, y
      | _ -> raise (runtime_type_error "failed to unbox pair")

  let rec unbox_list =
    function
      | Concat vs -> concat_map unbox_list vs
      | Singleton v -> [v]
      | _ -> raise (runtime_type_error "failed to unbox list")

  let unbox_record =
    function
      | Record r -> r
      | _ -> raise (runtime_type_error "failed to unbox record")

  let unbox_string =
    function
      | Constant (Constant.String s) -> s
      | (Concat _ | Singleton _) as v ->
          implode
            (List.map
               (function
                  | Constant (Constant.Char c) -> c
                  | _ -> raise (runtime_type_error "failed to unbox string"))
               (unbox_list v))
      | _ -> raise (runtime_type_error "failed to unbox string")

  let rec field_types_of_list =
    function
      | Concat (v::_) -> field_types_of_list v
      | Singleton (Record fields) -> StringMap.map type_of_expression fields
      | Table table -> table_field_types table
      | _ -> assert false

  (* takes a normal form expression and returns true iff it has list type *)
  let is_list =
    function
      | For _
      | Table _
      | Singleton _
      | Concat _
      | If (_, _, Concat []) -> true
      | _ -> false


  (* simple optimisations *)
  let reduce_and (a, b) =
    match a, b with
      | Constant (Constant.Bool true), x
      | x, Constant (Constant.Bool true)
      | (Constant (Constant.Bool false) as x), _
      | _, (Constant (Constant.Bool false) as x) -> x
      | _ -> Apply  (Primitive "&&", [a; b])

  let reduce_or (a, b) =
    match a, b with
      | (Constant (Constant.Bool true) as x), _
      | _, (Constant (Constant.Bool true) as x)
      | Constant (Constant.Bool false), x
      | x, Constant (Constant.Bool false) -> x
      | _ -> Apply  (Primitive "||", [a; b])

  let reduce_not a =
    match a with
      | Constant (Constant.Bool false) -> Constant (Constant.Bool true)
      | Constant (Constant.Bool true)  -> Constant (Constant.Bool false)
      | _                       -> Apply  (Primitive "not", [a])

  let rec reduce_eq (a, b) =
    let bool x = Constant (Constant.Bool x) in
    let eq_constant =
      function
        | (Constant.Bool a  , Constant.Bool b)   -> bool (a = b)
        | (Constant.Int a   , Constant.Int b)    -> bool (a = b)
        | (Constant.Float a , Constant.Float b)  -> bool (a = b)
        | (Constant.Char a  , Constant.Char b)   -> bool (a = b)
        | (Constant.String a, Constant.String b) -> bool (a = b)
        | (a, b)                 -> Apply (Primitive "==", [Constant a; Constant b])
    in
      match a, b with
        | (Constant a, Constant b) -> eq_constant (a, b)
        | (Variant (s1, a), Variant (s2, b)) ->
          if s1 <> s2 then
            Constant (Constant.Bool false)
          else
            reduce_eq (a, b)
        | (Record lfields, Record rfields) ->
          List.fold_right2
            (fun (_, v1) (_, v2) e ->
              reduce_and (reduce_eq (v1, v2), e))
            (StringMap.to_alist lfields)
            (StringMap.to_alist rfields)
            (Constant (Constant.Bool true))
        | (a, b) -> Apply (Primitive "==", [a; b])

  let reduce_concat vs =
    let vs =
      concat_map
        (function
          | Concat vs -> vs
          | v -> [v])
        vs
    in
      match vs with
        | [v] -> v
        | vs -> Concat vs

  let rec reduce_where_then (c, t) =
    match t with
      (* optimisation *)
      | Constant (Constant.Bool true) -> t
      | Constant (Constant.Bool false) -> Concat []

      | Concat vs ->
        reduce_concat (List.map (fun v -> reduce_where_then (c, v)) vs)
      | For (_, gs, os, body) ->
        For (None, gs, os, reduce_where_then (c, body))
      | If (c', t', Concat []) ->
        reduce_where_then (reduce_and (c, c'), t')
      | _ ->
        If (c, t, Concat [])

  let reduce_for_body (gs, os, body) =
    match body with
      | For (_, gs', os', body') -> For (None, gs @ gs', os @ os', body')
      | _                         -> For (None, gs, os, body)

  let rec reduce_for_source : t * (t -> t) -> t =
    fun (source, body) ->
      let rs = fun source -> reduce_for_source (source, body) in
        match source with
          | Singleton v -> body v
          | Concat vs ->
            reduce_concat (List.map rs vs)
          | If (c, t, Concat []) ->
            reduce_for_source
              (t, fun v -> reduce_where_then (c, body v))
          | For (_, gs, os, v) ->
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
          | Table table ->
            let field_types = table_field_types table in
            (* we need to generate a fresh variable in order to
               correctly handle self joins *)
            let x = Var.fresh_raw_var () in
              (* Debug.print ("fresh variable: " ^ string_of_int x); *)
              reduce_for_body ([(x, source)], [], body (Var (x, field_types)))
          | v -> query_error "Bad source in for comprehension: %s" (string_of_t v)

  let rec reduce_if_body (c, t, e) =
    match t with
      | Record then_fields ->
        begin match e with
          | Record else_fields ->
            assert (StringMap.equal (fun _ _ -> true) then_fields else_fields);
            Record
              (StringMap.fold
                 (fun name t fields ->
                   let e = StringMap.find name else_fields in
                     StringMap.add name (reduce_if_body (c, t, e)) fields)
                 then_fields
                 StringMap.empty)
          (* NOTE: this relies on any record variables having
             been eta-expanded by this point *)
          | _ -> query_error "Mismatched fields"
        end
      | _ ->
        begin
          match t, e with
            | Constant (Constant.Bool true), _ ->
              reduce_or (c, e)
            | _, Constant (Constant.Bool false) ->
              reduce_and (c, t)
            | _ ->
              If (c, t, e)
        end

  let reduce_if_condition (c, t, e) =
    match c with
      | Constant (Constant.Bool true) -> t
      | Constant (Constant.Bool false) -> e
      | If (c', t', _) ->
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
end

module Q = Lang

let _show_env = Q.show_env (* Generated by ppx_deriving show *)

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
      | Q.For (_, gs, _, _body) -> generators gs
      | Q.Table ((db, _), _, _, _) -> Some db
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
      | Q.Concat vs -> comprehensions vs
      | v -> used v

let string_of_t = Q.string_of_t

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

module Eval =
struct
  let env_of_value_env policy value_env =
    let open Lang in
    { venv = value_env; qenv = Env.Int.empty; policy }

  let empty_env policy =
    let open Lang in
    { venv = Value.Env.empty; qenv = Env.Int.empty; policy }

  let (++) e1 e2 =
    let open Lang in
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
        match Var.name_of_binder (f, finfo) with
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
                Js.var_name_binder (f, finfo) ^ " in query"))
          end
      end
    | None -> None

  let find_fun env (f, fvs) =
    match lookup_fun env (f, fvs) with
    | Some v -> v
    | None ->
      raise (internal_error ("Attempt to find undefined function: " ^
        string_of_int f))

  let rec expression_of_value : Lang.env -> Value.t -> Q.t = fun env v ->
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


  let bind env (x, v) =
    let open Lang in
    { env with qenv = Env.Int.bind x v env.qenv }

  let lookup env var =
    let open Lang in
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

  let eta_expand_var (x, field_types) =
    Q.Record
      (StringMap.fold
         (fun name _t fields ->
            StringMap.add name (Q.Project (Q.Var (x, field_types), name)) fields)
         field_types
         StringMap.empty)

  let eta_expand_list xs =
    let x = Var.fresh_raw_var () in
    let field_types = Q.field_types_of_list xs in
      ([x, xs], [], Q.Singleton (eta_expand_var (x, field_types)))

  let reduce_artifacts = function
  | Q.Apply (Q.Primitive "stringToXml", [u]) ->
    Q.Singleton (Q.XML (Value.Text (Q.unbox_string u)))
  | Q.Apply (Q.Primitive "AsList", [xs]) -> xs
  | u -> u

  let check_policies_compatible env_policy block_policy =
    let open QueryPolicy in
    let resolve = function
      | Flat -> `Flat
      | Nested -> `Nested
      | Default ->
          if (Settings.get Database.shredding) then `Nested else `Flat in
    let show = function | `Nested -> "nested" | `Flat -> "flat" in
    let expected = resolve env_policy in
    let actual = resolve block_policy in
    if expected = actual then () else
      let error = Printf.sprintf
        "Incompatible query evaluation annotations. Expected %s, got %s."
        (show expected) (show actual) in
      raise (Errors.runtime_error error)

  let rec xlate env : Ir.value -> Q.t = let open Ir in function
    | Constant c -> Q.Constant c
    | Variable var ->
        begin
          match lookup env var with
            | Q.Var (x, field_types) ->
                (* eta-expand record variables *)
                eta_expand_var (x, field_types)
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
              Q.freshen_for_bindings (Env.Int.empty) v
        end
    | Extend (ext_fields, r) ->
      begin
        match opt_app (xlate env) (Q.Record StringMap.empty) r with
          | Q.Record fields ->
            Q.Record (StringMap.fold
                       (fun label v fields ->
                         if StringMap.mem label fields then
                           query_error
                             "Error adding fields: label %s already present"
                             label
                         else
                           StringMap.add label (xlate env v) fields)
                       ext_fields
                       fields)
          | _ -> query_error "Error adding fields: non-record"
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
      let open Lang in
      let (_finfo, (xs, body), z_opt, _location) = Tables.find Tables.fun_defs f in
      let z = OptionUtils.val_of z_opt in
      (* Debug.print ("Converting evalir closure: " ^ Var.show_binder (f, _finfo) ^ " to query closure"); *)
      (* yuck! *)
      let env' = bind (empty_env env.policy) (z, xlate env v) in
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
                    computation (bind env (x, tail_computation env tc)) (bs, tailcomp)
              | Fun (_, _, _, Location.Client) ->
                  query_error "Client function"
              | Fun ((f, _), _, _, _) ->
                (* This should never happen now that we have closure conversion*)
                raise (internal_error
                  ("Function definition in query: " ^ string_of_int f ^
                   ". This should have been closure-converted."))
              | Rec _ ->
                  query_error "Recursive function"
              | Alien _ -> (* just skip it *)
                  computation env (bs, tailcomp)
              | Module _ -> raise (internal_error "Not implemented modules yet")
          end
  and tail_computation env : Ir.tail_computation -> Q.t = let open Ir in function
    | Return v -> xlate env v
    | Apply (f, args) ->
        reduce_artifacts (Q.Apply (xlate env f, List.map (xlate env) args))
    | Special (Ir.Query (None, policy, e, _)) ->
        let open Lang in
        check_policies_compatible env.policy policy;
        computation env e
    | Special (Ir.Table (db, name, keys, (readtype, _, _))) as _s ->
       (** WR: this case is because shredding needs to access the keys of tables
           but can we avoid it (issue #432)? *)
       (* Copied almost verbatim from evalir.ml, which seems wrong, we should probably call into that. *)
       begin
         match xlate env db, xlate env name, xlate env keys, (TypeUtils.concrete_type readtype) with
         | Q.Database (db, params), name, keys, `Record row ->
        let unboxed_keys =
          List.map
        (fun key ->
         List.map Q.unbox_string (Q.unbox_list key))
        (Q.unbox_list keys)
        in
            Q.Table ((db, params), Q.unbox_string name, unboxed_keys, row)
         | _ -> query_error "Error evaluating table handle"
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
    | Q.Concat xs -> Q.reduce_concat (List.map (norm env) xs)
    | Q.Project (r, label) ->
      let rec project (r, label) =
        match r with
          | Q.Record fields ->
            assert (StringMap.mem label fields);
            StringMap.find label fields
          | Q.If (c, t, e) ->
            Q.If (c, project (t, label), project (e, label))
          | Q.Var (_x, field_types) ->
            assert (StringMap.mem label field_types);
            Q.Project (r, label)
          | _ -> query_error ("Error projecting from record: %s") (string_of_t r)
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
          | Q.Var (_x, field_types) ->
            assert (StringSet.subset labels (labels_of_field_types field_types));
            Q.Erase (r, labels)
          | _ -> query_error "Error erasing from record"
      in
        erase (norm env r, labels)
    | Q.Variant (label, v) -> Q.Variant (label, norm env v)
    | Q.Apply (f, xs) -> apply env (norm env f, List.map (norm env) xs)
    | Q.If (c, t, e) ->
        Q.reduce_if_condition (norm env c, norm env t, norm env e)
    | Q.Case (v, cases, default) ->
      let rec reduce_case (v, cases, default) =
        match v with
          | Q.Variant (label, v) as w ->
            begin
              match StringMap.lookup label cases, default with
                | Some ((x, _), c), _ ->
                  norm (bind env (x, v)) c
                | None, Some ((z, _), c) ->
                  norm (bind env (z, w)) c
                | None, None -> query_error "Pattern matching failed"
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
            bind env (x, arg)) xs args env in
        (* Debug.print("Applied"); *)
          norm_comp env body
    | Q.Primitive "Cons", [x; xs] ->
        Q.reduce_concat [Q.Singleton x; xs]
    | Q.Primitive "Concat", ([_xs; _ys] as l) ->
        Q.reduce_concat l
    | Q.Primitive "ConcatMap", [f; xs] ->
        begin
          match f with
            | Q.Closure (([x], body), closure_env) ->
                let env = env ++ closure_env in
                  Q.reduce_for_source
                    (xs, fun v -> norm_comp (bind env (x, v)) body)
            | _ -> assert false
        end
    | Q.Primitive "Map", [f; xs] ->
        begin
          match f with
            | Q.Closure (([x], body), closure_env) ->
                let env = env ++ closure_env in
                  Q.reduce_for_source
                    (xs, fun v -> Q.Singleton (norm_comp (bind env (x, v)) body))
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
                        eta_expand_list xs
                    | _ -> assert false in
                let xs = Q.For (None, gs, os', body) in
                  begin
                    match f with
                      | Q.Closure (([x], os), closure_env) ->
                          let os =
                            let env = bind (env ++ closure_env) (x, Q.tail_of_t xs) in
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
      Q.reduce_not (v)
    | Q.Primitive "&&", [v; w] ->
      Q.reduce_and (v, w)
    | Q.Primitive "||", [v; w] ->
      Q.reduce_or (v, w)
    | Q.Primitive "==", [v; w] ->
      Q.reduce_eq (v, w)
    | Q.Primitive f, args ->
        Q.Apply (Q.Primitive f, args)
    | Q.If (c, t, e), args ->
        Q.reduce_if_condition (c, apply env (t, args), apply env (e, args))
    | Q.Apply (f, args), args' ->
        apply env (f, args @ args')
    | t, _ -> query_error "Application of non-function: %s" (string_of_t t)

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
    match v with
      | Variant ("Repeat", pair) ->
          begin
            match unbox_pair pair with
              | Variant ("Star", _), Variant ("Any", _) -> Some ("%")
              | _ -> None
          end
      | Variant ("Simply", Constant (Constant.String s)) -> Some (quote s)
      | Variant ("Quote", Variant ("Simply", v)) ->
          (* TODO:

             detect variables and convert to a concatenation operation
             (this needs to happen in RLIKE compilation as well)
          *)
         let rec string =
            function
              | Constant (Constant.String s) -> Some s
              | Singleton (Constant (Constant.Char c)) -> Some (string_of_char c)
              | Concat vs ->
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
      | Variant ("Seq", rs) ->
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
      | Variant ("StartAnchor", _) -> Some ""
      | Variant ("EndAnchor", _) -> Some ""
      | _ -> assert false

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
              | (fields, tables, condition, []) ->
                  (fields, (table, x)::tables, condition, os)
              | _ -> assert false
          end
    | If (c, body, Concat []) ->
      (* Turn conditionals into where clauses. We might want to do
         this earlier on.  *)
      let c = base index c in
      let (fields, tables, c', os) = select_clause index unit_query body in
      let c = Sql.smart_and c c' in
      (fields, tables, c, os)
    | Table (_db, table, _keys, (fields, _, _)) ->
      (* eta expand tables. We might want to do this earlier on.  *)
      (* In fact this should never be necessary as it is impossible
         to produce non-eta expanded tables. *)
      let var = Sql.fresh_table_var () in
      let fields =
        List.rev
          (StringMap.fold
             (fun name _ fields ->
               (Sql.Project (var, name), name)::fields)
             fields
             [])
      in
        (fields, [(table, var)], Sql.Constant (Constant.Bool true), [])
    | Singleton _ when unit_query ->
      (* If we're inside an Sql.Empty or a Sql.Length it's safe to ignore
         any fields here. *)
      (* We currently detect this earlier, so the unit_query stuff here
         is redundant. *)
      ([], [], Sql.Constant (Constant.Bool true), [])
    | Singleton (Record fields) ->
      let fields =
        List.rev
          (StringMap.fold
             (fun name v fields ->
               (base index v, name)::fields)
             fields
             [])
      in
        (fields, [], Sql.Constant (Constant.Bool true), [])
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
            Sql.Apply ("LIKE", [base index s; Sql.Constant (Constant.String r)])
          | None ->
            let r =
                  (* HACK:

                     this only works if the regexp doesn't include any variables bound by the query
                  *)
                  Sql.Constant (Constant.String (Regex.string_of_regex (Linksregex.Regex.ofLinks (value_of_expression r))))
                in
                  Sql.Apply ("RLIKE", [base index s; r])
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
  Sql.UnionAll (List.map (clause [] true) (prepare_clauses v), 0)
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
    labels_of_field_types field_types
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
  in
  let get_fields = if Settings.get use_keys_in_shredding
                   then key_fields
                   else all_fields
  in concat_map (table_index get_fields) gs

let outer_index gs_out = gens_index gs_out
let inner_index z gs_in =
  (* it's just a dynamic index! *)
  (z, "2") :: gens_index gs_in

let extract_gens =
  function
    | Q.For (_, gs, _, _) -> gs
    | _ -> assert false

let let_clause : let_clause -> Sql.query =
  fun (q, outer, z, inner) ->
    let gs_out = extract_gens outer in
    let gs_in = extract_gens inner in
      Sql.With (q,
             clause (outer_index gs_out) false outer,
             z,
             clause (inner_index z gs_in) false inner)

let sql_of_let_query : let_query -> Sql.query =
  fun cs ->
    Sql.UnionAll (List.map (let_clause) cs, 0)

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
    let env = Eval.bind (Eval.env_of_value_env QueryPolicy.Default env) (x, Q.Var (x, field_types)) in
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
    let env = Eval.bind (Eval.env_of_value_env QueryPolicy.Default env) (x, Q.Var (x, field_types)) in
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

let is_list = Q.is_list
let table_field_types = Q.table_field_types
let value_of_expression = Q.value_of_expression
let default_of_base_type = Q.default_of_base_type
let type_of_expression = Q.type_of_expression
let unbox_xml = Q.unbox_xml
