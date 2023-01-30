(*****************************************************************************
 ** queryLang.ml                                                            **
 **                                                                         **
 ** Common data types and definitions for the NRC-like sublanguage of Links **
 ** (factorised from query.ml)                                              **
 **                                                                         **
 **                                                                         **
 ** author:  Wilmer Ricciotti                                               **
 ** created: 30 Jul 2021                                                    **
 *****************************************************************************)

open Utility
open CommonTypes
open Var

exception DbEvaluationError of string

let internal_error message =
  Errors.internal_error ~filename:"query/queryLang.ml" ~message

let runtime_type_error error =
  internal_error
    ("Runtime type error: " ^ error ^ ".\n" ^
     "This should not happen if the type system / checker is correct. " ^
     "Please file a bug report.")

let query_error fmt =
  let error msg = raise (DbEvaluationError msg) in
    Printf.ksprintf error fmt

type base_type = | Bool | Char | Float | Int | String | DateTime

type tag = int
    [@@deriving show]

type t =
    | For       of tag option * (Var.var * t) list * t list * t
    | If        of t * t * t
    | Table     of Value.table
    | Database  of (Value.database * string)
    | Singleton of t
    | Concat    of t list
    | Dedup     of t
    | Prom      of t
    | Record    of t StringMap.t
    | Project   of t * string
    | Erase     of t * StringSet.t
    | Variant   of string * t
    | XML       of Value.xmlitem
    | Apply     of t * t list
    | Closure   of (Ir.var list * Ir.computation) * env
    | Case      of t * (binder * t) StringMap.t * (binder * t) option
    | Primitive of string
    | Var       of Var.var * Types.datatype
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
    | Dedup     of pt
    | Prom      of pt
    | Record    of pt StringMap.t
    | Project   of pt * string
    | Erase     of pt * StringSet.t
    | Variant   of string * pt
    | XML       of Value.xmlitem
    | Apply     of pt * pt list
    | Lam       of Ir.var list * Ir.computation
    | Case      of pt * (binder * pt) StringMap.t * (binder * pt) option
    | Primitive of string
    | Var       of (Var.var * Types.datatype)
    | Constant  of Constant.t
      [@@deriving show]
end

let rec pt_of_t : 't -> S.pt = fun v ->
  let bt = pt_of_t in
    match v with
      | For (_, gs, os, b) ->
          S.For (List.map (fun (x, source) -> (x, bt source)) gs,
                List.map bt os,
                bt b)
      | If (c, t, e) -> S.If (bt c, bt t, bt e)
      | Table t -> S.Table t
      | Singleton v -> S.Singleton (bt v)
      | Concat vs -> S.Concat (List.map bt vs)
      | Dedup q -> S.Dedup (bt q)
      | Prom q -> S.Prom (bt q)
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

let bind env (x, v) =
  { env with qenv = Env.Int.bind x v env.qenv }

let default_of_base_type =
  function
    | Primitive.Bool     -> Constant (Constant.Bool false)
    | Primitive.Int      -> Constant (Constant.Int 42)
    | Primitive.Char     -> Constant (Constant.Char '?')
    | Primitive.Float    -> Constant (Constant.Float 0.0)
    | Primitive.String   -> Constant (Constant.String "")
    | Primitive.DateTime -> Constant (Constant.DateTime.now())
    | _                  -> assert false

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
  | `DateTime dt -> Constant (Constant.DateTime dt)
  | other ->
      raise (internal_error ("expression_of_base_value undefined for " ^
        Value.string_of_value other))

let field_types_of_spec_map =
        StringMap.map (function
          | Types.Present t -> t
          | _ -> assert false)

let field_types_of_row r =
        let (field_spec_map,_,_) = TypeUtils.extract_row_parts r in
          field_types_of_spec_map field_spec_map

let table_field_types Value.Table.{ row = (fields, _, _); temporal_fields; _ } =
    (* As well as the declared fields in the table, we must also include
     * the period-stamping fields included in the temporal metadata. *)
    let dt x = (x, Types.Primitive Primitive.DateTime) in
    let metadata_fields =
        OptionUtils.opt_app (fun (x, y) -> [dt x; dt y]) [] temporal_fields
    in
    let declared_fields = field_types_of_spec_map fields in
    (* Add metadata fields *)
    StringMap.superimpose (StringMap.from_alist metadata_fields) declared_fields

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

let recdty_field_types (t : Types.datatype) : Types.datatype StringMap.t =
      field_types_of_row (TypeUtils.extract_row t)

let rec subst t x u =
  let srec t = subst t x u in
  match t with
  | Var (var, _) when var = x -> u
  | Record fl -> Record (StringMap.map srec fl)
  | Singleton v -> Singleton (srec v)
  | Concat xs -> Concat (List.map srec xs)
  | Project (r, label) -> Project (srec r, label)
  | Erase (r, labels) -> Erase (srec r, labels)
  | Variant (label, v) -> Variant (label, srec v)
  | Apply (f, xs) -> Apply (srec f, List.map srec xs)
  | For (_, gs, os, u) ->
      (* XXX: assuming fresh x!*)
      let gs' = List.map (fun (v,g) -> (v, srec g)) gs in
      let os' = List.map srec os in
      let u' = srec u in
      For (None, gs', os', u')
  | If (c, t, e) ->
      If (srec c, srec t, srec e)
  | Case (v, cases, default) ->
      let v' = srec v in
      let cases' = StringMap.map (fun (v,q) -> (v,srec q)) cases in
      let default' = default >>=? fun d -> Some (fst d, srec (snd d)) in
      Case (v', cases', default')
  | Dedup v -> Dedup (srec v)
  | Prom v -> Prom (srec v)
  | Closure (c, closure_env) ->
      let cenv = bind closure_env (x,u) in
      Closure (c, cenv)
  | v -> v

(** Returns (Some ty) if v occurs free with type ty, None otherwise *)
let occurs_free (v : Var.var) =
  let rec occf bvs = function
  | Var (w,tyw) ->
      if w = v && not (List.mem v bvs)
          then Some tyw
          else None
  | If (c,t,e) -> occf bvs c ||=? occf bvs t ||=? occf bvs e
  | Closure ((_wl,_b),_e) ->
      (* XXX: to be checked
        we use this function only in normalized queries, so there shouldn't be any closure;
        recursion on b would require deeper analysis of computations, so for the moment
        let's not implement this and hope everything works fine
      let bvs' = bvs @ wl @ List.map (fun (w,_) -> w) (Query.Eval.query_bindings_of_env e) in
      occf bvs' b ||= tryPick (fun _ q -> occf bvs q) e *)
      failwith "MixingQuery.occurs_free: unexpected Closure in query"
  | Apply (t, args) -> occf bvs t ||=? list_tryPick (occf bvs) args
  | Singleton t
  | Dedup t
  | Prom t
  | Project (t,_) -> occf bvs t
  | Concat tl -> list_tryPick (occf bvs) tl
  | For (_, gs, _os, b) ->
      (* FIXME: do we need to check os as well? *)
      let bvs'', res = List.fold_left (fun (bvs',acc) (w,q) -> w::bvs', acc ||=? occf bvs' q) (bvs, None) gs in
      res ||=? occf bvs'' b
  | Record fl -> map_tryPick (fun _ t -> occf bvs t) fl
  | _ -> None
  in occf []

(** Returns Some (x,qx,tyx) for the first generator x <- qx such that x occurs free with type tyx *)
let rec occurs_free_gens (gs : (Var.var * t) list) q =
  match gs with
  | [] -> None
  | (x,qx)::gs' ->
      match occurs_free x (For (None, gs', [], q)) with
      | Some tyx -> Some (x,qx,tyx)
      | None -> occurs_free_gens gs' q

(** Return the type associated with an expression *)
(* Inferring the type of an expression is straightforward because all
    variables are annotated with their types. *)
let rec type_of_expression : t -> Types.datatype = fun v ->
  let te = type_of_expression in
  let record fields : Types.datatype =
    Types.make_record_type (StringMap.map te fields)
  in
  match v with
  | Var (_,ty) -> ty
  | Concat [] -> Types.make_list_type(Types.unit_type)
  | Concat (v::_) -> te v
  | For (_, _, _os, body) -> te body
  | Singleton t -> Types.make_list_type (te t)
  | Record fields -> record fields
  | If (_, t, _) -> te t
  | Table Value.Table.{ row; _ } -> Types.make_list_type (Types.Record (Types.Row row))
  | Dedup u
  | Prom u -> te u
  | Constant (Constant.Bool   _) -> Types.bool_type
  | Constant (Constant.Int    _) -> Types.int_type
  | Constant (Constant.Char   _) -> Types.char_type
  | Constant (Constant.Float  _) -> Types.float_type
  | Constant (Constant.String _) -> Types.string_type
  | Constant (Constant.DateTime _) -> Types.datetime_type
  | Project (w, name) ->
      begin
        match te w with
        | Types.Record _ as rty -> StringMap.find name (recdty_field_types rty)
        | ty ->
            failwith
              (Format.asprintf ("term:\n" ^^
                  "%s\n" ^^
                  "has type:\n" ^^
                  "%a\n" ^^
                  "but it was expected to have a record type.")
                (string_of_t w) Types.pp_datatype ty)
      end
  | Apply (Primitive "Empty", _) -> Types.bool_type (* HACK *)
  | Apply (Primitive "Distinct", [q]) -> type_of_expression q
  | Apply (Primitive f, _) -> TypeUtils.return_type (Env.String.find f Lib.type_env)
  | e -> Debug.print("Can't deduce type for: " ^ show e); assert false

let eta_expand_var (x, ty) =
  match ty with
  | Types.Record row ->
      let field_types = field_types_of_row row in
      Record
        (StringMap.fold
          (fun name _t fields ->
              StringMap.add name (Project (Var (x, ty), name)) fields)
          field_types
          StringMap.empty)
  | _ -> Var (x, ty)

let eta_expand_list xs =
  let x = Var.fresh_raw_var () in
  let ty = TypeUtils.element_type ~overstep_quantifiers:true (type_of_expression xs) in
    (* Debug.print ("eta_expand_list create: " ^ show (Var (x, ty))); *)
    ([x, xs], [], Singleton (eta_expand_var (x, ty)))

(* takes a normal form expression and returns true iff it has list type *)
let is_list =
  function
    | For _
    | Table _
    | Singleton _
    | Concat _
    | Prom _
    | Dedup _
    | If (_, _, Concat []) -> true
    | _ -> false

(** Returns which database was used if any.

   Currently this assumes that at most one database is used.
*)
let used_database : t -> Value.database option =
  let rec traverse = function
    | [] -> None
    | x :: xs ->
        begin
          match used_item x with
            | None -> traverse xs
            | Some db -> Some db
        end
  and used_item =
    function
      | Prom q -> used q
      | Dedup q -> used_item q
      | Table Value.Table.{ database = (db, _); _ } -> Some db
      | For (_, gs, _, _body) -> List.map snd gs |> traverse
      | Singleton v -> used v
      | Record v ->
          StringMap.to_alist v
          |> List.map snd
          |> traverse
      | Apply (_, args) ->
          (* Functions will be normalised, so only need to traverse the args. *)
          traverse args
      | If (i, t, e) -> traverse [i; t; e]
      | Case (scrutinee, cases, default) ->
          let cases = StringMap.to_alist cases |> List.map (snd ->- snd) in
          let default = OptionUtils.opt_app (fun (_, x) -> [x]) [] default in
          traverse (scrutinee :: (cases @ default))
      | Erase (x, _) -> used x
      | Variant (_, x) -> used x
      | _ -> None
  and used =
    function
      | Concat vs -> traverse vs
      | v -> used_item v
  in used

let string_of_t = string_of_t

let labels_of_field_types field_types =
  StringMap.fold
    (fun name _ labels' ->
      StringSet.add name labels')
    field_types
    StringSet.empty

let recdty_field_types (t : Types.datatype) : Types.datatype StringMap.t =
  field_types_of_row (TypeUtils.extract_row t)

let env_of_value_env policy value_env =
  { venv = value_env; qenv = Env.Int.empty; policy }

let empty_env policy =
  { venv = Value.Env.empty; qenv = Env.Int.empty; policy }

let append_env e1 e2 =
  if (e1.policy <> e2.policy) then
    raise (internal_error "Trying to append environments with different query policies")
  else
    let venv = Value.Env.shadow e1.venv ~by:e2.venv in
    let qenv = Env.Int.extend e1.qenv e2.qenv in
    { policy = e1.policy; venv; qenv }

let lookup_fun env (f, fvs) =
  match Tables.lookup Tables.fun_defs f with
  | Some (finfo, (xs, body), z, location) ->
    Some
      begin
        (* TODO(dhil): This is a bit of a round-about way to obtain
            the binder name. *)
      match Var.(name_of_binder (make_binder f finfo)) with
      | "dedup" ->
        Primitive "Distinct"
      | "concatMap" ->
        Primitive "ConcatMap"
      | "map" ->
        Primitive "Map"
      | "empty" ->
        Primitive "Empty"
      | "sortByBase" ->
        Primitive "SortBy"
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

let rec expression_of_value : env -> Value.t -> t = fun env v ->
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
        Record
          (List.fold_left
              (fun fields (name, v) -> StringMap.add name (expression_of_value env v) fields)
              StringMap.empty
              fields)
    | `Variant (name, v) -> Variant (name, expression_of_value env v)
    | `XML xmlitem -> XML xmlitem
    | `FunctionPtr (f, fvs) -> find_fun env (f, fvs)
    | `PrimitiveFunction (f,_) -> Primitive f
    | `DateTime dt -> Constant (Constant.DateTime dt)
    | v ->
        raise (internal_error (Printf.sprintf
            "Cannot convert value %s to expression" (Value.string_of_value v)))


let bind env (x, v) =
  { env with qenv = Env.Int.bind x v env.qenv }

let lookup env var =
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

let check_policies_compatible env_policy block_policy =
  if env_policy != block_policy
  then
    let error = Printf.sprintf
        "Incompatible query evaluation annotations. Expected %s, got %s."
        (QueryPolicy.show env_policy) (QueryPolicy.show block_policy) in
    raise (Errors.runtime_error error)

(* convert a regexp to a like if possible *)
  let rec likeify v =
    let quote = Str.global_replace (Str.regexp_string "%") "\\%" in
    let append x y = Apply (Primitive "^^",  [x; y]) in
    let str x = Constant (Constant.String x) in
      match v with
        | Variant ("Repeat", pair) ->
            begin
              match unbox_pair pair with
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

let rec select_clause : Sql.index -> bool -> t -> Sql.select_clause =
  fun index unit_query v ->
  (*  Debug.print ("select_clause: "^string_of_t v); *)
  match v with
    | Concat _ -> assert false
    | For (_, [], _, body) ->
        select_clause index unit_query body
    | For (_, (x, Table Value.Table.{ name; _ })::gs, os, body) ->
        let body = select_clause index unit_query (For (None, gs, [], body)) in
        let os = List.map (base index) os in
          begin
            match body with
              | (_, fields, tables, condition, []) ->
                  (Sql.All, fields, Sql.TableRef(name, x)::tables, condition, os)
              | _ -> assert false
          end
    | If (c, body, Concat []) ->
      (* Turn conditionals into where clauses. We might want to do
         this earlier on.  *)
      let c = base index c in
      let (_, fields, tables, c', os) = select_clause index unit_query body in
      let c = Sql.smart_and c c' in
      (Sql.All, fields, tables, c, os)
    | Table Value.Table.{ name = table; row = (fields, _, _); _ } ->
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
and clause : Sql.index -> bool -> t -> Sql.query =
  fun index unit_query v -> Sql.Select(select_clause index unit_query v)
and base : Sql.index -> t -> Sql.base = fun index ->
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
    | Project (Var (x, _tyx), name) ->
        Sql.Project (x, name)
    | Constant c -> Sql.Constant c
    | Primitive "index" ->
        (* This is the only place the index parameter is ever materially used. *)
        Sql.RowNumber index
    | e ->
      Debug.print ("Not a base expression: " ^ show e);
      assert false

and unit_query v =
  let prepare_clauses : t -> t list =
    function
      | Concat vs -> vs
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

type let_clause = Var.var * t * Var.var * t
type let_query = let_clause list


let gens_index (gs : (Var.var * t) list)   =
  let open Value.Table in
  let all_fields t =
    let field_types = table_field_types t in
    labels_of_field_types field_types
  in
 (* Use keys if available *)
  let key_fields t =
    match t.keys with
      | (ks::_) -> StringSet.from_list ks
      | _ -> all_fields t
  in
  let table_index (x, source) =
    let t = match source with Table t -> t | _ -> assert false in
    let labels = key_fields t in
      List.rev
        (StringSet.fold
           (fun name ps -> (x, name) :: ps)
           labels
           [])
  in concat_map (table_index) gs

let outer_index gs_out = gens_index gs_out
let inner_index z gs_in =
  (* it's just a dynamic index! *)
  (z, "2") :: gens_index gs_in

let extract_gens =
  function
    | For (_, gs, _, _) -> gs
    | _ -> assert false

let let_clause : let_clause -> Sql.query =
  fun (q, outer, t, inner) ->
    let gs_out = extract_gens outer in
    let gs_in = extract_gens inner in
    let q_outer = clause (outer_index gs_out) false outer in
    let (_fDist, result,tables,where,os) = select_clause (inner_index t gs_in) false inner in
    let tablename = Sql.string_of_subquery_var q in
    let q_inner = Sql.Select(Sql.All,result,Sql.TableRef(tablename,t)::tables,where,os) in
    Sql.With (tablename, q_outer, [q_inner])

let sql_of_let_query : let_query -> Sql.query =
  fun cs ->
    Sql.Union (Sql.All, List.map (let_clause) cs, 0)

let update : ((Ir.var * string) * t option * t) -> Sql.query =
  fun ((_, table), where, body) ->
    let open Sql in
    let upd_where =
      OptionUtils.opt_map (base []) where in
    let upd_fields =
      unbox_record body
      |> StringMap.map (base [])
      |> StringMap.to_alist in
    Update { upd_table = table; upd_fields; upd_where }

let delete : ((Ir.var * string) * t option) -> Sql.query =
  fun ((_, table), where) ->
    let open Sql in
    let del_where = OptionUtils.opt_map (base []) where in
    Delete { del_table = table; del_where }

let insert table_name field_names rows =
  let rows = List.map (List.map (expression_of_base_value ->- base [])) rows in
  Sql.(Insert {
      ins_table = table_name;
      ins_fields = field_names;
      ins_records = Values rows })


module type QUERY_VISITOR =
sig
  class visitor :
  object ('self_type)
    method query : t -> ('self_type * t)
    method tag : tag -> ('self_type * tag)
    method binder : binder -> ('self_type * binder)

    method option :
      'a.
        ('self_type -> 'a -> ('self_type * 'a)) ->
        'a option ->
        ('self_type * ('a option))

    method list :
      'a.
        ('self_type -> 'a -> ('self_type * 'a)) ->
        'a list ->
        ('self_type * ('a list))
  end
end


module Transform : QUERY_VISITOR =
struct
  class visitor =
  object ((o : 'self_type))
    method tag x = (o, x)

    method binder x = (o, x)

    method option :
      'a.
        ('self_type -> 'a -> ('self_type * 'a)) ->
        'a option ->
        ('self_type * ('a option)) =
      fun f x ->
        match x with
          | Some x ->
              let (o, x) = f o x in
              (o, Some x)
          | None -> (o, None)

    method list :
        'a.
        ('self_type -> 'a -> ('self_type * 'a)) ->
        'a list ->
        ('self_type * ('a list))
      = fun f xs ->
      List.fold_right (fun x (o, acc) ->
        let (o, x) = f o x in
        (o, x :: acc)) xs (o, [])

    method query =
      function
      | For (tag_opt, gs, os, body) ->
          let (o, tag_opt) = o#option (fun o -> o#tag) tag_opt in
          let (o, gs) =
            o#list (fun o (v, t) ->
              let (o, t) = o#query t in
              (o, (v, t))) gs in
          let (o, os) = o#list (fun o -> o#query) os in
          let (o, body) = o#query body in
          (o, For (tag_opt, gs, os, body))
      | If (i, t, e) ->
          let (o, i) = o#query i in
          let (o, t) = o#query t in
          let (o, e) = o#query e in
          (o, If (i, t, e))
      | Table t -> (o, Table t)
      | Database  (dt, s) -> (o, Database (dt, s))
      | Singleton x -> let (o, x) = o#query x in (o, Singleton x)
      | Concat xs -> let (o, xs) = o#list (fun o -> o#query) xs in (o, Concat xs)
      | Dedup q ->
          let (o, q) = o#query q in
          (o, Dedup q)
      | Prom q ->
          let (o, q) = o#query q in
          (o, Prom q)
      | Record fields ->
        let (o, fields) =
          StringMap.fold (fun k v (o, acc)->
            let (o, v) = o#query v in
            (o, StringMap.add k v acc)) fields (o, StringMap.empty) in
        (o, Record fields)
      | Project (x, field) -> let (o, x) = o#query x in (o, Project (x, field))
      | Erase  (x, fields) ->
        let (o, x) = o#query x in
        (o, Erase (x, fields))
      | Variant (v, x) ->
          let (o, x) = o#query x in
          (o, Variant (v, x))
      | XML x -> (o, XML x)
      | Apply (f, args) ->
          let (o, f) = o#query f in
          let (o, args) = o#list (fun o -> o#query) args in
          (o, Apply (f, args))
      | Closure ((var, comp), env) -> (o, Closure ((var, comp), env))
      | Case (x, cases, default) ->
          let (o, x) = o#query x in
          let (o, cases) =
            StringMap.fold (fun k (bnd, body) (o, acc) ->
              let (o, bnd) = o#binder bnd in
              let (o, body) = o#query body in
              (o, StringMap.add k (bnd, body) acc)) cases (o, StringMap.empty) in
          let (o, default) =
            o#option (fun o (bnd, x) ->
              let (o, bnd) = o#binder bnd in
              let (o, x) = o#query x in
              (o, (bnd, x))) default in
          (o, Case (x, cases, default))
      | Primitive x -> (o, Primitive x)
      | Var (v, dts) -> (o, Var (v, dts))
      | Constant c -> (o, Constant c)
  end
end
