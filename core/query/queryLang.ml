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
    Printf.kprintf error fmt

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

let field_types_of_spec_map =
        StringMap.map (function
          | Types.Present t -> t
          | _ -> assert false)

let field_types_of_row r =
        let (field_spec_map,_,_) = TypeUtils.extract_row_parts r in
          field_types_of_spec_map field_spec_map

let table_field_types (_, _, _, (field_spec_map, _, _)) =
        field_types_of_spec_map field_spec_map

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
  | Table (_, _, _, row) -> Types.make_list_type (Types.Record (Types.Row row))
  | Dedup u
  | Prom u -> te u
  | Constant (Constant.Bool   _) -> Types.bool_type
  | Constant (Constant.Int    _) -> Types.int_type
  | Constant (Constant.Char   _) -> Types.char_type
  | Constant (Constant.Float  _) -> Types.float_type
  | Constant (Constant.String _) -> Types.string_type
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
    | Prom _
    | Dedup _
    | If (_, _, Concat []) -> true
    | _ -> false

(** Returns which database was used if any.

   Currently this assumes that at most one database is used.
*)
let used_database v : Value.database option =
  let rec traverse = function
    | [] -> None
    | x :: xs ->
        begin
          match used x with
            | None -> traverse xs
            | Some db -> Some db
        end
  and used =
    function
      | Table ((db, _), _, _, _) -> Some db
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
      | Concat vs -> comprehensions vs
      | v -> used v

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

let (++) e1 e2 =
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
      | "dedup"
      | "distinct" ->
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