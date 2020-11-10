open Utility
open CommonTypes
open Var
open Errors

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
      (* | Var       of Var.var * Types.datatype StringMap.t *)
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
      (* | Var       of (Var.var * (Types.datatype StringMap.t) option) *)
      | Var       of (Var.var * Types.datatype)
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

  (* let rec tail_of_t : t -> t = fun v ->
    let tt = tail_of_t in
      match v with
        | For (_, _gs, _os, Singleton (Record fields)) -> Record fields
        | For (_tag, _gs, _os, If (_, t, Concat [])) -> tt (For (_tag, _gs, _os, t))
        | _ -> Debug.print ("v: "^string_of_t v); assert false *)

  let rec tail_of_t : t -> t = fun v ->
    let tt = tail_of_t in
      match v with
        | Singleton (Record fields) -> Record fields
        | If (_, t, Concat []) -> tt t
        | For (_, _gs, _os, t) -> tt t
        | _ -> (* Debug.print ("v: "^string_of_t v); *) assert false

  let bind env (x, v) =
    { env with qenv = Env.Int.bind x v env.qenv }   


  let field_types_of_spec_map =
    StringMap.map (function
      | Types.Present t -> t
      | _ -> assert false)
  
  let field_types_of_row r =
    let (field_spec_map,_,_) = TypeUtils.extract_row_parts r in
      field_types_of_spec_map field_spec_map
    
  let table_field_types (_, _, _, (field_spec_map, _, _)) =
    field_types_of_spec_map field_spec_map
                  
  let labels_of_field_types field_types =
    StringMap.fold
      (fun name _ labels' ->
        StringSet.add name labels')
      field_types
      StringSet.empty

  let recdty_field_types (t : Types.datatype) : Types.datatype StringMap.t =
    field_types_of_row (TypeUtils.extract_row t)

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

  let type_of_for_var gen = 
    type_of_expression gen
    |> Types.unwrap_list_type
  

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
      let rec var_lookup x = 
        match Env.Int.find_opt x env with
        | None -> x
        | Some y -> var_lookup y
      in
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
      | Dedup q -> ffb q
      | Prom q -> ffb q
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
      | Var (x, ts) as _v ->
        (* begin
          match Env.Int.find_opt x env with
          | None -> v (* Var (x, ts) *)
          | Some y -> Var (y, ts)
        end *)
          Var (var_lookup x, ts)
      | Constant c -> Constant c

  let flatfield f1 f2 = f1 ^ "@" ^ f2

  let rec flattened_pair x y = 
    match x, y with
    | Var (_nx, Types.Record row), _ ->
        let x' = Record (StringMap.fold (fun f _ acc -> StringMap.add f (Project (x,f)) acc) (field_types_of_row row) StringMap.empty)
        in flattened_pair x' y
    | _, Var (_ny, Types.Record row) ->
        let y' = Record (StringMap.fold (fun f _ acc -> StringMap.add f (Project (y,f)) acc) (field_types_of_row row) StringMap.empty)
        in flattened_pair x y'
    (* XXX: using a field with an empty name to deal with variables of non-record type ... will it work? *)
    | Var (_nx, _), _ ->
        let x' = Record (StringMap.from_alist ["",x])
        in flattened_pair x' y
    | _, Var (_ny, _) ->
        let y' = Record (StringMap.from_alist ["",y])
        in flattened_pair x y'
    | Record fty1, Record fty2 ->
        let out1 = 
            StringMap.fold (fun f v acc -> StringMap.add (flatfield "1" f) v acc) fty1 StringMap.empty
        in 
        let out2 = StringMap.fold (fun f v acc -> StringMap.add (flatfield "2" f) v acc) fty2 out1
        in Record out2
    | _ -> assert false

  let rec flattened_pair_ft x y = 
    match x, y with
    | Var (_nx, Types.Record rowx), Var (_ny, Types.Record rowy) -> 
        let out1 = 
            StringMap.fold (fun f t acc -> StringMap.add (flatfield "1" f) t acc) (field_types_of_row rowx) StringMap.empty
        in 
        StringMap.fold (fun f t acc -> StringMap.add (flatfield "2" f) t acc) (field_types_of_row rowy) out1
    (* XXX: same as above, using a field with an empty name to deal with variables of non-record type ... will it work? *)
    | Var (nx, tyx), _ -> flattened_pair_ft (Var (nx, Types.make_record_type (StringMap.from_alist ["", tyx]))) y
    | _, Var (ny, tyy) -> flattened_pair_ft x (Var (ny, Types.make_record_type (StringMap.from_alist ["", tyy])))
    | _ -> assert false

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

  (* let rec field_types_of_list =
    function
      | Concat (v::_) -> field_types_of_list v
      | Singleton (Record fields) -> StringMap.map type_of_expression fields
      | Table table -> table_field_types table
      | _ -> assert false *)

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

  (* gs must ALWAYS be non-empty, both input and output!*)
  let expand_collection = function
  | For (_, gs, os', body) -> gs, os', body
  | Concat (_::_)
  | Singleton _
  | Prom _ 
  | Table _ as xs ->
      (* I think we can omit the `Table case as it
         can never occur *)
      (* eta-expand *)
      eta_expand_list xs
  | _ -> assert false
  
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
    match c, t with
    (* optimisation *)
    | Constant (Constant.Bool true), _
    | _, Constant (Constant.Bool true) -> t
    | Constant (Constant.Bool false), _
    | _, Constant (Constant.Bool false) -> Concat []

    | _, Concat vs ->
        reduce_concat (List.map (fun v -> reduce_where_then (c, v)) vs)
    | _, Prom t' -> Prom (reduce_where_then (c, t'))
    | _, For (_, gs, os, body) ->
        For (None, gs, os, reduce_where_then (c, body))
    | _, If (c', t', Concat []) ->
        reduce_where_then (reduce_and (c, c'), t')
    | _ ->
        If (c, t, Concat [])

  let reduce_for_body (gs, os, body) =
    match body with
      (* | Concat []                 -> body *)
      | For (_, gs', os', body') -> For (None, gs @ gs', os @ os', body')
      (* | Prom _ as u               ->           
            let z = Var.fresh_raw_var () in
            let tyz = type_of_expression u in
            let ftz = recdty_field_types (Types.unwrap_list_type tyz) in
            let vz = Var (z, ftz) in
            For (None, gs @ [(z, u)], [] (* os *), (Singleton vz)) *)
      (* make sure when we reach this place, gs can NEVER be empty 
        | _ when gs = [] (* && _os = [] *) -> body *)
      | _                         -> For (None, gs, os, body)

  let rec reduce_for_source : env -> var * t * Types.datatype -> (env -> (t list -> t list) -> t) -> t =
    fun env (x, source, ty) body ->
       let empty_os = fun os -> os in
       let add_os os = fun os' -> os@os' in 
       let rs = fun gen' -> reduce_for_source env gen' body in
        match source with
          | Singleton v ->
            begin
              let env' = bind env (x, v) in
              match body env' empty_os with
              (* the normal form of a For does not have Prom in its body
                 --> we hoist it to a generator *)
              | Prom _ as q -> 
                  let z = Var.fresh_raw_var () in
                  let tyq = type_of_expression q in
                  (* Debug.print ("reduce_for_source.Singleton fresh var: " ^ show (Var (z,tyq))); *)
                  reduce_for_body ([(z,q)], [], Singleton (eta_expand_var (z, tyq)))
              | q -> q
            end
          | Concat vs ->
            reduce_concat (List.map (fun s -> rs (x,s,ty)) vs)
          | If (c, t, Concat []) ->
            reduce_for_source env (x, t, ty) (fun env' os_f -> reduce_where_then (c, body env' os_f))
          | For (_, gs, os', v) ->
            (* NOTE:

               We are relying on peculiarities of the way we manage
               the environment in order to avoid having to
               augment it with the generator bindings here.

               In particular, we rely on the fact that if a variable
               is not found on a lookup then we return the eta
               expansion of that variable rather than complaining that
               it isn't bound in the environment.

            *)
            let tyv = type_of_expression v in
            (* this ensures os' is added to the right of the final comprehension, and further inner orderings to the right of os' *)
            let body' = fun env' os_f -> body env' (os_f ->- add_os os') in
            reduce_for_body (gs, [], reduce_for_source env (x,v,tyv) body')
          | Table (_, _, _, row)
          | Dedup (Table (_, _, _, row)) ->
              (* we need to generate a fresh variable in order to
                correctly handle self joins *)
              let y = Var.fresh_raw_var () in
              let ty_elem = Types.Record (Types.Row row) in
              (* Debug.print ("reduce_for_source.Table fresh var: " ^ string_of_int y ^ " for " ^ string_of_int x); *)
              let env' = bind env (x, Var (y, ty_elem)) in
              (* Debug.print ("reduce_for_source.Table body before renaming: " ^ show (body env empty_os)); *)
              let body' = body env' empty_os in
              (* Debug.print ("reduce_for_source.Table body after renaming: " ^ show body'); *)
              reduce_for_body ([(y, source)], [], body')
          | Prom _ -> 
              let y = Var.fresh_raw_var () in 
              let ty_elem = type_of_for_var source in 
              (* Debug.print ("reduce_for_source.Prom fresh var: " ^ string_of_int y); *)
              let env' = bind env (x, Var (y, ty_elem)) in
              let body' = body env' empty_os in
              reduce_for_body ([(y,source)], [], body')
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
      | Q.Table ((db, _), _, _, _) -> Some db
      | Q.For (_, gs, _, _body) -> List.map snd gs |> traverse
      | Q.Singleton v -> used v
      | Q.Record v ->
          StringMap.to_alist v
          |> List.map snd
          |> traverse
      | Q.Apply (_, args) ->
          (* Functions will be normalised, so only need to traverse the args. *)
          traverse args
      | Q.If (i, t, e) -> traverse [i; t; e]
      | Q.Case (scrutinee, cases, default) ->
          let cases = StringMap.to_alist cases |> List.map (snd ->- snd) in
          let default = OptionUtils.opt_app (fun (_, x) -> [x]) [] default in
          traverse (scrutinee :: (cases @ default))
      | Q.Erase (x, _) -> used x
      | Q.Variant (_, x) -> used x
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

module Eval =
struct
  let env_of_value_env policy value_env =
    let open Lang in
    { venv = value_env; qenv = Env.Int.empty; policy }

  let empty_env policy =
    let open Lang in
    { venv = Value.Env.empty; qenv = Env.Int.empty; policy }

  let query_bindings_of_env e =
    let open Lang in 
    Env.Int.bindings (e.qenv)

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
        match Var.(name_of_binder (make_binder f finfo)) with
        | "dedup"
        | "distinct" ->
          Q.Primitive "Distinct"
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
  
  let reduce_artifacts = function
  | Q.Apply (Q.Primitive "stringToXml", [u]) ->
    Q.Singleton (Q.XML (Value.Text (Q.unbox_string u)))
  | Q.Apply (Q.Primitive "AsList", [xs]) -> xs
  | Q.Apply (Q.Primitive "Distinct", [u]) -> Q.Prom (Q.Dedup u) 
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
            | Q.Var (x, tyx) ->
                (* eta-expand record variables *)
                Q.eta_expand_var (x, tyx)
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
                    computation (Q.bind env (x, tail_computation env tc)) (bs, tailcomp)
              | Fun {fn_location = Location.Client; _} ->
                  query_error "Client function"
              | Fun {fn_binder = b; _} ->
                 let f = Var.var_of_binder b in
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
       (* WR: this case is because shredding needs to access the keys of tables
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
  
  (** returns true iff the input query contains any free variable in the list;
   * the query is assumed to be in normal form! *)
  let contains_free fvs =
    let rec cfree bvs = function
    | Q.Var (w,_tyw) -> List.mem w fvs && not (List.mem w bvs)
    | Q.If (c,t,e) -> cfree bvs c || cfree bvs t || cfree bvs e
    | Q.Closure ((wl,b),e) -> cfree (bvs@wl) (computation e b)
    | Q.Apply (t, args) -> cfree bvs t || List.exists (cfree bvs) args
    | Q.Singleton t
    | Q.Dedup t
    | Q.Prom t
    | Q.Project (t,_) -> cfree bvs t
    | Q.Erase (t,_) -> 
        (* for well-typed normal forms, this is NOT an overapproximation *)
        cfree bvs t
    | Q.Variant (_, t) -> cfree bvs t
    | Q.Concat tl -> List.exists (cfree bvs) tl
    | Q.For (_, gs, os, b) -> 
        let bvs'', res = List.fold_left (fun (bvs',acc) (w,q) -> w::bvs', acc || cfree bvs' q) (bvs, false) gs in
        res || cfree bvs'' b || List.exists (cfree bvs) os
    | Q.Record fl -> StringMap.exists (fun _ t -> cfree bvs t) fl
    | _ -> false
    in cfree []

  let mk_for_term env (x,xs) body_f =
    let ty_elem = 
      Q.type_of_expression xs
      |> TypeUtils.element_type ~overstep_quantifiers:true
    in
    (* let newx = Var.fresh_raw_var () in *)
    let vx = Q.Var (x, ty_elem) in
    let cenv = Q.bind env (x, vx) in
    (* Debug.print ("mk_for_term: " ^ string_of_int newx ^ " for " ^ string_of_int x); *)
    Q.For (None, [x,xs], [], body_f cenv)

  let rec norm in_dedup env : Q.t -> Q.t =
    function
    (* XXX: this is a quick and dirty fix to implement substitution into a term via normalization:
       normally at this point free variables should already have been substituted (by xlate)
       but sometimes we use norm with open terms and a specially crafted env to perform substitution.
       It should be tested, and may be more complicated than required because it was ported from xlate *)
    | Q.Var (var, _) as orig ->
        begin
          try
            match lookup env var with
              (* XXX it should never be in_dedup, should it? *)
              | Q.Var (x, tyx) when not in_dedup ->
                  (* eta-expand record variables *)
                  Q.eta_expand_var (x, tyx)
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
                  (* Q.freshen_for_bindings Env.Int.empty (retn in_dedup v) *)
                  Q.freshen_for_bindings Env.Int.empty (norm in_dedup env v)
          with
          | InternalError _ -> retn in_dedup orig
        end
    | Q.Record fl -> Q.Record (StringMap.map (norm false env) fl)
    | Q.Singleton v -> Q.Singleton (norm false env v)
    | Q.Concat xs -> Q.reduce_concat (List.map (norm in_dedup env) xs)
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
            | _ -> query_error ("Error projecting label %s from record: %s") label (string_of_t r)
        in
        retn in_dedup (project (norm false env r, label))
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
          | _ -> query_error "Error erasing from record"
        in
        erase (norm false env r, labels)
    | Q.Variant (label, v) -> Q.Variant (label, norm false env v)
    | Q.Apply (f, xs) as _orig -> 
      apply in_dedup env (norm false env f, List.map (norm false env) xs)
    | Q.For (_, gs, os, u) as _orig -> 
        (* Debug.print ("norm.For: " ^ Q.show _orig); *)
        let rec reduce_gs env os_f body = function
        | [] -> 
          begin
            let open Q in (* XXX remove *)
            match norm in_dedup env body with
            | Q.For (_, gs', os', u') ->
                reduce_gs env (os_f -<- (fun os'' -> os'@os'')) u' gs'
            (* this special case allows us to hoist a non-standard For body into a generator *)
            | Q.Prom _ as u' ->
                let z = Var.fresh_raw_var () in
                let tyz = 
                  Q.type_of_expression u'
                  |> TypeUtils.element_type 
                in
                let vz = Q.Var (z, tyz) in
                (* Debug.print ("norm.For fresh var: " ^ Q.show vz); *)
                Q.reduce_for_source env (z, u', tyz) (fun env' os_f' -> 
                  Q.For (None, [], List.map (norm false env') (os_f' (os_f [])),
                    norm in_dedup env' (Q.Singleton vz)))
            | u' -> 
              (* Q.For (None, [], List.map (norm false env) (os_f []), u') *)
              let os' = os_f [] in
              (* let domenv = Env.Int.fold (fun x _ l -> x::l) env.qenv [] in
              Debug.print(">>> final env variables: " ^ String.concat ", "  (List.map string_of_int domenv));
              Debug.print(">>> final input os: " ^ String.concat ", " (List.map Q.show os));
              Debug.print(">>> final u': " ^ Q.show u'); *)
              let os'' = List.map (norm false env) os' in
              Q.For (None, [], os'', u')
          end
        | (x,g)::gs' -> (* equivalent to xs = For gs' u, body = g, but possibly the arguments aren't normalized *)
            let tyg = Q.type_of_expression g in
            Q.reduce_for_source env (x, norm in_dedup env g, tyg) (fun env' os_f' -> reduce_gs env' (os_f -<- os_f') body gs')
        in
        (* Debug.print(">>> env variables: " ^ String.concat ", "  (List.map string_of_int (Env.Int.fold (fun x _ l -> x::l) env.qenv [])));
        Debug.print(">>> os :" ^ String.concat ", " (List.map Q.show os)); *)
        reduce_gs env (fun os' -> os@os') u gs
    | Q.If (c, t, e) ->
        Q.reduce_if_condition (norm false env c, norm in_dedup env t, norm in_dedup env e)
    | Q.Case (v, cases, default) ->
      let rec reduce_case (v, cases, default) =
        match v with
        | Q.Variant (label, v) as w ->
           begin
             match StringMap.lookup label cases, default with
             | Some (b, c), _ ->
                let x = Var.var_of_binder b in
                norm in_dedup (Q.bind env (x, v)) c
             | None, Some (b, c) ->
                let z = Var.var_of_binder b in
                norm in_dedup (Q.bind env (z, w)) c
             | None, None -> query_error "Pattern matching failed"
           end
        | Q.If (c, t, e) ->
           Q.If
             (c,
              reduce_case (t, cases, default),
              reduce_case (e, cases, default))
        |  _ -> assert false
      in
      reduce_case (norm false env v, cases, default)
    | Q.Dedup v -> norm true env v
    | Q.Prom v when in_dedup -> norm false env v
    | Q.Prom v (* when not in_dedup *) ->
        Q.Prom (norm false env v)
    | v -> retn in_dedup v

  and apply in_dedup env : Q.t * Q.t list -> Q.t = function
    | Q.Closure ((xs, body), closure_env), args ->
      (* Debug.print ("Applying closure"); *)
      (* Debug.print ("body: " ^ Ir.show_computation body); *)
      (* Debug.print("Applying query closure: " ^ show_t (`Closure ((xs, body), closure_env))); *)
      (* Debug.print("args: " ^ mapstrcat ", " Q.show args); *)
        let env = env ++ closure_env in
        let env = List.fold_right2 (fun x arg env ->
            Q.bind env (x, arg)) xs args env in
        (* Debug.print("Applied"); *)
          norm_comp in_dedup env body
    | Q.Primitive "Cons", [x; xs] ->
        norm in_dedup env (Q.Concat [Q.Singleton x; xs])
    | Q.Primitive "Concat", ([_xs; _ys] as l) ->
        norm in_dedup env (Q.Concat l)
    | Q.Primitive "ConcatMap", [f; xs] ->
        begin
          match f with
            | Q.Closure (([x], body), closure_env) ->
                (* Debug.print ("Applying ConcatMap");
                Debug.print ("f: " ^ Q.show f);
                Debug.print ("xs: " ^ Q.show xs); *)
                (fun cenv -> computation cenv body)
                |> mk_for_term (env ++ closure_env) (x,xs)
                |> norm in_dedup env
            | _ -> assert false
        end
    | Q.Primitive "Map", [f; xs] ->
        begin
          match f with
            | Q.Closure (([x], body), closure_env) ->
                (fun cenv -> Q.Singleton (computation cenv body))
                |> mk_for_term (env ++ closure_env) (x,xs)
                |> norm in_dedup env
            | _ -> assert false
        end
    | Q.Primitive "SortBy", [f; xs] ->
        begin
          match xs with
            | Q.Concat [] -> Q.Concat []
            | _ ->
                let gs, os', body = Q.expand_collection xs in
                let xs = Q.For (None, gs, os', body) in
                begin
                  match f with
                  | Q.Closure (([x], os), closure_env) ->
                      let os =
                        let cenv = Q.bind (env ++ closure_env) (x, Q.tail_of_t (* xs *) body) in
                          
                          let o = norm_comp false cenv os in
                            match o with
                              | Q.Record fields ->
                                  List.rev (StringMap.fold (fun _ o os -> o::os) fields [])
                              | _ -> assert false
                      in
                      (* this is unsmart: everything is normalized here, but we have to potentially
                         propagate an external in_dedup, which requires to scan the full term again;
                         the easiest way to do that is to call again norm, but maybe we should use an
                         auxiliary do_dedup function *)
                      let out = Q.For (None, gs, os @ os', body) in
                      (* Debug.print ("Query.norm SortBY generates: " ^ Q.show out); *)
                      if in_dedup 
                        then norm true env out 
                        else out
                  | _ -> assert false
                end
        end
    | Q.Primitive "Distinct", [v] -> Q.Prom (norm true env v)
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
        Q.reduce_if_condition (c, apply in_dedup env (t, args), apply in_dedup env (e, args))
    | Q.Apply (f, args), args' ->
        apply in_dedup env (f, args @ args')
    | t, _ -> query_error "Application of non-function: %s" (string_of_t t)

  and norm_comp in_dedup env c = 
    computation env c
    |> norm in_dedup env
  and retn in_dedup u = if in_dedup then Q.Dedup u else u

  (* specialize norm_* with in_dedup = false at the start of normalization *)
  (* (norm is currently unused outside query.ml, so we comment the following) *)
  let norm = norm false
  let norm_comp = norm_comp false

  let eval policy env e =
    Debug.debug_time "Query.eval" (fun () ->
      (* let res = *)
      norm_comp (env_of_value_env policy env) e
      (* in
      Debug.print ("eval returned: " ^ Q.show res);
      res *)
      )
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
              | (_, fields, tables, condition, []) ->
                  (false, fields, Sql.TableRef(table, x)::tables, condition, os)
              | _ -> assert false
          end
    | If (c, body, Concat []) ->
      (* Turn conditionals into where clauses. We might want to do
         this earlier on.  *)
      let c = base index c in
      let (_, fields, tables, c', os) = select_clause index unit_query body in
      let c = Sql.smart_and c c' in
      (false, fields, tables, c, os)
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
        (false, fields, [Sql.TableRef(table, var)], Sql.Constant (Constant.Bool true), [])
    | Singleton _ when unit_query ->
      (* If we're inside an Sql.Empty or a Sql.Length it's safe to ignore
         any fields here. *)
      (* We currently detect this earlier, so the unit_query stuff here
         is redundant. *)
      (false, Sql.Fields [], [], Sql.Constant (Constant.Bool true), [])
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
        (false, fields, [], Sql.Constant (Constant.Bool true), [])
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
    | Project (Var (x, _tyx), name) ->
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
  Sql.Union (false, List.map (clause [] true) (prepare_clauses v), 0)
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
    let q_inner = Sql.Select(false,result,Sql.TableRef(tablename,t)::tables,where,os) in
    Sql.With (tablename, q_outer, q_inner)

let sql_of_let_query : let_query -> Sql.query =
  fun cs ->
    Sql.Union (false, List.map (let_clause) cs, 0)

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

let is_list = Q.is_list
let value_of_expression = Q.value_of_expression
let default_of_base_type = Q.default_of_base_type
let type_of_expression = Q.type_of_expression
let unbox_xml = Q.unbox_xml
