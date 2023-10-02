(*****************************************************************************
 ** mixingQuery.ml                                                          **
 **                                                                         **
 ** Normalisation of queries mixing sets and bags                           **
 **                                                                         **
 **                                                                         **
 ** author:  Wilmer Ricciotti                                               **
 ** created: 30 Jul 2021                                                    **
 *****************************************************************************)

open Utility
open CommonTypes
open Errors

module Q = QueryLang

let (++) = Q.append_env

let internal_error message =
  Errors.internal_error ~filename:"query/mixingQuery.ml" ~message

let rec tail_of_t : Q.t -> Q.t = fun v ->
  let tt = tail_of_t in
    match v with
      | Q.Singleton (Q.Record fields) -> Q.Record fields
      | Q.If (_, t, Q.Concat []) -> tt t
      | Q.For (_, _gs, _os, t) -> tt t
      | _ -> (* Debug.print ("v: "^string_of_t v); *) assert false

let type_of_for_var pol gen =
  match pol, Q.type_of_expression gen with
  | Q.Keys, ty -> fst <| Types.unwrap_map_type ty
  | _, ty -> Types.unwrap_list_type ty

let rec freshen_for_bindings : Var.var Env.Int.t -> Q.t -> Q.t =
  fun env v ->
    let ffb = freshen_for_bindings env in
    let rec var_lookup x =
      match Env.Int.find_opt x env with
      | None -> x
      | Some y -> var_lookup y
    in
    match v with
    | Q.For (tag, gs, os, b) ->
      let gs', env' =
        List.fold_left
          (fun (gs', env') (genkind, x, source) ->
            let y = Var.fresh_raw_var () in
            ((genkind, y, ffb source)::gs', Env.Int.bind x y env'))
          ([], env)
          gs
      in
      Q.For (tag, List.rev gs', List.map (freshen_for_bindings env') os, freshen_for_bindings env' b)
    | Q.If (c, t, e) -> Q.If (ffb c, ffb t, ffb e)
    | Q.Table _ as t -> t
    | Q.Singleton v -> Q.Singleton (ffb v)
    | Q.MapEntry (k,v) -> Q.MapEntry (ffb k, ffb v)
    | Q.Database db -> Q.Database db
    | Q.Concat vs -> Q.Concat (List.map ffb vs)
    | Q.Dedup q -> Q.Dedup (ffb q)
    | Q.Prom q -> Q.Prom (ffb q)
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
    | Q.Var (x, ts) as _v ->
      (* begin
        match Env.Int.find_opt x env with
        | None -> v (* Var (x, ts) *)
        | Some y -> Var (y, ts)
      end *)
      Q.Var (var_lookup x, ts)
    | Q.Constant c -> Q.Constant c
    | Q.GroupBy ((v,i),q) ->
      let y = Var.fresh_raw_var () in
      let env' = Env.Int.bind v y env in
      Q.GroupBy ((y, freshen_for_bindings env' i), ffb q)
    (* XXX: defensive programming; recursion on ar not needed now, but may be in the future *)
    | Q.AggBy (ar, q) -> Q.AggBy (StringMap.map (fun (x,y) -> ffb x, y) ar, ffb q)
    | Q.Lookup (q,k) -> Q.Lookup (ffb q, ffb k)

let flatfield f1 f2 = f1 ^ "@" ^ f2

let rec flattened_pair x y =
  match x, y with
  | Q.Var (_nx, Types.Record row), _ ->
      let x' = Q.Record (StringMap.fold (fun f _ acc -> StringMap.add f (Q.Project (x,f)) acc) (Q.field_types_of_row row) StringMap.empty)
      in flattened_pair x' y
  | _, Q.Var (_ny, Types.Record row) ->
      let y' = Q.Record (StringMap.fold (fun f _ acc -> StringMap.add f (Q.Project (y,f)) acc) (Q.field_types_of_row row) StringMap.empty)
      in flattened_pair x y'
  (* We use a field with an empty name to deal with variables of non-record type *)
  | Q.Var (_nx, _), _ ->
      let x' = Q.Record (StringMap.from_alist ["",x])
      in flattened_pair x' y
  | _, Q.Var (_ny, _) ->
      let y' = Q.Record (StringMap.from_alist ["",y])
      in flattened_pair x y'
  | Q.Record fty1, Q.Record fty2 ->
      let out1 =
          StringMap.fold (fun f v acc -> StringMap.add (flatfield "1" f) v acc) fty1 StringMap.empty
      in
      let out2 = StringMap.fold (fun f v acc -> StringMap.add (flatfield "2" f) v acc) fty2 out1
      in Q.Record out2
  | _ -> assert false

let rec flattened_pair_ft x y =
  match x, y with
  | Q.Var (_nx, Types.Record rowx), Q.Var (_ny, Types.Record rowy) ->
      let out1 =
          StringMap.fold (fun f t acc -> StringMap.add (flatfield "1" f) t acc) (Q.field_types_of_row rowx) StringMap.empty
      in
      StringMap.fold (fun f t acc -> StringMap.add (flatfield "2" f) t acc) (Q.field_types_of_row rowy) out1
  (* XXX: same as above, using a field with an empty name to deal with variables of non-record type *)
  | Q.Var (nx, tyx), _ -> flattened_pair_ft (Q.Var (nx, Types.make_record_type (StringMap.from_alist ["", tyx]))) y
  | _, Q.Var (ny, tyy) -> flattened_pair_ft x (Q.Var (ny, Types.make_record_type (StringMap.from_alist ["", tyy])))
  | _ -> assert false

(* gs must ALWAYS be non-empty, both input and output!*)
let expand_collection = function
| Q.For (_, gs, os', body) -> gs, os', body
| Q.Concat (_::_)
| Q.Singleton _
| Q.Prom _
| Q.Table _ as xs ->
    (* I think we can omit the `Table case as it
        can never occur *)
    (* eta-expand *)
    Q.eta_expand_list xs
| _ -> assert false

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
    | _                       -> Q.Apply  (Q.Primitive "not", [a])

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
  match c, t with
  (* optimisation *)
  | Q.Constant (Constant.Bool true), _
  | _, Q.Constant (Constant.Bool true) -> t
  | Q.Constant (Constant.Bool false), _
  | _, Q.Constant (Constant.Bool false) -> Q.Concat []

  | _, Q.Concat vs ->
      reduce_concat (List.map (fun v -> reduce_where_then (c, v)) vs)
  | _, Q.Prom t' -> Q.Prom (reduce_where_then (c, t'))
  | _, Q.For (_, gs, os, body) ->
    Q.For (None, gs, os, reduce_where_then (c, body))
  | _, Q.If (c', t', Q.Concat []) ->
      reduce_where_then (reduce_and (c, c'), t')
  | _ ->
    Q.If (c, t, Q.Concat [])

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
  let query_bindings_of_env e =
    let open Q in
    Env.Int.bindings (e.qenv)

  let rec reduce_project (r, label) =
    match r with
      | Q.Record fields ->
        assert (StringMap.mem label fields);
        StringMap.find label fields
      | Q.If (c, t, e) ->
        Q.If (c, reduce_project (t, label), reduce_project (e, label))
      | Q.Var (_x, Types.Record row) ->
        let field_types =  Q.field_types_of_row row in
        assert (StringMap.mem label field_types);
        Q.Project (r, label)
      | _ -> Q.query_error ("Error projecting label %s from record: %s") label (Q.string_of_t r)

  (* this now receives a function xc (xlate computation) *)
  let reduce_artifacts xc = function
  | Q.Apply (Q.Primitive "stringToXml", [u]) ->
    Q.Singleton (Q.XML (Value.Text (Q.unbox_string u)))
  | Q.Apply (Q.Primitive "AsList", [xs])
  | Q.Apply (Q.Primitive "AsListT", [xs])
  | Q.Apply (Q.Primitive "AsListV", [xs]) -> xs
  (* Temporal projection operations *)
  | Q.Apply (Q.Primitive "ttData", [x])
  | Q.Apply (Q.Primitive "vtData", [x]) ->
    Q.Project (x, TemporalField.data_field)
  | Q.Apply (Q.Primitive "ttFrom", [x])
  | Q.Apply (Q.Primitive "vtFrom", [x]) ->
    Q.Project (x, TemporalField.from_field)
  | Q.Apply (Q.Primitive "ttTo", [x])
  | Q.Apply (Q.Primitive "vtTo", [x]) ->
    Q.Project (x, TemporalField.to_field)
  | Q.Apply (Q.Primitive "Distinct", [u]) -> Q.Prom (Q.Dedup u)
  | Q.Apply (Q.Primitive "AggBy", [q; aggs]) ->
    let aggError x = Debug.print ("AggBy error --" ^ x); assert false in
    let of_closure = function
    | Q.Closure (([x], comp), env) ->
        (* a dummy type because we cannot synthesize the right one here *)
        let vty = Types.wrong_type in
        let vx = Q.Var (x, vty) in
        let env' = Q.bind env (x, vx) in
        x, xc env' comp
    | q -> aggError ("of_closure of " ^ (Q.show q))
    in
    let of_apply = function
    | Q.Apply (f, [arg]) -> f, arg
    | q -> aggError ("of_apply of " ^ (Q.show q))
    in
    let of_map_project = function
    | Q.Apply (Q.Primitive "ConcatMap", [c;q]) -> c, q
    | Q.Apply (Q.Project (r, l), [c;q]) when reduce_project (r, l) = Q.Primitive "ConcatMap" -> c, q
    | q -> aggError ("of_map_project of " ^ (Q.show q))
    in
    let of_project = function
    | Q.Project (r, l) -> l, r
    | q -> aggError ("of_project of " ^ (Q.show q))
    in
    let of_singleton = function
    (* | Q.Singleton q -> q -- but not really *)
    | Q.Apply (Q.Primitive "Cons", [q; Q.Concat []]) -> q
    | q -> aggError ("of_singleton of " ^  (Q.show q))
    in
    let of_record _x = function
    | Q.Record fields ->
      StringMap.fold (fun label v acc ->
        (* f is the aggregate function for this label *)
        let f, arg = of_apply v in
        let c, _q = of_map_project arg in
        (* TODO we should check q = x *)
        let y, cbody = of_closure c in
        match of_project (of_singleton cbody) with
        | l, Q.Var (var, _) when var = y ->
          StringMap.add label (f, l) acc
        | l, q -> aggError ("of_record label " ^ l ^ ": " ^ (Q.show q))
        )
        fields
        StringMap.empty
    | q -> aggError ("of_record " ^ (Q.show q))
    in
    Debug.print ("Aggregating with: " ^ Q.show aggs);
    let x, v = of_closure aggs in
    let ar = of_record x v in
    Q.AggBy (ar, q)

  | u -> u

  let rec xlate env : Ir.value -> Q.t = let open Ir in function
    | Constant c -> Q.Constant c
    | Variable var ->
        begin
          match Q.lookup env var with
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
        reduce_artifacts computation (Q.Apply (xlate env f, List.map (xlate env) ps))
    | Closure (f, _, v) ->
      let open Q in
      let (_finfo, (xs, body), z_opt, _location) = Tables.find Tables.fun_defs f in
      let z = OptionUtils.val_of z_opt in
      (* Debug.print ("Converting evalir closure: " ^ Var.show_binder (f, _finfo) ^ " to query closure"); *)
      (* yuck! *)
      let env' = bind (Q.empty_env env.policy) (z, xlate env v) in
      Q.Closure ((xs, body), env')
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
        reduce_artifacts computation (Q.Apply (xlate env f, List.map (xlate env) args))
    | Special (Ir.Query (None, policy, e, _)) ->
        let open Q in
        check_policies_compatible env.policy policy;
        computation env e
    | Special (Ir.Table { database; table = name; keys; temporal_fields;
                table_type = (temporality, readtype, _, _) }) ->
       (*  WR: this case is because shredding needs to access the keys of tables
           but can we avoid it (issue #432)? *)
       (* Copied almost verbatim from evalir.ml, which seems wrong, we should probably call into that. *)
       begin
         match xlate env database, xlate env name, xlate env keys, (TypeUtils.concrete_type readtype) with
         | Q.Database (db, params), name, keys, Types.Record (Types.Row row) ->
            let unboxed_keys =
              List.map
                (fun key ->
                  List.map Q.unbox_string (Q.unbox_list key))
                (Q.unbox_list keys)
            in
            let tbl =
                Value.make_table ~database:(db, params)
                    ~name:(Q.unbox_string name) ~keys:unboxed_keys
                    ~temporality ~temporal_fields ~row
            in
            Q.Table tbl
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
        let bvs'', res = List.fold_left (fun (bvs',acc) (_genkind,w,q) -> w::bvs', acc || cfree bvs' q) (bvs, false) gs in
        res || cfree bvs'' b || List.exists (cfree bvs) os
    | Q.Record fl -> StringMap.exists (fun _ t -> cfree bvs t) fl
    | _ -> false
    in cfree []

  let mk_for_term env (pol,x,xs) body_f =
    let ty_elem = type_of_for_var pol xs in
    let vx = Q.Var (x, ty_elem) in
    let cenv = Q.bind env (x, vx) in
    Q.For (None, [pol,x,xs], [], body_f cenv)

  (* this has the effect of performing beta reduction when the generator in
   * the main input is a Singleton *)
  let reduce_for_source gsx env = function
  (* in a value comprehesion, no need to unbox a MapEntry in v because
   * NRC with grouping forces the generator to be a pure collection *)
  | (Q.Entries, x, Q.Singleton v) -> gsx, Q.bind env (x,v)
  | (Q.Keys, x, Q.Singleton (Q.MapEntry (k,_))) -> gsx, Q.bind env (x,k)
  | (Q.Entries, x, (Q.Table (Value.Table.{ row; temporality; temporal_fields; _ } as table) as q))
     when temporality = Temporality.Transaction || temporality = Temporality.valid ->
       let (from_field, to_field) = OptionUtils.val_of temporal_fields in
       (* Transaction / Valid-time tables: Need to wrap as metadata *)
       (* First, generate a fresh variable for the table *)
       let make_spec_map = StringMap.map (fun x -> Types.Present x) in
       let field_types = Q.table_field_types table in
       let base_field_types =
         StringMap.filter
           (fun x _ -> x <> from_field && x <> to_field)
           field_types in
       let (_, row_var, dual) = row in
       let z = Var.fresh_raw_var () in
       let tyz = Types.(Record (Row (make_spec_map field_types, row_var, dual))) in
       let base_ty_elem = Types.(Record (Row (make_spec_map base_field_types, row_var, dual))) in
       let vz = Q.Var (z, tyz) in

       (* Second, generate a fresh variable for the metadata *)
       let metadata_record =
         StringMap.from_alist [
           (TemporalField.data_field,
             Q.eta_expand_var (z, base_ty_elem));
           (TemporalField.from_field,
             Q.Project (vz, from_field));
           (TemporalField.to_field,
             Q.Project (vz, to_field))
         ] in
       Debug.print ("reduce_for_source with temporal generator: " ^ (Q.show (Q.Record metadata_record)));
       let env' = Q.bind env (x, Q.Record metadata_record) in
       gsx@[Q.Entries, z, q], env'
  | (pol, x, q) ->
      let z = Var.fresh_raw_var () in
      let tyz = type_of_for_var pol q in
      let vz = Q.Var (z, tyz) in
      let env' = Q.bind env (x, vz) in
      gsx@[pol, z, q], env'

  (* when the head of a comprehension is a Prom, this lifts it to a generator
   * by means of eta-expansion *)
  let reduce_for_body gsx = function
  | Q.Prom _ as u ->
      let z = Var.fresh_raw_var () in
      let tyz =
        Q.type_of_expression u
        |> TypeUtils.element_type
      in
      let vz = Q.Var (z, tyz) in
      gsx@[Q.Entries,z,u], Q.Singleton vz
  | u -> gsx, u

  (* auxiliary functions to pack/unpack normalised collections *)
  let pack_for (body, c, gs, os) =
    let body' = reduce_where_then (c,body) in
    match gs, os with
    | [], [] -> body'
    | _ -> Q.For (None, gs, os, body')

  let pack_ncoll ql = reduce_concat (List.map pack_for ql)

  let unpack_where = function
    | Q.If (c, t, Q.Concat []) -> t, c
    | q -> q,  Q.Constant (Constant.Bool true)

  let unpack_for = function
    | Q.For (_, gs, os, body) ->
        let body', c = unpack_where body in
        body', c, gs, os
    | q ->
        let body', c = unpack_where q in
        body', c, [], []

  let unpack_ncoll = function
  | Q.Concat ql -> List.map unpack_for ql
  | q -> [unpack_for q]

  let rec norm in_dedup env : Q.t -> Q.t =
    function
    (* XXX: this is a quick and dirty fix to implement substitution into a term via normalization:
       normally at this point free variables should already have been substituted (by xlate)
       but sometimes we use norm with open terms and a specially crafted env to perform substitution.
       It should be tested, and may be more complicated than required because it was ported from xlate *)
    | Q.Var (var, _) as orig ->
        begin
          try
            match Q.lookup env var with
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
                  freshen_for_bindings Env.Int.empty (norm in_dedup env v)
          with
          | InternalError _ -> retn in_dedup orig
        end
    | Q.Record fl -> Q.Record (StringMap.map (norm false env) fl)
    | Q.Singleton v -> Q.Singleton (norm false env v)
    | Q.MapEntry (k,v) -> Q.MapEntry (norm false env k, norm false env v)
    | Q.Concat xs -> reduce_concat (List.map (norm in_dedup env) xs)
    | Q.Project (r, label) ->
        retn in_dedup (reduce_project (norm false env r, label))
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
        erase (norm false env r, labels)
    | Q.Variant (label, v) -> Q.Variant (label, norm false env v)
    | Q.Apply (f, xs) as _orig ->
      apply in_dedup env (norm false env f, List.map (norm false env) xs)
    | Q.For (_, gs, os, u) as _orig ->
        let reduce_gs =
          let rec rgs gsx cx env os body = function
          | (pol,x,g)::gs' ->
              let ng = unpack_ncoll (norm in_dedup env g) in
              begin
                match pol, in_dedup, ng with
                (* if pol is Entries, we are in a set comprehension, or the generator is nil or a singleton, we can reduce *)
                | Q.Entries, _, _
                | _, true, _
                | _, _, ([] | [(Q.Singleton _, _, [], _)]) ->
                  List.concat (List.map (fun (gsrc, gc, ggs, gos) ->
                        let gsx', env' = reduce_for_source (gsx@ggs) env (pol,x,gsrc) in
                        rgs gsx' (reduce_and (cx, gc)) env' (os@gos) body gs') ng)
                (* pol is Keys, we are in a bag comprehension, and the generator is not nil or a singleton: no unnesting or splitting of unions *)
                | _ ->
                  let z = Var.fresh_raw_var () in
                  let tyz = type_of_for_var pol g in
                  let vz = Q.Var (z, tyz) in
                  let env' = Q.bind env (x, vz) in
                  rgs (gsx@[pol,z,pack_ncoll ng]) cx env' os body gs'
              end
          | [] ->
              let nbody = unpack_ncoll (norm in_dedup env body) in
              List.map (fun (bbody, bc, bgs, bos) ->
                    let bgs', bbody' = reduce_for_body (gsx@bgs) bbody in
                    let os' = List.map (fun o -> norm false env o) (os@bos) in
                    bbody', reduce_and (cx, bc), bgs', os') nbody
          in rgs [] (Q.Constant (Constant.Bool true))
        in
        pack_ncoll (reduce_gs env os u gs)
    | Q.If (c, t, e) ->
        reduce_if_condition (norm false env c, norm in_dedup env t, norm in_dedup env e)
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
             | None, None -> Q.query_error "Pattern matching failed"
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
    | Q.GroupBy ((x,qx), q) ->
       let ql = unpack_ncoll (norm in_dedup env q) in
       let qx' = norm false env qx in
       (* the following assumes normalised records are eta expanded *)
       let rcd_combine = function
       | Q.Record rx, Q.Record ry ->
           begin
             try Q.Record (StringMap.union_disjoint rx ry)
             with StringMap.Not_disjoint _ -> Q.query_error "rcd_combine: unnable to merge overlapping grouping criteria (buggy typechecker?)"
           end
       | Q.Record _, z | z, _ -> Q.query_error "rcd_combine: unexpected non-record argument (buggy normaliser?): %s" (Q.show z)
       in
       let gc_combine (x,qx) (y,qy) =
         let z = Var.fresh_raw_var () in
         let tyz = TypeUtils.element_type ~overstep_quantifiers:true (Q.type_of_expression q) in
         let vz = Q.Var (z, tyz) in
         let qx = norm false (Q.bind env (x, vz)) qx in
         let qy = norm false (Q.bind env (y, vz)) qy in
         z, rcd_combine (qx, qy)
       in
       let reduce_groupby = function
       | Q.Singleton (Q.MapEntry (k, v)) -> Q.Singleton (Q.MapEntry (rcd_combine ((norm false (Q.bind env (x, v)) qx'), k), v))
       | Q.Singleton v (* not MapEntry *) -> Q.Singleton (Q.MapEntry (norm false (Q.bind env (x, v)) qx', v))
       | Q.GroupBy ((y, qy), q') -> Q.GroupBy (gc_combine (x,qx') (y,qy), q')
       | qf -> Q.GroupBy ((x, qx'), qf)
       in
       let ql' = List.map (fun (b, c, gs, os) -> (reduce_groupby b, c, gs, os)) ql in
       pack_ncoll ql'
    | Q.AggBy (ar, q) -> Q.AggBy (StringMap.map (fun (x,y) -> norm false env x, y) ar, norm in_dedup env q)
    | Q.Lookup (q, k) ->
       let ql = unpack_ncoll (norm in_dedup env q) in
       let k' = norm false env k in
       let reduce_lookup = function
       | Q.Singleton (Q.MapEntry (kv, v)) -> reduce_where_then (reduce_eq (kv, k'), Q.Singleton v)
       | Q.Singleton _ (* not MapEntry *) as qorig -> assert (Q.type_of_expression k' = Types.unit_type); qorig
       | qorig -> Q.Lookup (qorig, k)
       in
       let ql' = List.map (fun (b, c, gs, os) -> (reduce_lookup b, c, gs, os)) ql in
       pack_ncoll ql'
    | v -> retn in_dedup v

  and apply_concatMap in_dedup env pol xs = function
  | Q.Closure (([x], body), closure_env) ->
      (* Debug.print ("Application of ConcatMap(Key)");
      Debug.print ("pol: " ^ Q.show_genkind pol);
      Debug.print ("f: " ^ Q.show f);
      Debug.print ("xs: " ^ Q.show xs); *)
      (fun cenv -> computation cenv body)
      |> mk_for_term (env ++ closure_env) (pol,x,xs)
      |> norm in_dedup env
  | _ -> assert false

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
    | Q.Primitive "ConcatMap", [f; xs] -> apply_concatMap in_dedup env Q.Entries xs f
    | Q.Primitive "ConcatMapKey", [f; xs] -> apply_concatMap in_dedup env Q.Keys xs f
    | Q.Primitive "Map", [f; xs] ->
        begin
          match f with
            | Q.Closure (([x], body), closure_env) ->
                (fun cenv -> Q.Singleton (computation cenv body))
                |> mk_for_term (env ++ closure_env) (Q.Entries,x,xs)
                |> norm in_dedup env
            | _ -> assert false
        end
    | Q.Primitive "GroupBy", [f; q] ->
        begin
          match f with
            | Q.Closure (([x], body_c), closure_env) ->
                let tyx = TypeUtils.element_type ~overstep_quantifiers:true (Q.type_of_expression q) in
                let vx = Q.Var (x, tyx) in
                let body_env = Q.bind (env ++ closure_env) (x,vx) in
                let body = computation body_env body_c in
                norm in_dedup env (Q.GroupBy ((x, body), q))
            | _ -> assert false
        end
    | Q.Primitive "Lookup", [v; q] -> norm in_dedup env (Q.Lookup (q, v))
    | Q.Primitive "SortBy", [f; xs] ->
        begin
          match xs with
            | Q.Concat [] -> Q.Concat []
            | _ ->
                let gs, os', body = expand_collection xs in
                begin
                  match f with
                  | Q.Closure (([x], os), closure_env) ->
                      let os =
                        let cenv = Q.bind (env ++ closure_env) (x, tail_of_t body) in

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
        reduce_if_condition (c, apply in_dedup env (t, args), apply in_dedup env (e, args))
    | Q.Apply (f, args), args' ->
        apply in_dedup env (f, args @ args')
    | t, _ -> Q.query_error "Application of non-function: %s" (Q.string_of_t t)

  and norm_comp in_dedup env c =
    computation env c
    |> norm in_dedup env
  and retn in_dedup u = if in_dedup then Q.Dedup u else u

  (* specialize norm_* with in_dedup = false at the start of normalization *)
  let norm = norm false
  let norm_comp = norm_comp false

  let eval policy env e =
    Debug.debug_time "Query.eval" (fun () ->
      (* let res = *)
      norm_comp (Q.env_of_value_env policy env) e
      (* in
      Debug.print ("eval returned: " ^ Q.show res);
      res *)
      )
end

let compile_update : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option * Ir.computation) -> Sql.query =
  fun db env ((x, table, field_types), where, body) ->
    let tyx = Types.make_record_type field_types in
    let env = Q.bind (Q.env_of_value_env QueryPolicy.Mixing env) (x, Q.Var (x, tyx)) in
(*      let () = opt_iter (fun where ->  Debug.print ("where: "^Ir.show_computation where)) where in*)
    let where = opt_map (Eval.norm_comp env) where in
(*       Debug.print ("body: "^Ir.show_computation body); *)
    let body = Eval.norm_comp env body in
    let q = Q.update ((x, table), where, body) in
      Debug.print ("Generated update query: " ^ (db#string_of_query q));
      q

let compile_delete : Value.database -> Value.env ->
  ((Ir.var * string * Types.datatype StringMap.t) * Ir.computation option) -> Sql.query =
  fun db env ((x, table, field_types), where) ->
    let tyx = Types.make_record_type field_types in
    let env = Q.bind (Q.env_of_value_env QueryPolicy.Mixing env) (x, Q.Var (x, tyx)) in
    let where = opt_map (Eval.norm_comp env) where in
    let q = Q.delete ((x, table), where) in
      Debug.print ("Generated update query: " ^ (db#string_of_query q));
      q
