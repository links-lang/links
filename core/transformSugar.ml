open Utility
open Sugartypes
open Operators

module TyEnv = Env.String

let type_section env =
  function
    | `Minus -> TyEnv.lookup env "-"
    | `FloatMinus -> TyEnv.lookup env "-."
    | `Project label ->
        let ab, a = Types.fresh_type_quantifier (`Any, `Any) in
        let rhob, (fields, rho, _) = Types.fresh_row_quantifier (`Any, `Any) in
        let eb, e = Types.fresh_row_quantifier (`Any, `Any) in

        let r = `Record (StringMap.add label (`Present a) fields, rho, false) in
          `ForAll (Types.box_quantifiers [ab; rhob; eb],
                   `Function (Types.make_tuple_type [r], e, a))
    | `Name var -> TyEnv.lookup env var

let type_unary_op env tycon_env =
  let datatype = DesugarDatatypes.read ~aliases:tycon_env in function
    | `Minus      -> datatype "(Int) -> Int"
    | `FloatMinus -> datatype "(Float) -> Float"
    | `Name n     -> TyEnv.lookup env n

let type_binary_op env tycon_env =
  let datatype = DesugarDatatypes.read ~aliases:tycon_env in function
  | `Minus        -> TyEnv.lookup env "-"
  | `FloatMinus   -> TyEnv.lookup env "-."
  | `RegexMatch flags ->
      let nativep  = List.exists ((=) `RegexNative)  flags
      and listp    = List.exists ((=) `RegexList)    flags
      and replacep = List.exists ((=) `RegexReplace) flags in
        (match replacep, listp, nativep with
           | true,   _   , false -> (* stilde  *) datatype "(String, Regex) -> String"
           | false, true , false -> (* ltilde *)  datatype "(String, Regex) -> [String]"
           | false, false, false -> (* tilde *)   datatype "(String, Regex) -> Bool"
           | _,     _,     true  -> assert false)

  | `And
  | `Or           -> datatype "(Bool,Bool) -> Bool"
  | `Cons         -> TyEnv.lookup env "Cons"
  | `Name "++"    -> TyEnv.lookup env "Concat"
  | `Name ">"
  | `Name ">="
  | `Name "=="
  | `Name "<"
  | `Name "<="
  | `Name "<>" ->
      let ab, a = Types.fresh_type_quantifier (`Any, `Any) in
      let eb, e = Types.fresh_row_quantifier (`Any, `Any) in
        `ForAll (Types.box_quantifiers [ab; eb],
                 `Function (Types.make_tuple_type [a; a], e, `Primitive `Bool))
  | `Name "!"     -> TyEnv.lookup env "Send"
  | `Name n       -> TyEnv.lookup env n

let fun_effects t pss =
  let rec get_eff =
    function
      | `Function (_, effects, _), [_] -> effects
      | `Function (_, _, t), _::pss -> get_eff (TypeUtils.concrete_type t, pss)
      | `Lolli (_, effects, _), [_] -> effects
      | `Lolli (_, _, t), _::pss -> get_eff (TypeUtils.concrete_type t, pss)
      | _ -> assert false in
  let t =
    match TypeUtils.concrete_type t with
      | `ForAll (_, t) -> TypeUtils.concrete_type t
      | t -> t
  in
    get_eff (t, pss)

let option :
    'self_type ->
  ('self_type -> 'a -> ('self_type * 'a * Types.datatype)) ->
  'a option -> ('self_type * ('a option) * (Types.datatype option))
  =
  fun o f ->
    function
      | None -> (o, None, None)
      | Some x -> let (o, x, t) = f o x in (o, Some x, Some t)

let optionu :
    'self_type ->
    ('self_type -> 'a -> ('self_type * 'a)) ->
  'a option -> ('self_type * ('a option)) =
  fun o f ->
    function
      | None -> (o, None)
      | Some x -> let (o, x) = f o x in (o, Some x)

let rec list :
    'self_type ->
    ('self_type -> 'a -> 'self_type * 'a * Types.datatype) ->
  'a list -> 'self_type * 'a list * Types.datatype list =
  fun o f ->
        function
          | [] -> (o, [], [])
          | x :: xs ->
              let (o, x, t) = f o x in
              let (o, xs, ts) = list o f xs in (o, x::xs, t::ts)

let rec listu :
    'self_type ->
    ('self_type -> 'a -> 'self_type * 'a) ->
  'a list -> 'self_type * 'a list =
  fun o f ->
    function
      | [] -> (o, [])
      | x :: xs ->
          let (o, x) = f o x in
          let (o, xs) = listu o f xs in (o, x::xs)

let check_type_application (e, t) k =
  begin
    try
      k ()
    with Instantiate.ArityMismatch ->
      prerr_endline ("Arity mismatch in type application");
      prerr_endline ("Expression: " ^ show_phrasenode e);
      prerr_endline ("Type: "^Types.string_of_datatype t);
      raise Instantiate.ArityMismatch
  end

class transform (env : Types.typing_environment) =
  object (o : 'self_type)
    val var_env = env.Types.var_env
    val tycon_env = env.Types.tycon_env
    val formlet_env = TyEnv.empty
    val effect_row = fst (Types.unwrap_row env.Types.effect_row)

    method get_var_env : unit -> Types.environment = fun () -> var_env
    method get_tycon_env : unit -> Types.tycon_environment = fun () -> tycon_env
    method get_formlet_env : unit -> Types.environment = fun () -> formlet_env

    method backup_envs = var_env, tycon_env, formlet_env, effect_row
    method restore_envs (var_env, tycon_env, formlet_env, effect_row) =
      {< var_env = var_env; tycon_env = tycon_env; formlet_env = formlet_env;
         effect_row = effect_row >}

    method with_var_env var_env =
      {< var_env = var_env >}

    method with_formlet_env formlet_env =
      {< formlet_env = formlet_env >}

    method lookup_type : name -> Types.datatype = fun var ->
      TyEnv.lookup var_env var

    method lookup_effects : Types.row = effect_row

    method with_effects : Types.row -> 'self_type = fun effects ->
      {< effect_row = fst (Types.unwrap_row effects) >}

    method sugar_datatype : datatype -> ('self_type * datatype) =
      fun s -> (o, s)

    method datatype : Types.datatype -> ('self_type * Types.datatype) =
      fun t -> (o, t)

    method datatype' : datatype' -> ('self_type * datatype') =
      fun (s, t) ->
        let (o, s) = o#sugar_datatype s in
        let (o, t) = optionu o (fun o -> o#datatype) t in
          (o, (s, t))

    method lens_sort : Types.lens_sort -> ('self_type * Types.lens_sort) =
      fun (fds, cond, t) ->
            (o, (fds, cond, t))

    method row : Types.row -> ('self_type * Types.row) =
      fun row -> (o, row)

    method unary_op : unary_op -> ('self_type * unary_op * Types.datatype) =
      fun op ->
        (o, op, type_unary_op var_env tycon_env op)

    method binop : binop -> ('self_type * binop * Types.datatype) =
      fun op ->
        (o, op, type_binary_op var_env tycon_env op)

    method sec : sec -> ('self_type * sec * Types.datatype) =
      fun sec ->
        (o, sec, type_section var_env sec)

    method sentence : sentence -> ('self_type * sentence) =
      function
      | `Definitions defs ->
          let (o, defs) = listu o (fun o -> o#binding) defs
          in (o, `Definitions defs)
      | `Expression e -> let (o, e, _) = o#phrase e in (o, `Expression e)
      | `Directive d -> (o, `Directive d)

    method regex : regex -> ('self_type * regex) =
      function
        | (`Range _ | `Simply _ | `Any  | `StartAnchor | `EndAnchor) as r -> (o, r)
        | `Quote r -> let (o, r) = o#regex r in (o, `Quote r)
        | `Seq rs ->
            let (o, rs) = listu o (fun o -> o#regex) rs in (o, `Seq rs)
        | `Alternate (r1, r2) ->
            let (o, r1) = o#regex r1 in
            let (o, r2) = o#regex r2 in
              (o, `Alternate (r1, r2))
        | `Group r -> let (o, r) = o#regex r in (o, `Group r)
        | `Repeat (repeat, r) ->
            let (o, r) = o#regex r in (o, `Repeat (repeat, r))
        | `Splice e -> let (o, e, _) = o#phrase e in (o, `Splice e)
        | `Replace (r, `Literal s) ->
            let (o, r) = o#regex r in
              (o, `Replace (r, `Literal s))
        | `Replace (r, `Splice e) ->
            let (o, r) = o#regex r in
            let (o, e, _) = o#phrase e in
              (o, `Replace (r, `Splice e))

    method program : program -> ('self_type * program * Types.datatype option) =
      fun (bs, e) ->
        let (o, bs) = listu o (fun o -> o#binding) bs in
        let (o, e, t) = option o (fun o -> o#phrase) e
        in (o, (bs, e), t)

    method given_spawn_location :
      given_spawn_location ->
      ('self_type * given_spawn_location) = function
        | `ExplicitSpawnLocation p ->
            let (o, phr, _phr_ty) = o#phrase p in
            (o, `ExplicitSpawnLocation phr)
        | l -> (o, l)

    method phrasenode : phrasenode -> ('self_type * phrasenode * Types.datatype) =
      function
      | `Constant c -> let (o, c, t) = o#constant c in (o, (`Constant c), t)
      | `Var var -> (o, `Var var, o#lookup_type var)
      | `FunLit (Some argss, lin, lam, location) ->
          let inner_e = snd (try last argss with Invalid_argument s -> raise (Invalid_argument ("@" ^ s))) in
          let (o, lam, rt) = o#funlit inner_e lam in
          let (o, t) =
            List.fold_right
              (fun (args, effects) (o, rt) ->
                 let (o, args) = o#datatype args in
                 let (o, effects) = o#row effects in
                   (o, `Function (args, effects, rt)))
              argss
              (o, rt)
          in
            (o, `FunLit (Some argss, lin, lam, location), t)
      | `HandlerLit _ -> assert false
      | `Spawn (`Wait, loc, body, Some inner_effects) ->
          assert (loc = `NoSpawnLocation);
          (* bring the inner effects into scope, then restore the
             environments afterwards *)
          let envs = o#backup_envs in
          let (o, inner_effects) = o#row inner_effects in
          let o = o#with_effects inner_effects in
          let (o, body, body_type) = o#phrase body in
          let o = o#restore_envs envs in
            (o, `Spawn (`Wait, loc, body, Some inner_effects), body_type)
      | `Spawn (k, spawn_loc, body, Some inner_effects) ->
          (* bring the inner effects into scope, then restore the
             environments afterwards *)
          let (o, spawn_loc) = o#given_spawn_location spawn_loc in
          let envs = o#backup_envs in
          let (o, inner_effects) = o#row inner_effects in
          let process_type = `Application (Types.process, [`Row inner_effects]) in
          let o = o#with_effects inner_effects in
          let (o, body, _) = o#phrase body in
          let o = o#restore_envs envs in
            (o, (`Spawn (k, spawn_loc, body, Some inner_effects)), process_type)
      | `Select (l, e) ->
         let (o, e, t) = o#phrase e in
         (o, (`Select (l, e)), TypeUtils.select_type l t)
      | `Offer (e, bs, Some t) ->
          let (o, e, _) = o#phrase e in
          let (o, bs) =
            listu o
              (fun o (p, e) ->
                 let (o, p) = o#pattern p in
                 let (o, e, _) = o#phrase e in (o, (p, e)))
              bs in
          let (o, t) = o#datatype t in
            (o, `Offer (e, bs, Some t), t)
      | `CP p ->
         let (o, p, t) = o#cp_phrase p in
         (o, `CP p, t)
      | `Query (range, body, Some t) ->
          let (o, range) =
            optionu o
              (fun o (limit, offset) ->
                 let (o, limit, _) = o#phrase limit in
                 let (o, offset, _) = o#phrase offset in
                   (o, (limit, offset)))
              range in
          let (o, body, _) = o#phrase body in
          let (o, t) = o#datatype t in
            (o, (`Query (range, body, Some t)), t)
      | `ListLit (es, Some t) ->
          let (o, es, _) = list o (fun o -> o#phrase) es in
          let (o, t) = o#datatype t in
            (o, `ListLit (es, Some t), Types.make_list_type t)
      | `RangeLit (e1, e2) ->
          let (o, e1, _) = o#phrase e1 in
          let (o, e2, _) = o#phrase e2 in
            (o, `RangeLit (e1, e2), Types.make_list_type Types.int_type)
      | `Iteration (gens, body, cond, orderby) ->
          let (o, gens) = listu o (fun o -> o#iterpatt) gens in
          let (o, body, t) = o#phrase body in
          let (o, cond, _) = option o (fun o -> o#phrase) cond in
          let (o, orderby, _) = option o (fun o -> o#phrase) orderby in
            (o, `Iteration (gens, body, cond, orderby), t)
      | `Escape (b, e) ->
          let envs = o#backup_envs in
          let (o, b) = o#binder b in
          let (o, e, t) = o#phrase e in
          let o = o#restore_envs envs in
            (o, `Escape (b, e), t)
      | `Section sec -> (o, `Section sec, type_section var_env sec)
      | `Conditional (p, e1, e2) ->
          let (o, p, _) = o#phrase p in
          let (o, e1, t) = o#phrase e1 in
          let (o, e2, _) = o#phrase e2
          in (o, `Conditional (p, e1, e2), t)
      | `Block (bs, e) ->
          let envs = o#backup_envs in
          let (o, bs) = listu o (fun o -> o#binding) bs in
          let (o, e, t) = o#phrase e in
          let o = o#restore_envs envs in
            o, `Block (bs, e), t
      | `InfixAppl ((tyargs, op), e1, e2) ->
          let (o, op, t) = o#binop op in
            check_type_application
              (`InfixAppl ((tyargs, op), e1, e2), t)
              (fun () ->
                 let t = TypeUtils.return_type (Instantiate.apply_type t tyargs) in
                 let (o, e1, _) = o#phrase e1 in
                 let (o, e2, _) = o#phrase e2 in
                   (o, `InfixAppl ((tyargs, op), e1, e2), t))
      | `Regex r ->
          let (o, r) = o#regex r in
            (o, `Regex r, Instantiate.alias "Regex" [] tycon_env)
      | `UnaryAppl ((tyargs, op), e) ->
          let (o, op, t) = o#unary_op op in
            check_type_application
              (`UnaryAppl ((tyargs, op), e), t)
              (fun () ->
                 let t = TypeUtils.return_type (Instantiate.apply_type t tyargs) in
                 let (o, e, _) = o#phrase e in
                   (o, `UnaryAppl ((tyargs, op), e), t))
      | `FnAppl (f, args) ->
         let (o, f, ft) = o#phrase f in
         let (o, args, _) = list o (fun o -> o#phrase) args in
            (o, `FnAppl (f, args), TypeUtils.return_type ft)
      | `TAbstr (tyvars, e) ->
          let outer_tyvars = o#backup_quantifiers in
          let (o, qs) = o#quantifiers (Types.unbox_quantifiers tyvars) in
          let (o, e, t) = o#phrase e in
          let o = o#restore_quantifiers outer_tyvars in
          let t = Types.for_all (qs, t) in
            (o, tabstr (qs, e.node), t)
      | `TAppl (e, tyargs) ->
          let (o, e, t) = o#phrase e in
            check_type_application
              (`TAppl (e, tyargs), t)
              (fun () ->
                 let t = Instantiate.apply_type t tyargs in
                   (o, `TAppl (e, tyargs), t))
      | `TupleLit [e] ->
          (* QUESTION:

             Why do we type 1-tuples as if they aren't tuples?
          *)
          let (o, e, t) = o#phrase e in
            (o, `TupleLit [e], t)
      | `TupleLit es ->
          let (o, es, ts) = list o (fun o -> o#phrase) es in
            (o, `TupleLit es, Types.make_tuple_type ts)
      | `RecordLit (fields, base) ->
          let (o, fields, field_types) =
            let rec list o =
              function
                | [] -> (o, [], StringMap.empty)
                | (name, e)::fields ->
                    let (o, e, t) = o#phrase e in
                    let (o, fields, field_types) = list o fields in
                      (o,
                       (name, e)::fields,
                       StringMap.add name t field_types)
            in
              list o fields in
          let (o, base, base_type) = option o (fun o -> o#phrase) base in
          let t =
            match base_type with
              | None -> Types.make_record_type field_types
              | Some t ->
                  begin
                    match TypeUtils.concrete_type t with
                      | `Record row ->
                          `Record (Types.extend_row field_types row)
                      | t ->
                          Debug.print ("bad t: " ^ Types.string_of_datatype t);
                          assert false
                  end
          in
            (o, `RecordLit (fields, base), t)
      | `Projection (e, name) ->
          let (o, e, t) = o#phrase e in
          (o, `Projection (e, name), TypeUtils.project_type name t)
      | `With (e, fields) ->
          let (o, e, t) = o#phrase e in
          let (o, fields) =
            let rec list o =
              function
                | [] -> (o, [])
                | (name, e)::fields ->
                    let (o, e, _) = o#phrase e in
                    let (o, fields) = list o fields in
                      (o, (name, e)::fields)
            in
              list o fields
          in
            (o, `With (e, fields), t)
      | `TypeAnnotation (e, ann_type) ->
          let (o, e, _) = o#phrase e in
          let (o, ann_type) = o#datatype' ann_type in
          let t = val_of (snd ann_type) in
          (o, `TypeAnnotation (e, ann_type), t)
      | `Upcast (e, to_type, from_type) ->
          let (o, e, _) = o#phrase e in
          let (o, to_type) = o#datatype' to_type in
          let (o, from_type) = o#datatype' from_type in
          let t = val_of (snd to_type) in
            (o, `Upcast (e, to_type, from_type), t)
      | `ConstructorLit (name, e, Some t) ->
          let (o, e, _) = option o (fun o -> o#phrase) e in
          let (o, t) = o#datatype t in
          (o, `ConstructorLit (name, e, Some t), t)
      | `DoOperation (name, ps, Some t) ->
         let (o, ps, _) = list o (fun o -> o#phrase) ps in
         (o, `DoOperation (name, ps, Some t), t)
      | `Handle { sh_expr; sh_effect_cases; sh_value_cases; sh_descr } ->
         let (input_row, input_t, output_row, output_t) = sh_descr.shd_types in
         let (o, expr, _) = o#phrase sh_expr in
         let envs = o#backup_envs in
         let (o, params) =
           match sh_descr.shd_params with
           | Some params ->
              let (o, bindings) =
                List.fold_left
                  (fun (o, bindings) (body, pat) ->
                    (* let (o, body, _) = o#phrase body in *)
                    let (o, pat) = o#pattern pat in
                    (o, (body, pat) :: bindings))
                  (o, []) params.shp_bindings
              in
              (o, Some { params with shp_bindings = List.rev bindings })
           | None -> (o, None)
         in
         let (o, val_cases) =
           listu o
              (fun o (p, e) ->
                let (o, p) = o#pattern p in
                let (o, e, _) = o#phrase e in (o, (p, e)))
              sh_value_cases
         in
         let (o, eff_cases) =
           listu o
              (fun o (p, e) ->
                let (o, p) = o#pattern p in
                let (o, e, _) = o#phrase e in (o, (p, e)))
              sh_effect_cases
         in
         let o = o#restore_envs envs in
         let (o, input_row) = o#row input_row in
         let (o, input_t) = o#datatype input_t in
         let (o, output_row) = o#row output_row in
         let (o, output_t) = o#datatype output_t in
         let (o, raw_row) = o#row sh_descr.shd_raw_row in
         let descr = {
           shd_depth = sh_descr.shd_depth;
           shd_types = (input_row, input_t, output_row, output_t);
           shd_raw_row = raw_row;
           shd_params = params}
         in
         (o, `Handle { sh_expr = expr; sh_effect_cases = eff_cases; sh_value_cases = val_cases; sh_descr = descr }, output_t)
      | `TryInOtherwise (try_phr, as_pat, as_phr, otherwise_phr, (Some dt)) ->
          let (o, try_phr, _) = o#phrase try_phr in
          let (o, as_pat) = o#pattern as_pat in
          let (o, as_phr, _) = o#phrase as_phr in
          let (o, otherwise_phr, _) = o#phrase otherwise_phr in
          let (o, dt) = o#datatype dt in
          (o, `TryInOtherwise (try_phr, as_pat, as_phr, otherwise_phr, (Some dt)), dt)
      | `Raise -> (o, `Raise, `Not_typed) (* TEMP *)
      | `Switch (v, cases, Some t) ->
          let (o, v, _) = o#phrase v in
          let (o, cases) =
            listu o
              (fun o (p, e) ->
                 let (o, p) = o#pattern p in
                 let (o, e, _) = o#phrase e in (o, (p, e)))
              cases in
          let (o, t) = o#datatype t in
            (o, `Switch (v, cases, Some t), t)
      | `Receive (cases, Some t) ->
          let (o, cases) =
            listu o
              (fun o (p, e) ->
                 let (o, p) = o#pattern p in
                 let (o, e, _) = o#phrase e in (o, (p, e)))
              cases in
          let (o, t) = o#datatype t in
            (o, `Receive (cases, Some t), t)
      | `DatabaseLit (name, (driver, args)) ->
          let (o, name, _) = o#phrase name in
          let (o, driver, _) = option o (fun o -> o#phrase) driver in
          let (o, args, _) = option o (fun o -> o#phrase) args in
            (o, `DatabaseLit (name, (driver, args)), `Primitive `DB)
      | `LensLit (table, Some t) ->
         let (o, table, _) = o#phrase table in
         let (o, t) = o#lens_sort t in
            (o, `LensLit (table, Some t), `Lens (t))
      | `LensDropLit (lens, drop, key, default, Some t) ->
          let (o, lens, _) = o#phrase lens in
          let (o, t) = o#lens_sort t in
          let (o, default, _) = o#phrase default in
            (o, `LensDropLit (lens, drop, key, default, Some t), `Lens (t))
      | `LensSelectLit (lens, predicate, Some t) ->
          let (o, lens, _) = o#phrase lens in
          (* let (o, predicate, _) = o#phrase predicate in *)
          let (o, t) = o#lens_sort t in
            (o, `LensSelectLit (lens, predicate, Some t), `Lens t)
      | `LensJoinLit (lens1, lens2, on, left, right, Some t) ->
          let (o, lens1, _) = o#phrase lens1 in
          let (o, lens2, _) = o#phrase lens2 in
          let (o, t) = o#lens_sort t in
            (o, `LensJoinLit (lens1, lens2, on, left, right, Some t), `Lens t)
      | `LensGetLit (lens, Some t) ->
          let (o, lens, _) = o#phrase lens in
          let (o, t) = o#datatype t in
            (o, `LensGetLit (lens, Some t), Types.make_list_type t)
      | `LensPutLit (lens, data, Some t) ->
          let (o, lens, _) = o#phrase lens in
          let (o, data, _) = o#phrase data in
          let (o, t) = o#datatype t in
            (o, `LensPutLit (lens, data, Some t), Types.make_list_type t)
      | `TableLit (name, (dtype, Some (read_row, write_row, needed_row)), constraints, keys, db) ->
          let (o, name, _) = o#phrase name in
          let (o, db, _) = o#phrase db in
          let (o, dtype) = o#sugar_datatype dtype in
          let (o, read_row) = o#datatype read_row in
          let (o, write_row) = o#datatype write_row in
          let (o, needed_row) = o#datatype needed_row in
            (o, `TableLit (name, (dtype, Some (read_row, write_row, needed_row)), constraints, keys, db), `Table (read_row, write_row, needed_row))
      | `DBDelete (p, from, where) ->
          let (o, from, _) = o#phrase from in
          let (o, p) = o#pattern p in
            (* BUG:

               We should really reset the environment: variables bound
               by p shouldn't be visible in subsequent expression.

               The same applies to `DBUpdate and `Iteration.
            *)
          let (o, where, _) = option o (fun o -> o#phrase) where in
            (o, `DBDelete (p, from, where), Types.unit_type)
      | `DBInsert (into, labels, values, id) ->
          let (o, into, _) = o#phrase into in
          let (o, values, _) = o#phrase values in
          let (o, id, _) = option o (fun o -> o#phrase) id in
            (o, `DBInsert (into, labels, values, id), Types.unit_type)
      | `DBUpdate (p, from, where, set) ->
          let (o, from, _) = o#phrase from in
          let (o, p) = o#pattern p in
          let (o, where, _) = option o (fun o -> o#phrase) where in
          let (o, set) =
            listu o
              (fun o (name, value) ->
                 let (o, value, _) = o#phrase value in (o, (name, value)))
              set
          in
            (o, `DBUpdate (p, from, where, set), Types.unit_type)
      | `Xml (tag, attrs, attrexp, children) ->
          let (o, attrs) =
            listu o
              (fun o (name, value) ->
                 let (o, value, _) = list o (fun o -> o#phrase) value in
                   (o, (name, value)))
              attrs in
          let (o, attrexp, _) = option o (fun o -> o#phrase) attrexp in
          let (o, children, _) = list o (fun o -> o#phrase) children in
            (o, `Xml (tag, attrs, attrexp, children), Types.xml_type)
      | `TextNode s -> (o, `TextNode s, Types.xml_type)
      | `Formlet (body, yields) ->
         let envs = o#backup_envs in
         let (o, body, _) = o#phrase body in
         (* ensure that the formlet bindings are only in scope in the
            yields clause *)
         let o = o#with_var_env (TyEnv.extend (o#get_var_env ()) (o#get_formlet_env ())) in
         let (o, yields, t) = o#phrase yields in
         let o = o#restore_envs envs in
         (o, `Formlet (body, yields), Instantiate.alias "Formlet" [`Type t] tycon_env)
      | `Page e -> let (o, e, _) = o#phrase e in (o, `Page e, Instantiate.alias "Page" [] tycon_env)
      | `FormletPlacement (f, h, attributes) ->
          let (o, f, _) = o#phrase f in
          let (o, h, _) = o#phrase h in
          let (o, attributes, _) = o#phrase attributes in
            (o, `FormletPlacement (f, h, attributes), Types.xml_type)
      | `PagePlacement e ->
          let (o, e, _) = o#phrase e in (o, `PagePlacement e, Types.xml_type)
      | `FormBinding (f, p) ->
         let envs = o#backup_envs in
         let (o, f, _) = o#phrase f in
         (* HACK: add the formlet bindings to the formlet environment *)
         let o = o#with_var_env TyEnv.empty in
         let (o, p) = o#pattern p in
         let formlet_env = TyEnv.extend formlet_env (o#get_var_env()) in
         let o = o#restore_envs envs in
         let o = o#with_formlet_env formlet_env in
         (* let o = {< formlet_env=TyEnv.extend formlet_env (o#get_var_env()) >} in *)
         (o, `FormBinding (f, p), Types.xml_type)
      | e -> failwith ("oops: "^show_phrasenode  e)

    method phrase : phrase -> ('self_type * phrase * Types.datatype) =
      fun {node; pos} ->
        let (o, node, t) = o#phrasenode node in
        (o, {node;pos}, t)

    method patternnode : patternnode -> ('self_type * patternnode) =
      function
      | `Any -> (o, `Any)
      | `Nil -> (o, `Nil)
      | `Cons (p, ps) ->
          let (o, p) = o#pattern p in
          let (o, ps) = o#pattern ps in (o, `Cons (p, ps))
      | `List p ->
          let (o, p) = listu o (fun o -> o#pattern) p in (o, `List p)
      | `Variant (name, p) ->
          let (o, p) = optionu o (fun o -> o#pattern) p
          in (o, `Variant (name, p))
      | `Effect (name, ps, k) ->
         let (o, ps) = listu o (fun o -> o#pattern) ps in
         let (o, k)  = o#pattern k in
         (o, `Effect (name, ps, k))
      | `Negative name -> (o, `Negative name)
      | `Record (fields, rest) ->
          let (o, fields) =
            listu o
              (fun o (name, p) ->
                 let (o, p) = o#pattern p in (o, (name, p)))
              fields in
          let (o, rest) = optionu o (fun o -> o#pattern) rest
          in (o, `Record (fields, rest))
      | `Tuple ps ->
          let (o, ps) = listu o (fun o -> o#pattern) ps in (o, `Tuple ps)
      | `Constant c -> let (o, c, _) = o#constant c in (o, `Constant c)
      | `Variable x -> let (o, x) = o#binder x in (o, `Variable x)
      | `As (x, p) ->
          let (o, x) = o#binder x in
          let (o, p) = o#pattern p in (o, (`As (x, p)))
      | `HasType (p, t) ->
          let (o, p) = o#pattern p in (o, (`HasType (p, t)))

    method pattern : pattern -> ('self_type * pattern) =
      fun {node; pos} ->
        let (o, node) = o#patternnode node in (o, {node; pos})

    method iterpatt : iterpatt -> ('self_type * iterpatt) =
      function
      | `List (p, e) ->
          let (o, e, _) = o#phrase e in
          let (o, p) = o#pattern p in
            (o, `List (p, e))
      | `Table (p, e) ->
          let (o, e, _) = o#phrase e in
          let (o, p) = o#pattern p in
           (o, `Table (p, e))

    method funlit : Types.row -> funlit -> ('self_type * funlit * Types.datatype) =
      fun inner_eff (pss, e) ->
        let envs = o#backup_envs in
        let (o, pss) = listu o (fun o -> listu o (fun o -> o#pattern)) pss in
        let o = o#with_effects inner_eff in
        let (o, e, t) = o#phrase e in
        let o = o#restore_envs envs in
        (o, (pss, e), t)

    method handlerlit : Types.datatype -> handlerlit -> ('self_type * handlerlit * Types.datatype) =
      fun _ _ -> failwith "transformSugar.ml: method handlerlit not yet implemented!" (*
      let envs = o#backup_envs in
      let (o, m) =
	match m with
	  `Phrase p  -> let (o, m) = o#phrase p in (o, `Phrase m)
	| `Pattern p -> let (o, m) = o#pattern p in (o, `Pattern m)
      in
      let (o, cases) =
        listu o
	      (fun o (p, e) ->
               let (o, p) = o#pattern p in
               let (o, e, _) = o#phrase e in (o, (p, e)))
	      cases
      in
      let o = o#restore_envs envs in
      (o, (m, cases, params), t)*)

    method constant : constant -> ('self_type * constant * Types.datatype) =
      function
        | `Float v -> (o, `Float v, Types.float_type)
        | `Int v -> (o, `Int v, Types.int_type)
        | `String v -> (o, `String v, Types.string_type)
        | `Bool v -> (o, `Bool v, Types.bool_type)
        | `Char v -> (o, `Char v, Types.char_type)

    method quantifiers : Types.quantifier list -> ('self_type * Types.quantifier list) =
      fun qs -> (o, qs)
    method backup_quantifiers : IntSet.t = IntSet.empty
    method restore_quantifiers : IntSet.t -> 'self_type = fun _ -> o

    method rec_bodies :
      (binder * declared_linearity * ((tyvar list * (Types.datatype * Types.quantifier option list) option) * funlit) * location * datatype' option * position) list ->
      ('self_type *
         (binder * declared_linearity * ((tyvar list * (Types.datatype * Types.quantifier option list) option) * funlit) * location * datatype' option * position) list) =
      let outer_tyvars = o#backup_quantifiers in
      let rec list o =
        function
          | [] -> (o, [])
          | (f, lin, ((tyvars, Some (inner, extras)), lam), location, t, pos)::defs ->
              let (o, tyvars) = o#quantifiers tyvars in
              let (o, inner) = o#datatype inner in
              let inner_effects = fun_effects inner (fst lam) in
              let (o, lam, _) = o#funlit inner_effects lam in
              let o = o#restore_quantifiers outer_tyvars in
              let (o, defs) = list o defs in
                (o, (f, lin, ((tyvars, Some (inner, extras)), lam), location, t, pos)::defs)
          | _ :: _ -> assert false
      in
        list o

    method rec_activate_outer_bindings :
      (binder * declared_linearity * ((tyvar list * (Types.datatype * Types.quantifier option list) option) * funlit) * location * datatype' option * position) list ->
      ('self_type *
         (binder * declared_linearity * ((tyvar list * (Types.datatype * Types.quantifier option list) option) * funlit) * location * datatype' option * position) list) =
      let rec list o =
        function
          | [] -> o, []
          | (f, lin, body, location, t, pos)::defs ->
              let (o, f) = o#binder f in
              let (o, defs) = list o defs in
              let (o, t) = optionu o (fun o -> o#datatype') t in
                o, (f, lin, body, location, t, pos)::defs
      in
        list o

    method rec_activate_inner_bindings :
      (binder * declared_linearity * ((tyvar list * (Types.datatype * Types.quantifier option list) option) * funlit) * location * datatype' option * position) list ->
      'self_type =
      let rec list o =
        function
          | [] -> o
          | (f, _, ((_tyvars, Some (inner, _extras)), _lam), _location, _t, _pos)::defs ->
              let (o, _) = o#binder (set_binder_type f inner) in
                list o defs
          | _ :: _ -> assert false
      in
        list o

    method bindingnode : bindingnode -> ('self_type * bindingnode) =
      function
      | `Val (p, (tyvars, e), location, t) ->
         let outer_tyvars = o#backup_quantifiers in
         let (o, tyvars) = o#quantifiers tyvars in
         let (o, e, _) = o#phrase e in
         let o = o#restore_quantifiers outer_tyvars in
         let (o, p) = o#pattern p in
         let (o, t) = optionu o (fun o -> o#datatype') t in
         (o, `Val (p, (tyvars, e), location, t))
      | `Fun (bndr, lin, (tyvars, lam), location, t) when binder_has_type bndr ->
         let outer_tyvars = o#backup_quantifiers in
         let (o, tyvars) = o#quantifiers tyvars in
         let inner_effects = fun_effects (type_of_binder_exn bndr) (fst lam) in
         let (o, lam, _) = o#funlit inner_effects lam in
         let o = o#restore_quantifiers outer_tyvars in
         let (o, bndr) = o#binder bndr in
         let (o, t) = optionu o (fun o -> o#datatype') t in
         (o, `Fun (bndr, lin, (tyvars, lam), location, t))
      | `Fun _ -> failwith "Unannotated non-recursive function binding"
      | `Funs defs ->
         (* put the inner bindings in the environment *)
         let o = o#rec_activate_inner_bindings defs in

         (* transform the function bodies *)
         let (o, defs) = o#rec_bodies defs in

         (* put the outer bindings in the environment *)
         let o, defs = o#rec_activate_outer_bindings defs in
         (o, (`Funs defs))
      | `Handler _ -> assert false
      | `Foreign (f, raw_name, language, file, t) ->
         let (o, f) = o#binder f in
         (o, `Foreign (f, raw_name, language, file, t))
      | `Type (name, vars, (_, Some dt)) as e ->
         let tycon_env = TyEnv.bind tycon_env (name, `Alias (List.map (snd ->- val_of) vars, dt)) in
         {< tycon_env=tycon_env >}, e
      | `Type _ -> failwith "Unannotated type alias"
      | `Infix -> (o, `Infix)
      | `Exp e -> let (o, e, _) = o#phrase e in (o, `Exp e)
      | `AlienBlock _ -> assert false
      | `Module _ -> assert false
      | `QualifiedImport _ -> assert false

    method binding : binding -> ('self_type * binding) =
      fun {node; pos} ->
        let (o, node) = o#bindingnode node in (o, {node; pos})

    method binder : binder -> ('self_type * binder) =
      fun bndr ->
      assert (binder_has_type bndr);
      let var_env = TyEnv.bind var_env (name_of_binder bndr, type_of_binder_exn bndr) in
      ({< var_env=var_env >}, bndr)

    method cp_phrase : cp_phrase -> ('self_type * cp_phrase * Types.datatype) =
      fun {node; pos} ->
      let (o, node, t) = o#cp_phrasenode node in
      (o, {node; pos}, t)

    (* TODO: should really invoke o#datatype on type annotations! *)
    method cp_phrasenode : cp_phrasenode -> ('self_type * cp_phrasenode * Types.datatype) = function
      | `Unquote (bs, e) ->
         let envs = o#backup_envs in
         let (o, bs) = listu o (fun o -> o#binding) bs in
         let (o, e, t) = o#phrase e in
         let o = o#restore_envs envs in
         o, `Unquote (bs, e), t
      | `Grab (cbind, None, p) ->
         let (o, p, t) = o#cp_phrase p in
         o, `Grab (cbind, None, p), t
      | `Grab ((c, Some (`Input (_a, s), _grab_tyargs) as cbind), Some b, p) -> (* FYI: a = u *)
         let envs = o#backup_envs in
         let (o, b) = o#binder b in
         let venv = TyEnv.bind (o#get_var_env ()) (c, s) in
         let o = {< var_env = venv >} in
         let (o, p, t) = o#cp_phrase p in
         let o = o#restore_envs envs in
         o, `Grab (cbind, Some b, p), t
      | `Give ((c, Some (`Output (_t, s), _tyargs) as cbind), e, p) ->
         let envs = o#backup_envs in
         let o = {< var_env = TyEnv.bind (o#get_var_env ()) (c, s) >} in
         let (o, e, _typ) = option o (fun o -> o#phrase) e in
         let (o, p, t) = o#cp_phrase p in
         let o = o#restore_envs envs in
         o, `Give (cbind, e, p), t
      | `GiveNothing c ->
         let envs = o#backup_envs in
         let o, c = o#binder c in
         let o = o#restore_envs envs in
         o, `GiveNothing c, Types.make_endbang_type
      | `Grab _ -> failwith "Malformed grab in TransformSugar"
      | `Give _ -> failwith "Malformed give in TransformSugar"
      | `Select (b, label, p) ->
         let envs = o#backup_envs in
         let o, b = o#binder b in
         let (o, p, t) = o#cp_phrase p in
         let o = o#restore_envs envs in
         o, `Select (b, label, p), t
      | `Offer (b, cases) ->
         let (o, cases) = List.fold_right (fun (label, p) (o, cases) ->
                                           let envs = o#backup_envs in
                                           let o, _ = o#binder b in
                                           let (o, p, t) = o#cp_phrase p in
                                           (o#restore_envs envs, ((label, p), t) :: cases)) cases (o, []) in
         begin
           match List.split cases with
           | cases, t :: _ts ->
              o, `Offer (b, cases), t
           | _ -> assert false
         end
      | `Link (c, d) -> o, `Link (c, d), Types.unit_type
      | `Comp ({node = c, Some s; _} as bndr, left, right) ->
         let envs = o#backup_envs in
         let (o, left, _typ) = {< var_env = TyEnv.bind (o#get_var_env ()) (c, s) >}#cp_phrase left in
         let whiny_dual_type s = try Types.dual_type s with Invalid_argument _ -> raise (Invalid_argument ("Attempted to dualize non-session type " ^ Types.string_of_datatype s)) in
         let (o, right, t) = {< var_env = TyEnv.bind (o#get_var_env ()) (c, whiny_dual_type s) >}#cp_phrase right in
         let o = o#restore_envs envs in
         o, `Comp (bndr, left, right), t
      | `Comp _ -> assert false
  end
