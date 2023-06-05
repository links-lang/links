open CommonTypes
open Operators
open Sugartypes
open SourceCode
open SourceCode.WithPos
open Utility
open Types

module TyEnv = Env.String

type program_transformer  = Types.typing_environment -> Sugartypes.program  -> Sugartypes.program
type sentence_transformer = Types.typing_environment -> Sugartypes.sentence -> Sugartypes.sentence

let internal_error message =
  Errors.internal_error ~filename:"transformSugar.ml" ~message

let type_section env =
  let open Section in function
  | Minus -> TyEnv.find "-" env
  | FloatMinus -> TyEnv.find "-."env
  | Project label ->
      let ab, a = Types.fresh_type_quantifier (lin_any, res_any) in
      let rhob, row  = fresh_row_quantifier (lin_any, res_any) in
      let (fields, rho, _) = TypeUtils.extract_row_parts row in
      let eb, e = Types.fresh_row_quantifier default_effect_subkind in

      let r = Record (Row (StringMap.add label (Present a) fields, rho, false)) in
        ForAll ([ab; rhob; eb],
                Function (Types.make_tuple_type [r], e, a))
  | Name var -> TyEnv.find var env

let type_unary_op env tycon_env =
  let datatype = DesugarDatatypes.read ~aliases:tycon_env in function
    | UnaryOp.Minus      -> datatype "(Int) -> Int"
    | UnaryOp.FloatMinus -> datatype "(Float) -> Float"
    | UnaryOp.Name n     -> TyEnv.find n env

let type_binary_op env tycon_env =
  let open BinaryOp in
  let datatype = DesugarDatatypes.read ~aliases:tycon_env in function
  | Minus        -> TyEnv.find "-" env
  | FloatMinus   -> TyEnv.find "-." env
  | RegexMatch flags ->
      let nativep  = List.exists ((=) RegexNative)  flags
      and listp    = List.exists ((=) RegexList)    flags
      and replacep = List.exists ((=) RegexReplace) flags in
        (match replacep, listp, nativep with
           | true,   _   , false -> (* stilde  *) datatype "(String, Regex) -> String"
           | false, true , false -> (* ltilde *)  datatype "(String, Regex) -> [String]"
           | false, false, false -> (* tilde *)   datatype "(String, Regex) -> Bool"
           | _,     _,     true  -> assert false)

  | And
  | Or           -> datatype "(Bool,Bool) -> Bool"
  | Cons         -> TyEnv.find "Cons" env
  | Name "++"    -> TyEnv.find "Concat" env
  | Name ">"
  | Name ">="
  | Name "=="
  | Name "<"
  | Name "<="
  | Name "<>" ->
      let ab, a = Types.fresh_type_quantifier (lin_any, res_any) in
      let eb, e = Types.fresh_row_quantifier (lin_any, res_any) in
        ForAll ([ab; eb],
                Function (Types.make_tuple_type [a; a], e,
                          Primitive Primitive.Bool ))
  | Name "!"     -> TyEnv.find "Send" env
  | Name n       -> TyEnv.find n env

let fun_effects t pss =
  let rec get_eff =
    function
      | Function (_, effects, _), [_]
      | Lolli    (_, effects, _), [_] -> effects
      | Function (_, _, t), _::pss
      | Lolli    (_, _, t), _::pss -> get_eff (TypeUtils.concrete_type t, pss)
      | _ -> assert false in
  let t =
    match TypeUtils.concrete_type t with
      | ForAll (_, body) -> TypeUtils.concrete_type body
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

let on_effects o (eff : Types.row) fn x =
  let effect_row = o#lookup_effects in
  let o = o#with_effects eff in
  let (o, x, y) = fn o x in
  (o#with_effects effect_row, x, y)

let check_type_application (e, t) k =
  begin
    try
      k ()
    with Instantiate.ArityMismatch (exp, prov) ->
      prerr_endline ("Arity mismatch in type application");
      prerr_endline ("Expression: " ^ show_phrasenode e);
      prerr_endline ("Type: "^Types.string_of_datatype t);
      raise (Instantiate.ArityMismatch (exp, prov))
  end

class transform (env : Types.typing_environment) =
  let open PrimaryKind in
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

    method bind_tycon name tycon =
      {< tycon_env = TyEnv.bind name tycon tycon_env >}

    method bind_binder bndr =
      {< var_env = TyEnv.bind (Binder.to_name bndr)  (Binder.to_type bndr) var_env >}

    method lookup_type : Name.t -> Types.datatype = fun var ->
      TyEnv.find var var_env

    method lookup_effects : Types.row = effect_row

    method with_effects : Types.row -> 'self_type = fun effects ->
      {< effect_row = fst (Types.unwrap_row effects) >}

    method sugar_datatype : Datatype.with_pos -> ('self_type * Datatype.with_pos) =
      fun s -> (o, s)

    method datatype : Types.datatype -> ('self_type * Types.datatype) =
      fun t -> (o, t)

    method datatype' : datatype' -> ('self_type * datatype') =
      fun (s, t) ->
        let (o, s) = o#sugar_datatype s in
        let (o, t) = optionu o (fun o -> o#datatype) t in
          (o, (s, t))

    method lens_type : Lens.Type.t -> ('self_type * Lens.Type.t) =
      fun sort ->
            (o, sort)

    method row : Types.row -> ('self_type * Types.row) =
      fun row -> (o, row)

    method unary_op : UnaryOp.t -> ('self_type * UnaryOp.t * Types.datatype) =
      fun op ->
        (o, op, type_unary_op var_env tycon_env op)

    method binop : BinaryOp.t -> ('self_type * BinaryOp.t * Types.datatype) =
      fun op ->
        (o, op, type_binary_op var_env tycon_env op)

    method section : Section.t -> ('self_type * Section.t * Types.datatype) =
      fun section ->
        (o, section, type_section var_env section)

    method sentence : sentence -> ('self_type * sentence * Types.datatype option) =
      function
      | Definitions defs ->
         let (o, defs) =
           listu o (fun o -> o#binding) defs
         in (o, Definitions defs, Some Types.unit_type)
      | Expression e ->
         let (o, e, t) = o#phrase e in
         (o, Expression e, Some t)
      | Directive d ->
         (o, Directive d, None)

    method regex : regex -> ('self_type * regex) =
      function
        | (Range _ | Simply _ | Any  | StartAnchor | EndAnchor) as r -> (o, r)
        | Quote r -> let (o, r) = o#regex r in (o, Quote r)
        | Seq rs ->
            let (o, rs) = listu o (fun o -> o#regex) rs in (o, Seq rs)
        | Alternate (r1, r2) ->
            let (o, r1) = o#regex r1 in
            let (o, r2) = o#regex r2 in
              (o, Alternate (r1, r2))
        | Group r -> let (o, r) = o#regex r in (o, Group r)
        | Repeat (repeat, r) ->
            let (o, r) = o#regex r in (o, Repeat (repeat, r))
        | Splice e -> let (o, e, _) = o#phrase e in (o, Splice e)
        | Replace (r, Literal s) ->
            let (o, r) = o#regex r in
              (o, Replace (r, Literal s))
        | Replace (r, SpliceExpr e) ->
            let (o, r) = o#regex r in
            let (o, e, _) = o#phrase e in
              (o, Replace (r, SpliceExpr e))

    method program : program -> ('self_type * program * Types.datatype option) =
      fun (bs, e) ->
        let (o, bs) = listu o (fun o -> o#binding) bs in
        let (o, e, t) = option o (fun o -> o#phrase) e
        in (o, (bs, e), opt_map Types.normalise_datatype t)

    method given_spawn_location :
      given_spawn_location ->
      ('self_type * given_spawn_location) = function
        | ExplicitSpawnLocation p ->
            let (o, phr, _phr_ty) = o#phrase p in
            (o, ExplicitSpawnLocation phr)
        | l -> (o, l)

    method temporal_update : temporal_update -> ('self_type * temporal_update) =
        function
            | ValidTimeUpdate (SequencedUpdate { validity_from; validity_to }) ->
                let (o, validity_from, _) = o#phrase validity_from in
                let (o, validity_to, _) = o#phrase validity_to in
                (o, ValidTimeUpdate (SequencedUpdate { validity_from; validity_to }))
            | ValidTimeUpdate (NonsequencedUpdate { from_time; to_time }) ->
                let (o, from_time, _) = option o (fun o -> o#phrase) from_time in
                let (o, to_time, _) = option o (fun o -> o#phrase) to_time in
                (o, ValidTimeUpdate (NonsequencedUpdate { from_time; to_time }))
            | upd -> (o, upd)

    method temporal_deletion : temporal_deletion -> ('self_type * temporal_deletion) =
        function
            | ValidTimeDeletion (SequencedDeletion { validity_from; validity_to }) ->
                let (o, validity_from, _) = o#phrase validity_from in
                let (o, validity_to, _) = o#phrase validity_to in
                (o, ValidTimeDeletion (SequencedDeletion { validity_from; validity_to }))
            | del -> (o, del)

    method phrasenode : phrasenode -> ('self_type * phrasenode * Types.datatype) =
      function
      | Constant c -> let (o, c, t) = o#constant c in (o, Constant c, t)
      | Sugartypes.Var var -> (o, Sugartypes.Var var, o#lookup_type var)
      | FreezeVar var -> (o, FreezeVar var, o#lookup_type var)
      | FunLit (Some argss, lin, lam, location) ->
          let inner_e = snd (last argss) in
          let (o, lam, rt) = o#funlit inner_e lam in
          let (o, t) =
            List.fold_right
              (fun (args, effects) (o, rt) ->
                 let (o, args) = o#datatype args in
                 let (o, row) = o#row effects in
                   (o, Function (args, row, rt)))
              argss
              (o, rt)
          in
            (o, FunLit (Some argss, lin, lam, location), t)
      | Spawn (Wait, loc, body, Some inner_effects) ->
          assert (loc = NoSpawnLocation);
          (* bring the inner effects into scope, then restore the
             environments afterwards *)
          let envs = o#backup_envs in
          let (o, inner_effects) = o#row inner_effects in
          let o = o#with_effects inner_effects in
          let (o, body, body_type) = o#phrase body in
          let o = o#restore_envs envs in
            (o, Spawn (Wait, loc, body, Some inner_effects), body_type)
      | Spawn (k, spawn_loc, body, Some inner_effects) ->
          (* bring the inner effects into scope, then restore the
             environments afterwards *)
          let (o, spawn_loc) = o#given_spawn_location spawn_loc in
          let envs = o#backup_envs in
          let (o, inner_effects) = o#row inner_effects in
          let process_type = Application (Types.process, [(Row, inner_effects)]) in
          let o = o#with_effects inner_effects in
          let (o, body, _) = o#phrase body in
          let o = o#restore_envs envs in
            (o, Spawn (k, spawn_loc, body, Some inner_effects), process_type)
      | Sugartypes.Select (l, e) ->
         let (o, e, t) = o#phrase e in
         (o, Sugartypes.Select (l, e), TypeUtils.select_type l t)
      | Offer (e, bs, Some t) ->
          let (o, e, _) = o#phrase e in
          let (o, bs) =
            listu o
              (fun o (p, e) ->
                 let (o, p) = o#pattern p in
                 let (o, e, _) = o#phrase e in (o, (p, e)))
              bs in
          let (o, t) = o#datatype t in
            (o, Offer (e, bs, Some t), t)
      | CP p ->
         let (o, p, t) = o#cp_phrase p in
         (o, CP p, t)
      | Query (range, policy, body, Some t) ->
          let (o, range) =
            optionu o
              (fun o (limit, offset) ->
                 let (o, limit, _) = o#phrase limit in
                 let (o, offset, _) = o#phrase offset in
                   (o, (limit, offset)))
              range in
          let (o, body, _) = on_effects o (Types.make_empty_closed_row ()) (fun o -> o#phrase) body in
          let (o, body, _) = o#phrase body in
          let (o, t) = o#datatype t in
            (o, Query (range, policy, body, Some t), t)
      | ListLit (es, Some t) ->
          let (o, es, _) = list o (fun o -> o#phrase) es in
          let (o, t) = o#datatype t in
            (o, ListLit (es, Some t), Types.make_list_type t)
      | RangeLit (e1, e2) ->
          let (o, e1, _) = o#phrase e1 in
          let (o, e2, _) = o#phrase e2 in
            (o, RangeLit (e1, e2), Types.make_list_type Types.int_type)
      | Iteration (gens, body, cond, orderby) ->
          let envs = o#backup_envs in
          let (o, gens) = listu o (fun o -> o#iterpatt) gens in
          let (o, body, t) = o#phrase body in
          let (o, cond, _) = option o (fun o -> o#phrase) cond in
          let (o, orderby, _) = option o (fun o -> o#phrase) orderby in
          let o = o#restore_envs envs in
            (o, Iteration (gens, body, cond, orderby), t)
      | Escape (b, e) ->
          let envs = o#backup_envs in
          let (o, b) = o#binder b in
          let (o, e, t) = o#phrase e in
          let o = o#restore_envs envs in
            (o, Escape (b, e), t)
      | Section sec -> (o, Section sec, type_section var_env sec)
      | FreezeSection sec -> (o, FreezeSection sec, type_section var_env sec)
      | Conditional (p, e1, e2) ->
          let (o, p, _) = o#phrase p in
          let (o, e1, t) = o#phrase e1 in
          let (o, e2, _) = o#phrase e2
          in (o, Conditional (p, e1, e2), t)
      | Block (bs, e) ->
          let envs = o#backup_envs in
          let (o, bs) = listu o (fun o -> o#binding) bs in
          let (o, e, t) = o#phrase e in
          let o = o#restore_envs envs in
            o, Block (bs, e), t
      | InfixAppl ((tyargs, op), e1, e2) ->
          let (o, op, t) = o#binop op in
            check_type_application
              (InfixAppl ((tyargs, op), e1, e2), t)
              (fun () ->
                 let t = TypeUtils.return_type (Instantiate.apply_type t tyargs) in
                 let (o, e1, _) = o#phrase e1 in
                 let (o, e2, _) = o#phrase e2 in
                   (o, InfixAppl ((tyargs, op), e1, e2), t))
      | Regex r ->
          let (o, r) = o#regex r in
            (o, Regex r, Instantiate.alias "Regex" [] tycon_env)
      | UnaryAppl ((tyargs, op), e) ->
          let (o, op, t) = o#unary_op op in
            check_type_application
              (UnaryAppl ((tyargs, op), e), t)
              (fun () ->
                 let t = TypeUtils.return_type (Instantiate.apply_type t tyargs) in
                 let (o, e, _) = o#phrase e in
                   (o, UnaryAppl ((tyargs, op), e), t))
      | FnAppl (f, args) ->
         let (o, f, ft) = o#phrase f in
         let (o, args, _) = list o (fun o -> o#phrase) args in
            (o, FnAppl (f, args), TypeUtils.return_type ft)
      | TAbstr (tyvars, e) ->
          let outer_tyvars = o#backup_quantifiers in
          let (o, sqs) = o#quantifiers tyvars in
          let (o, e, t) = o#phrase e in
          let o = o#restore_quantifiers outer_tyvars in
          let qs = List.map SugarQuantifier.get_resolved_exn sqs in
          let t = Types.for_all (qs, t) in
            (o, tabstr (sqs, e.node), t)
      | TAppl (e, tyargs) ->
          let (o, e, t) = o#phrase e in
            check_type_application
              (TAppl (e, tyargs), t)
              (fun () ->
                 let t = Instantiate.apply_type t (List.map (snd ->- val_of) tyargs) in
                   (o, TAppl (e, tyargs), t))
      | TupleLit [e] ->
          (* QUESTION:

             Why do we type 1-tuples as if they aren't tuples?
          *)
          let (o, e, t) = o#phrase e in
            (o, TupleLit [e], t)
      | TupleLit es ->
          let (o, es, ts) = list o (fun o -> o#phrase) es in
            (o, TupleLit es, Types.make_tuple_type ts)
      | RecordLit (fields, base) ->
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
                      | Record row ->
                          Record (Types.extend_row field_types row)
                      | t ->
                          Debug.print ("bad t: " ^ Types.string_of_datatype t);
                          assert false
                  end
          in
            (o, RecordLit (fields, base), t)
      | Projection (e, name) ->
          let (o, e, t) = o#phrase e in
          (o, Projection (e, name), TypeUtils.project_type name t)
      | With (e, fields) ->
          let (o, e, t) = o#phrase e in
          let o, fields, ts =
            list o
              (fun o (name, e) -> let o, e, t = o#phrase e in (o, (name, e), t))
              fields
          in
          let t = match Types.concrete_type t with
            | Record row ->
               let  ( fs, rv, closed ) =
                 Types.flatten_row row |> TypeUtils.extract_row_parts
               in
               let fs = List.fold_left2 (fun fs (name, _) t -> StringMap.add name (Present t) fs) fs fields ts in
               Record (Row (fs, rv, closed))
            | _ -> t
          in
          (o, With (e, fields), t)
      | TypeAnnotation (e, ann_type) ->
          let (o, e, _) = o#phrase e in
          let (o, ann_type) = o#datatype' ann_type in
          let t = val_of (snd ann_type) in
          (o, TypeAnnotation (e, ann_type), t)
      | Upcast (e, to_type, from_type) ->
          let (o, e, _) = o#phrase e in
          let (o, to_type) = o#datatype' to_type in
          let (o, from_type) = o#datatype' from_type in
          let t = val_of (snd to_type) in
            (o, Upcast (e, to_type, from_type), t)
      | ConstructorLit (name, e, Some t) ->
          let (o, e, _) = option o (fun o -> o#phrase) e in
          let (o, t) = o#datatype t in
          (o, ConstructorLit (name, e, Some t), t)
      | DoOperation (name, ps, Some t, b) ->
         let (o, ps, _) = list o (fun o -> o#phrase) ps in
         (o, DoOperation (name, ps, Some t, b), t)
      | Handle { sh_expr; sh_effect_cases; sh_value_cases; sh_descr } ->
         let (input_row, input_t, output_row, output_t) = sh_descr.shd_types in
         let (o, expr, _) = o#phrase sh_expr in
         let envs = o#backup_envs in
         let (o, params) =
           match sh_descr.shd_params with
           | Some params ->
              let (o, bindings) =
                List.fold_right
                  (fun (pat, body) (o, bindings) ->
                    let (o, body, _) = o#phrase body in
                    let (o, pat) = o#pattern pat in
                    (o, (pat, body) :: bindings))
                  params.shp_bindings (o, [])
              in
              (o, Some { params with shp_bindings = bindings })
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
         (o, Handle { sh_expr = expr; sh_effect_cases = eff_cases; sh_value_cases = val_cases; sh_descr = descr }, output_t)
      | TryInOtherwise (try_phr, as_pat, as_phr, otherwise_phr, (Some dt)) ->
          let (o, try_phr, _) = o#phrase try_phr in
          let (o, as_pat) = o#pattern as_pat in
          let (o, as_phr, _) = o#phrase as_phr in
          let (o, otherwise_phr, _) = o#phrase otherwise_phr in
          let (o, dt) = o#datatype dt in
          (o, TryInOtherwise (try_phr, as_pat, as_phr, otherwise_phr, (Some dt)), dt)
      | Raise -> (o, Raise, Types.fresh_type_variable (lin_any, res_any))
      | Switch (v, cases, Some t) ->
          let (o, v, _) = o#phrase v in
          let (o, cases) =
            listu o
              (fun o (p, e) ->
                 let (o, p) = o#pattern p in
                 let (o, e, _) = o#phrase e in (o, (p, e)))
              cases in
          let (o, t) = o#datatype t in
            (o, Switch (v, cases, Some t), t)
      | Receive (cases, Some t) ->
          let (o, cases) =
            listu o
              (fun o (p, e) ->
                 let (o, p) = o#pattern p in
                 let (o, e, _) = o#phrase e in (o, (p, e)))
              cases in
          let (o, t) = o#datatype t in
            (o, Receive (cases, Some t), t)
      | DatabaseLit (name, (driver, args)) ->
          let (o, name, _) = o#phrase name in
          let (o, driver, _) = option o (fun o -> o#phrase) driver in
          let (o, args, _) = option o (fun o -> o#phrase) args in
            (o, DatabaseLit (name, (driver, args)), Primitive Primitive.DB)
      | LensLit (table, Some t) ->
         let (o, table, _) = o#phrase table in
         let (o, t) = o#lens_type t in
            (o, LensLit (table, Some t), Lens t)
      | LensSerialLit (lens,columns,Some t) ->
         let (o, lens, _) = o#phrase lens in
         let (o, t) = o#lens_type t in
            (o, LensSerialLit (lens, columns, Some t), Lens t)
      | LensDropLit (lens, drop, key, default, Some t) ->
          let (o, lens, _) = o#phrase lens in
          let (o, t) = o#lens_type t in
          let (o, default, _) = o#phrase default in
            (o, LensDropLit (lens, drop, key, default, Some t), Lens t)
      | LensSelectLit (lens, predicate, Some t) ->
          let (o, lens, _) = o#phrase lens in
          let (o, predicate, _) = o#phrase predicate in
          let (o, t) = o#lens_type t in
            (o, LensSelectLit (lens, predicate, Some t), Lens t)
      | LensJoinLit (lens1, lens2, on, left, right, Some t) ->
          let (o, lens1, _) = o#phrase lens1 in
          let (o, lens2, _) = o#phrase lens2 in
          let (o, t) = o#lens_type t in
            (o, LensJoinLit (lens1, lens2, on, left, right, Some t), Lens t)

      | LensCheckLit (lens, Some t) ->
          let (o, lens, _) = o#phrase lens in
          let (o, t) = o#lens_type t in
            (o, LensCheckLit (lens, Some t), Lens t)
      | LensGetLit (lens, Some t) ->
          let (o, lens, _) = o#phrase lens in
          let (o, t) = o#datatype t in
            (o, LensGetLit (lens, Some t), Types.make_list_type t)
      | LensPutLit (lens, data, Some t) ->
          let (o, lens, _) = o#phrase lens in
          let (o, data, _) = o#phrase data in
          let (o, t) = o#datatype t in
            (o, LensPutLit (lens, data, Some t), Types.make_list_type t)
      | TableLit {
            tbl_name;
            tbl_type = (tmp, dtype, Some (read_row, write_row, needed_row));
            tbl_field_constraints;
            tbl_keys; tbl_temporal_fields; tbl_database
         } ->
          let (o, tbl_name, _) = o#phrase tbl_name in
          let (o, tbl_database, _) = o#phrase tbl_database in
          let (o, dtype) = o#sugar_datatype dtype in
          let (o, read_row) = o#datatype read_row in
          let (o, write_row) = o#datatype write_row in
          let (o, needed_row) = o#datatype needed_row in
          let tbl =
              TableLit {
                  tbl_name;
                  tbl_type = (tmp, dtype, Some (read_row, write_row, needed_row));
                  tbl_field_constraints;
                  tbl_keys;
                  tbl_temporal_fields;
                  tbl_database
              }
          in
          (o, tbl, Table (tmp, read_row, write_row, needed_row))
      | DBDelete (del, p, from, where) ->
          let (o, del) = optionu o (fun o -> o#temporal_deletion) del in
          let (o, from, _) = o#phrase from in
          let (o, p) = o#pattern p in
            (* BUG:

               We should really reset the environment: variables bound
               by p shouldn't be visible in subsequent expression.

               The same applies to DBUpdate and Iteration.
            *)
          let (o, where, _) = option o (fun o -> o#phrase) where in
            (o, DBDelete (del, p, from, where), Types.unit_type)
      | DBInsert (tmp, into, labels, values, id) ->
          let (o, into, _) = o#phrase into in
          let (o, values, _) = o#phrase values in
          let (o, id, _) = option o (fun o -> o#phrase) id in
            (o, DBInsert (tmp, into, labels, values, id), Types.unit_type)
      | DBUpdate (upd, p, from, where, set) ->
          let (o, from, _) = o#phrase from in
          let (o, p) = o#pattern p in
          let (o, upd) = optionu o (fun o -> o#temporal_update) upd in
          let (o, where, _) = option o (fun o -> o#phrase) where in
          let (o, set) =
            listu o
              (fun o (name, value) ->
                 let (o, value, _) = o#phrase value in (o, (name, value)))
              set
          in
            (o, DBUpdate (upd, p, from, where, set), Types.unit_type)
      | DBTemporalJoin (mode, body, Some t) ->
          let (o, body, _) =
              on_effects o (Types.make_empty_closed_row ()) (fun o -> o#phrase) body in
          let (o, body, _) = o#phrase body in
          let (o, t) = o#datatype t in
            (o, DBTemporalJoin (mode, body, Some t), t)
      | Xml (tag, attrs, attrexp, children) ->
          let (o, attrs) =
            listu o
              (fun o (name, value) ->
                 let (o, value, _) = list o (fun o -> o#phrase) value in
                   (o, (name, value)))
              attrs in
          let (o, attrexp, _) = option o (fun o -> o#phrase) attrexp in
          let (o, children, _) = list o (fun o -> o#phrase) children in
            (o, Xml (tag, attrs, attrexp, children), Types.xml_type)
      | TextNode s -> (o, TextNode s, Types.xml_type)
      | Formlet (body, yields) ->
         let envs = o#backup_envs in
         let (o, body, _) = o#phrase body in
         (* ensure that the formlet bindings are only in scope in the
            yields clause *)
         let o = o#with_var_env (TyEnv.extend (o#get_var_env ()) (o#get_formlet_env ())) in
         let (o, yields, t) = o#phrase yields in
         let o = o#restore_envs envs in
         (o, Formlet (body, yields), Instantiate.alias "Formlet" [(Type, t)] tycon_env)
      | Page e -> let (o, e, _) = o#phrase e in (o, Page e, Instantiate.alias "Page" [] tycon_env)
      | FormletPlacement (f, h, attributes) ->
          let (o, f, _) = o#phrase f in
          let (o, h, _) = o#phrase h in
          let (o, attributes, _) = o#phrase attributes in
            (o, FormletPlacement (f, h, attributes), Types.xml_type)
      | PagePlacement e ->
          let (o, e, _) = o#phrase e in (o, PagePlacement e, Types.xml_type)
      | FormBinding (f, p) ->
         let envs = o#backup_envs in
         let (o, f, _) = o#phrase f in
         (* HACK: add the formlet bindings to the formlet environment *)
         let o = o#with_var_env TyEnv.empty in
         let (o, p) = o#pattern p in
         let formlet_env = TyEnv.extend formlet_env (o#get_var_env()) in
         let o = o#restore_envs envs in
         let o = o#with_formlet_env formlet_env in
         (* let o = {< formlet_env=TyEnv.extend formlet_env (o#get_var_env()) >} in *)
         (o, FormBinding (f, p), Types.xml_type)
      | e -> raise (internal_error ("oops: "^show_phrasenode  e))

    method phrase : phrase -> ('self_type * phrase * Types.datatype) =
      fun {node; pos} ->
        let (o, node, t) = o#phrasenode node in
        (o, WithPos.make ~pos node, t)

    method patternnode : Pattern.t -> ('self_type * Pattern.t) =
      let open Pattern in
      function
      | Any -> (o, Any)
      | Nil -> (o, Nil)
      | Cons (p, ps) ->
          let (o, p) = o#pattern p in
          let (o, ps) = o#pattern ps in (o, Cons (p, ps))
      | List p ->
          let (o, p) = listu o (fun o -> o#pattern) p in (o, List p)
      | Variant (name, p) ->
          let (o, p) = optionu o (fun o -> o#pattern) p
          in (o, Variant (name, p))
      | Operation (name, ps, k, b) ->
         let (o, ps) = listu o (fun o -> o#pattern) ps in
         let (o, k)  = o#pattern k in
         (o, Operation (name, ps, k, b))
      | Negative name -> (o, Negative name)
      | Record (fields, rest) ->
          let (o, fields) =
            listu o
              (fun o (name, p) ->
                 let (o, p) = o#pattern p in (o, (name, p)))
              fields in
          let (o, rest) = optionu o (fun o -> o#pattern) rest
          in (o, Record (fields, rest))
      | Tuple ps ->
          let (o, ps) = listu o (fun o -> o#pattern) ps in (o, Tuple ps)
      | Constant c -> let (o, c, _) = o#constant c in (o, Constant c)
      | Variable x -> let (o, x) = o#binder x in (o, Variable x)
      | As (x, p) ->
          let (o, x) = o#binder x in
          let (o, p) = o#pattern p in (o, (As (x, p)))
      | HasType (p, t) ->
          let (o, p) = o#pattern p in (o, (HasType (p, t)))

    method pattern : Pattern.with_pos -> ('self_type * Pattern.with_pos) =
      WithPos.traverse_map
        ~o
        ~f_pos:(fun o v -> o, v)
        ~f_node:(fun o v -> o#patternnode v)

    method iterpatt : iterpatt -> ('self_type * iterpatt) =
      function
      | List (p, e) ->
          let (o, e, _) = o#phrase e in
          let (o, p) = o#pattern p in
          (o, List (p, e))
      | Sugartypes.Table (t, p, e) ->
          let (o, e, _) = o#phrase e in
          let (o, p) = o#pattern p in
          (o, Sugartypes.Table (t, p, e))

    method funlit : Types.row -> funlit -> ('self_type * funlit * Types.datatype) =
      fun inner_eff f ->
        match f with
          | NormalFunlit (pss, e) ->
            let envs = o#backup_envs in
            let (o, pss) = listu o (fun o -> listu o (fun o -> o#pattern)) pss in
            let o = o#with_effects inner_eff in
            let (o, e, t) = o#phrase e in
            let o = o#restore_envs envs in
            (o, NormalFunlit (pss, e), t)
          | SwitchFunlit (pss, body) ->
            let envs = o#backup_envs in
            let (o, pss) = listu o (fun o -> listu o (fun o -> o#pattern)) pss in
            let o = o#with_effects inner_eff in
            let (o, body) =
              listu o (fun o (p, c) ->
                let (o, p) = o#pattern p in
                let (o, c, _) = o#phrase c in
                (o, (p, c))) body in
            let o = o#restore_envs envs in
            (o, SwitchFunlit (pss, body), Types.unit_type)

    method constant : Constant.t -> ('self_type * Constant.t * Types.datatype) =
      function
        | Constant.Float v  -> (o, Constant.Float v , Types.float_type )
        | Constant.Int v    -> (o, Constant.Int v   , Types.int_type   )
        | Constant.String v -> (o, Constant.String v, Types.string_type)
        | Constant.Bool v   -> (o, Constant.Bool v  , Types.bool_type  )
        | Constant.Char v   -> (o, Constant.Char v  , Types.char_type  )
        | Constant.DateTime v -> (o, Constant.DateTime v  , Types.datetime_type  )

    method quantifiers : SugarQuantifier.t list -> ('self_type * SugarQuantifier.t list) =
      fun qs -> (o, qs)
    method backup_quantifiers : IntSet.t = IntSet.empty
    method restore_quantifiers : IntSet.t -> 'self_type = fun _ -> o

    method rec_bodies : recursive_function list -> ('self * recursive_function list) =
      let outer_tyvars = o#backup_quantifiers in
      let rec list o =
        function
          | [] -> (o, [])
          | {node={ rec_definition = ((tyvars, Some (inner, extras)), lam); _ } as fn; pos} :: defs ->
              let (o, tyvars) = o#quantifiers tyvars in
              let (o, inner) = o#datatype inner in
              let lam_in = Sugartypes.get_normal_funlit lam in
              let inner_effects = fun_effects inner (fst lam_in) in
              let (o, lam, _) = o#funlit inner_effects lam in
              let o = o#restore_quantifiers outer_tyvars in
              let (o, defs) = list o defs in
              (o, make ~pos { fn with rec_definition = ((tyvars, Some (inner, extras)), lam) } :: defs)
          | _ :: _ -> assert false
      in
        list o

    method rec_activate_outer_bindings : recursive_function list -> ('self * recursive_function list) =
      let rec list o =
        function
          | [] -> o, []
          | {node={ rec_binder; rec_signature;  _ } as fn; pos} :: defs ->
              let (o, rec_binder) = o#binder rec_binder in
              let (o, defs) = list o defs in
              let (o, rec_signature) = optionu o (fun o -> o#datatype') rec_signature in
              (o, make ~pos { fn with rec_binder; rec_signature } :: defs)
      in
        list o

    method rec_activate_inner_bindings : recursive_function list -> 'self_type =
      let rec list o =
        function
          | [] -> o
          | {node={ rec_binder = f; rec_definition = ((_tyvars, Some (inner, _extras)), _lam); _ }; _} :: defs ->
              let (o, _) = o#binder (Binder.set_type f inner) in
              list o defs
          | _ :: _ -> assert false
      in
        list o

    method bindingnode : bindingnode -> ('self_type * bindingnode) =
      function
      | Val (p, (tyvars, e), location, t) ->
         let outer_tyvars = o#backup_quantifiers in
         let (o, tyvars) = o#quantifiers tyvars in
         let (o, e, _) = o#phrase e in
         let o = o#restore_quantifiers outer_tyvars in
         let (o, p) = o#pattern p in
         let (o, t) = optionu o (fun o -> o#datatype') t in
         (o, Val (p, (tyvars, e), location, t))
      | Fun { fun_binder; fun_linearity; fun_definition = (tyvars, lam);
              fun_location; fun_signature; fun_frozen; fun_unsafe_signature }
           when Binder.has_type fun_binder ->
         let outer_tyvars = o#backup_quantifiers in
         let (o, tyvars) = o#quantifiers tyvars in
         let lam_in = Sugartypes.get_normal_funlit lam in
         let inner_effects = fun_effects (Binder.to_type fun_binder) (fst lam_in) in
         let (o, lam, _) = o#funlit inner_effects lam in
         let o = o#restore_quantifiers outer_tyvars in
         let (o, fun_binder) = o#binder fun_binder in
         let (o, fun_signature) = optionu o (fun o -> o#datatype') fun_signature in
         (o, Fun { fun_binder; fun_linearity; fun_definition = (tyvars, lam);
                   fun_location; fun_signature; fun_frozen; fun_unsafe_signature })
      | Fun _ -> raise (internal_error "Unannotated non-recursive function binding")
      | Funs defs ->
         (* put the inner bindings in the environment *)
         let o = o#rec_activate_inner_bindings defs in

         (* transform the function bodies *)
         let (o, defs) = o#rec_bodies defs in

         (* put the outer bindings in the environment *)
         let o, defs = o#rec_activate_outer_bindings defs in
         (o, (Funs defs))
      | Foreign alien ->
         let (o, declarations) =
           listu o
             (fun o (b, dt) ->
               let o, b = o#binder b in
               let o, dt = o#datatype' dt in
               (o, (b, dt)))
             (Alien.declarations alien)
         in
         let o, language = o#foreign_language (Alien.language alien) in
         (o, Foreign (Alien.modify ~language ~declarations alien))
      | Aliases ts ->
          let (o, _) = listu o (fun o {node=(name, vars, b); pos} ->
              match b with
                | Typename (x, (Some dt as dt')) ->
                   let o = o#bind_tycon name
                     (`Alias (pk_type, List.map (SugarQuantifier.get_resolved_exn) vars, dt)) in
                   (o, WithPos.make ~pos (name, vars, Typename (x, dt')))
                | Effectname (x, (Some r as r')) ->
                   let o = o#bind_tycon name
                     (`Alias (pk_row, List.map (SugarQuantifier.get_resolved_exn) vars, r)) in
                   (o, WithPos.make ~pos (name, vars, Effectname (x, r')))
                | _ -> raise (internal_error "Unannotated type alias")
            ) ts in
          (o, Aliases ts)
      | (Infix _) as node ->
         (o, node)
      | Exp e -> let (o, e, _) = o#phrase e in (o, Exp e)
      | AlienBlock _ -> assert false
      | Module _ -> assert false
      | Import _ -> assert false
      | Open _ -> assert false

    method binding : binding -> ('self_type * binding) =
      WithPos.traverse_map
        ~o
        ~f_pos:(fun o v -> o, v)
        ~f_node:(fun _ v -> o#bindingnode v)

    method binder : Binder.with_pos -> ('self_type * Binder.with_pos) =
      fun bndr ->
      assert (Binder.has_type bndr);
      let var_env = TyEnv.bind (Binder.to_name bndr) (Binder.to_type bndr) var_env in
      ({< var_env=var_env >}, bndr)

    method cp_phrase : cp_phrase -> ('self_type * cp_phrase * Types.datatype) =
      fun phrase ->
      let o, node, t = WithPos.node phrase |> o#cp_phrasenode in
      o, (WithPos.map ~f:(fun _ -> node) phrase), t

    (* TODO: should really invoke o#datatype on type annotations! *)
    method cp_phrasenode : cp_phrasenode -> ('self_type * cp_phrasenode * Types.datatype) = function
      | CPUnquote (bs, e) ->
         let envs = o#backup_envs in
         let (o, bs) = listu o (fun o -> o#binding) bs in
         let (o, e, t) = o#phrase e in
         let o = o#restore_envs envs in
         o, CPUnquote (bs, e), t
      | CPGrab (cbind, None, p) ->
         let (o, p, t) = o#cp_phrase p in
         o, CPGrab (cbind, None, p), t
      | CPGrab ((c, Some (Input (_a, s), _grab_tyargs) as cbind), Some b, p) -> (* FYI: a = u *)
         let envs = o#backup_envs in
         let (o, b) = o#binder b in
         let venv = TyEnv.bind c s (o#get_var_env ()) in
         let o = {< var_env = venv >} in
         let (o, p, t) = o#cp_phrase p in
         let o = o#restore_envs envs in
         o, CPGrab (cbind, Some b, p), t
      | CPGive ((c, Some (Output (_t, s), _tyargs) as cbind), e, p) ->
         let envs = o#backup_envs in
         let o = {< var_env = TyEnv.bind c s (o#get_var_env ()) >} in
         let (o, e, _typ) = option o (fun o -> o#phrase) e in
         let (o, p, t) = o#cp_phrase p in
         let o = o#restore_envs envs in
         o, CPGive (cbind, e, p), t
      | CPGiveNothing c ->
         let envs = o#backup_envs in
         let o, c = o#binder c in
         let o = o#restore_envs envs in
         o, CPGiveNothing c, Types.make_endbang_type
      | CPGrab _ -> raise (internal_error "Malformed grab in TransformSugar")
      | CPGive _ -> raise (internal_error "Malformed give in TransformSugar")
      | CPSelect (b, label, p) ->
         let envs = o#backup_envs in
         let o, b = o#binder b in
         let (o, p, t) = o#cp_phrase p in
         let o = o#restore_envs envs in
         o, CPSelect (b, label, p), t
      | CPOffer (b, cases) ->
         let (o, cases) = List.fold_right (fun (label, p) (o, cases) ->
                                           let envs = o#backup_envs in
                                           let o, _ = o#binder b in
                                           let (o, p, t) = o#cp_phrase p in
                                           (o#restore_envs envs, ((label, p), t) :: cases)) cases (o, []) in
         begin
           match List.split cases with
           | cases, t :: _ts ->
              o, CPOffer (b, cases), t
           | _ -> assert false
         end
      | CPLink (c, d) -> o, CPLink (c, d), Types.unit_type
      | CPComp (bndr, left, right) ->
         let c = Binder.to_name bndr in
         let s = Binder.to_type bndr in
         let envs = o#backup_envs in
         let (o, left, _typ) = {< var_env = TyEnv.bind c s (o#get_var_env ()) >}#cp_phrase left in
         let whiny_dual_type s = try Types.dual_type s with Invalid_argument _ -> raise (Invalid_argument ("Attempted to dualize non-session type " ^ Types.string_of_datatype s)) in
         let (o, right, t) = {< var_env = TyEnv.bind c (whiny_dual_type s) (o#get_var_env ()) >}#cp_phrase right in
         let o = o#restore_envs envs in
         o, CPComp (bndr, left, right), t

    method foreign_language : ForeignLanguage.t -> ('self_type * ForeignLanguage.t)
      = fun lang -> (o, lang)
  end
