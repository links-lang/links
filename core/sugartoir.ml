open CommonTypes
open Operators
open Utility
open SourceCode
open SourceCode.WithPos
open Ir
open Var

(* {0 Sugar To IR}

   This module implements a compiler from the syntactic sugar to the
   IR.

   It is based on the Compileir module which compiled the old IR to
   the new one.

   Both Compileir and this module use one-pass monadic transformations
   in order to perform administrative reductions on the fly. The basic
   idea is to use a non-standard evaluator which interprets source
   terms in a monadic semantics. The monadic semantics is chosen in
   such a way that we can reify semantic values as IR terms.

   We can view our implementation as an instance of normalisation by
   evaluation (where normalisation means reduction to monadic normal
   form).

   Unfortunately pattern-matching compilation screws up some of our
   abstractions. For instance, we have to pass in the environment to
   comp, letfun, letrec and switch; and we have to prematurely reify
   terms in comp and switch which is why we subsequently 'reflect'
   them. It would be nicer if we had a cleaner separation between the
   syntactic and the semantic worlds.
*)

(*
  TODO:

  - move `Section, `UnaryAppl, most of `InfixAppl, `RangeLit,
  `ListLit to frontend desugaring transformations
  - check that we're doing the right thing with tyvars
*)

(* It would be nice to use primitive functions to construct XML nodes
   and table handles. Then they could be moved to a frontend
   transformation.

   We should either use escape in the frontend and IR or call/cc in
   the frontend and IR. What we do at the moment (escape in the
   frontend and call/cc in the IR) is silly.
*)

let show_compiled_ir
  = Settings.(flag "show_compiled_ir"
              |> depends Debug.enabled
              |> synopsis "Dumps the IR to stderr"
              |> convert parse_bool
              |> sync)

type datatype = Types.datatype

module NEnv = Env.String
module TEnv = Env.Int

type nenv = var NEnv.t
type tenv = Types.datatype TEnv.t

type env = nenv * tenv * Types.row

let lookup_name_and_type name (nenv, tenv, _eff) =
  let var = NEnv.find name nenv in
    var, TEnv.find var tenv

let lookup_effects (_, _, eff) = eff
let with_effects (x, y, _) eff = (x, y, eff)

(* Hmm... shouldn't we need to use something like this? *)

(* let with_mailbox_type t (nenv, tenv) = *)
(*   let mb_var = NEnv.lookup nenv "_MAILBOX_" in *)
(*     (nenv, TEnv.bind tenv (mb_var, t)) *)

module type MONAD =
sig
  type 'a sem

  val lift : 'a -> 'a sem
  val bind : 'a sem -> ('a -> 'b sem) -> 'b sem
end

module type BINDINGMONAD =
sig
  include MONAD

  val reflect : (binding list * 'a) -> 'a sem

  val lift_binding : binding -> 'a -> 'a sem

  type 'a semt = ('a * datatype) sem
  val sem_type : 'a semt -> datatype
  val reify : tail_computation semt -> computation
end

module type INTERPRETATION =
sig
  type 'a sem

  val sem_type : 'a sem -> datatype
  val reify : tail_computation sem -> computation

  val value_of_comp : tail_computation sem -> value sem
  val comp_of_value : value sem -> tail_computation sem

  val constant : Constant.t -> value sem
  val var : (var * datatype) -> value sem

  val escape : (var_info * Types.row * (var -> tail_computation sem)) -> tail_computation sem

  val tabstr : (Quantifier.t list * value sem) -> value sem
  val tappl : (value sem * Types.type_arg list) -> value sem

  val apply : (value sem * (value sem) list) -> tail_computation sem
  val apply_pure : (value sem * (value sem) list) -> value sem
  val condition : (value sem * tail_computation sem * tail_computation sem) -> tail_computation sem

  val comp : env -> (CompilePatterns.Pattern.t * value sem * tail_computation sem) -> tail_computation sem

  val temporal_join : Temporality.t * tail_computation sem -> tail_computation sem

  val letvar : (var_info * tail_computation sem * tyvar list *
               (var -> tail_computation sem)) -> tail_computation sem

  val xml : value sem * string * (Name.t * (value sem) list) list * (value sem) list -> value sem
  val record : (Name.t * value sem) list * (value sem) option -> value sem

  val project : value sem * Name.t -> value sem
  val update : value sem * (Name.t * value sem) list -> value sem

  val coerce : value sem * datatype -> value sem

  val query : (value sem * value sem) option * QueryPolicy.t * tail_computation sem -> tail_computation sem

  val db_insert : env -> (temporal_insertion option * value sem * value sem) -> tail_computation sem
  val db_insert_returning : env -> (temporal_insertion option * value sem * value sem * value sem) -> tail_computation sem
  val db_update : env ->
    ([   `TransactionUpdate | `ValidCurrentUpdate
       | `ValidSequencedUpdate of (value sem * value sem)
       | `ValidNonsequencedUpdate of (tail_computation sem option * tail_computation sem option)] option *
     CompilePatterns.Pattern.t *
     value sem *
     tail_computation sem option *
     tail_computation sem) -> tail_computation sem

  val db_delete : env -> (
    [   `TransactionDelete | `ValidCurrentDelete
      | `ValidSequencedDelete of (value sem * value sem) | `ValidNonsequencedDelete ] option *
    CompilePatterns.Pattern.t * value sem * tail_computation sem option) -> tail_computation sem

  val do_operation : Name.t * (value sem) list * Types.datatype -> tail_computation sem

  val handle : env -> (tail_computation sem *
                         (CompilePatterns.Pattern.t * (env -> tail_computation sem)) list *
                         (CompilePatterns.Pattern.t * (env -> tail_computation sem)) list *
                         ((env -> tail_computation sem) * CompilePatterns.Pattern.t * Types.datatype) list *
                          Sugartypes.handler_descriptor)
               -> tail_computation sem

  val switch : env -> (value sem * (CompilePatterns.Pattern.t * (env -> tail_computation sem)) list * Types.datatype) -> tail_computation sem

  val inject : Name.t * value sem * datatype -> value sem
  (* val case : *)
  (*   value sem * string * (var_info * (var -> tail_computation sem)) * *)
  (*   (var_info * (var -> tail_computation sem)) option -> *)
  (*   tail_computation sem *)
  val case_zero : value sem * datatype -> tail_computation sem

  val concat : (value sem * value sem * (value sem) list) -> value sem

  val database : value sem -> tail_computation sem

  val table_handle : value sem * value sem * value sem * (Temporality.t * datatype * datatype * datatype) * (string * string) option -> tail_computation sem

  val lens_handle : value sem * Lens.Type.t -> tail_computation sem

  val lens_serial : value sem * Lens.Alias.Set.t * Lens.Type.t -> tail_computation sem

  val lens_drop_handle : value sem * string * string * value sem * Lens.Type.t -> tail_computation sem

  val lens_select_handle : value sem * [`Static of Lens.Phrase.t | `Dynamic of value sem] * Lens.Type.t -> tail_computation sem

  val lens_join_handle : value sem * value sem * string list * Lens.Phrase.t * Lens.Phrase.t * Lens.Type.t -> tail_computation sem

  val lens_check : value sem * Lens.Type.t -> tail_computation sem

  val lens_get : value sem * datatype -> tail_computation sem

  val lens_put : value sem * value sem * datatype -> tail_computation sem

  val wrong : datatype -> tail_computation sem

  val letfun :
    (var_info * (Quantifier.t list * (env * CompilePatterns.Pattern.t list * tail_computation sem)) * location * bool) ->
    (var -> tail_computation sem) ->
    tail_computation sem

  val letrec :
    (var_info * (Quantifier.t list * (env * CompilePatterns.Pattern.t list * (var list -> tail_computation sem))) * location * bool) list ->
    (var list -> tail_computation sem) ->
    tail_computation sem

  val alien : var_info * string * ForeignLanguage.t * (var -> tail_computation sem) -> tail_computation sem

  val select : Name.t * value sem -> tail_computation sem

  val offer : env -> (value sem * (CompilePatterns.Pattern.t * (env -> tail_computation sem)) list * Types.datatype) -> tail_computation sem
end

module BindingListMonad : BINDINGMONAD =
struct
  type 'a sem = binding list * 'a

  let lift a = ([], a)
  let bind (bs, a) f =
    let (bs', a') = f a in
      (bs @ bs', a')

  let reflect (bs, v) = (bs, v)

  let lift_binding b a =
    ([b], a)

  type 'a semt = ('a * datatype) sem
  let sem_type (_, (_, t)) = t
  let reify (bs, (e, _)) = (bs, e)
end

module BindingContinuationMonad : BINDINGMONAD =
struct
  type 'a sem = ('a -> computation * datatype) -> computation * datatype

  let lift a = fun k -> k a
  let bind c f =
    (fun k ->
       c (fun a -> f a k))

  let reflect (bs, v) =
  fun k ->
    let (bs', tc), t = k v in
      (bs @ bs', tc), t

  let lift_binding b a =
    (fun k ->
       let (bs, a'), t = k a
       in
         (b :: bs, a'), t)

  type 'a semt = ('a * datatype) sem
  let reify c =
    let (e, _) =
      c (fun (a, t) -> ([], a), t)
    in
      e

  let dummy_computation = Special (Wrong Types.Not_typed)
  let sem_type s =
    let (_, t) =
      s (fun (_, t) -> ([], dummy_computation), t)
    in
      t
end

module Interpretation(M : BINDINGMONAD) : INTERPRETATION =
struct
  type 'a sem = 'a M.semt

  let lift = M.lift
  let bind s f = M.bind s (fun (a, _) -> f a)

  let lift_binding = M.lift_binding

  let sem_type = M.sem_type
  let reify = M.reify
  let reflect = M.reflect

  module S :
  sig
    val lift_list : ('a sem) list -> ('a list) M.sem

    val lift_alist : ('a*'b sem) list -> (('a*'b) list) M.sem

    val comp_binding : ?tyvars:tyvar list -> var_info * tail_computation -> var M.sem

    val fun_binding :
      Var.var_info * (tyvar list * binder list * computation) * location * bool ->
      Var.var M.sem
    val rec_binding :
      (Var.var_info * (tyvar list * binder list * (Var.var list -> computation))
       * location * bool) list ->
      (Var.var list) M.sem

    val alien_binding : var_info * string * ForeignLanguage.t -> var M.sem

    val value_of_untyped_var : var M.sem * datatype -> value sem
  end =
  struct
    let lift_list ss =
      List.fold_right
        (fun s s' ->
           bind s
             (fun v ->
                M.bind s'
                  (fun vs -> lift (v :: vs))))
        ss (lift [])

    let lift_alist ss =
      List.fold_right
        (fun (name, s) ss ->
           bind s
             (fun v ->
                M.bind ss
                  (fun vs -> lift ((name, v) :: vs))))
        ss (lift [])

    let comp_binding ?(tyvars=[]) (x_info , e) =
      let xb, x = Var.fresh_var x_info in
        lift_binding (letm ~tyvars (xb, e)) x

    let fun_binding (f_info, (fn_tyvars, xsb, fn_body), fn_location, fn_unsafe) =
      let fb, f = Var.fresh_var f_info in
        let fundef = {fn_binder = fb; fn_tyvars; fn_params = xsb; fn_body; fn_closure = None;
                       fn_location; fn_unsafe}
        in
        lift_binding (Fun fundef) f

    let rec_binding defs =
      let defs, fs =
        List.fold_right
          (fun (f_info, (tyvars, xsb, body), location, unsafe) (defs, fs) ->
             let fb, f = Var.fresh_var f_info in
               ((fb, (tyvars, xsb, body), None, location, unsafe) :: defs, f :: fs))
          defs ([], [])
      in
        lift_binding
          (Rec
             (List.map
                (fun (fb, (fn_tyvars, xsb, body), none, fn_location, fn_unsafe) ->
                  assert (none = None);
                  let fundef = {fn_binder = fb; fn_tyvars; fn_params = xsb;
                                fn_body = body fs; fn_closure = none; fn_location; fn_unsafe}
                  in
                  fundef)
                defs))
          fs

    let alien_binding (x_info, object_name, language) =
      let xb, x = Var.fresh_var x_info in
      lift_binding (Alien { binder = xb; object_name; language }) x

    let value_of_untyped_var (s, t) =
      M.bind s (fun x -> lift (Variable x, t))
  end
  open S

  let value_of_comp s =
    bind s
      (function
         | Return v -> lift (v, sem_type s)
         | e ->
             let t = sem_type s in
               value_of_untyped_var (comp_binding (Var.info_of_type t, e), t))

  let comp_of_value s =
    bind s (fun v -> lift (Return v, sem_type s))

  (* eval parameters *)
  let constant c = lift (Constant c, Types.Primitive (Constant.type_of c))
  let var (x, t) = lift (Variable x, t)

  let apply (s, ss) =
    let ss = lift_list ss in
    let t = TypeUtils.return_type (sem_type s) in
      bind s
        (fun v ->
           M.bind ss
             (fun vs -> lift (Apply (v, vs), t)))

  let apply_pure (s, ss) =
    let ss = lift_list ss in
    let t = TypeUtils.return_type (sem_type s) in
      bind s
        (fun v ->
           M.bind ss
             (fun vs -> lift (ApplyPure (v, vs), t)))

  let condition (s, s1, s2) =
    bind s (fun v -> lift (If (v, reify s1, reify s2), sem_type s1))

  let concat (nil, append, ss) =
    match ss with
      | [] -> nil
      | [s] -> s
      | s::ss ->
          List.fold_left (fun s s' -> apply_pure (append, [s; s'])) s ss

  let string_concat (string_append, ss) =
    match ss with
      | [] -> lift (Constant (Constant.String ""), Types.string_type)
      | [s] -> s
      | s::ss ->
          List.fold_left (fun s s' -> apply_pure (string_append, [s; s'])) s ss

  let xml (string_append, name, attrs, children) =
    let lift_attrs attrs =
      List.fold_right
        (fun (name, ss) attrs ->
           bind (string_concat (string_append, ss))
             (fun v ->
                M.bind attrs
                  (fun bs -> lift ((name, v) :: bs))))
        attrs (lift []) in
    let attrs = lift_attrs attrs in
    let children = lift_list children in
      M.bind attrs
        (fun attrs ->
           M.bind children
             (fun children ->
                let attrs = StringMap.from_alist attrs in
                  lift (XmlNode (name, attrs, children), Types.xml_type)))

  let record (fields, r) =
    let field_types =
      List.fold_left
        (fun field_types (name, s) -> StringMap.add name (sem_type s) field_types)
        StringMap.empty
        fields in
    let s' = lift_alist fields in
      match r with
        | None ->
            let t = Types.make_record_type field_types in
              M.bind s'
                (fun fields ->
                   lift (Extend (StringMap.from_alist fields, None), t))
        | Some s ->
            let t = Types.Record (Types.extend_row field_types (TypeUtils.extract_row (sem_type s))) in
              bind s
                (fun r ->
                   M.bind s'
                     (fun fields -> lift (Extend (StringMap.from_alist fields, Some r), t)))

  let project (s, name) =
    let t = TypeUtils.project_type name (sem_type s) in
      bind s (fun v -> lift (Project (name, v), t))

  let erase (s, names) =
    let t = TypeUtils.erase_type names (sem_type s) in
      bind s (fun v -> lift (Erase (names, v), t))

  let coerce (s, t) =
    bind s (fun v -> lift (Coerce (v, t), sem_type s))

  (*
      (r : (l1:A1, ... li:Ai | R) with (l1=v1, ..., li=vi))
    -->
      (l1=v1, ..., li=vi | r ((-l1, ..., -li | R) <-- (l1:A1, ... li:Ai | R)))
  *)
  let update (s, fields) =
    let names =
      List.fold_left
        (fun names (name, _) ->
           StringSet.add name names)
        StringSet.empty
        fields in
    record (fields, Some (erase (s, names)))

  let inject (name, s, t) =
      bind s (fun v -> lift (Inject (name, v, t), t))

  (* this isn't used... *)
  (* let case (s, name, (cinfo, cbody), default) = *)
  (*   bind s (fun v -> *)
  (*             let cb, c = Var.fresh_var cinfo in *)
  (*             let cbody' = cbody c in *)
  (*             let t = sem_type cbody' in *)
  (*             let default = *)
  (*               opt_map (fun (dinfo, dbody) -> *)
  (*                          let db, d = Var.fresh_var dinfo in *)
  (*                            (db, reify (dbody d))) default *)
  (*             in *)
  (*               lift *)
  (*                 (`Case (v, *)
  (*                         StringMap.add name (cb, reify cbody') StringMap.empty, *)
  (*                         default), t)) *)

  let case_zero (s, t) =
    bind s (fun v ->
              lift (Case (v, StringMap.empty, None), t))

  let database s =
    bind s (fun v -> lift (Special (Database v), Types.Primitive Primitive.DB))

  let table_handle (database, table, keys, (tmp, r, w, n), temporal_fields) =
    bind database
      (fun database ->
         bind table
           (fun table ->
         bind keys
        (fun keys ->
            let tbl = {
                database; table; keys;
                table_type = (tmp, r, w, n);
                temporal_fields
            } in
            lift (Special (Table tbl),
                               Types.Table (tmp, r, w, n)))))

  let lens_handle (table, t) =
      bind table
        (fun table ->
            lift (Special (Lens (table, t)), Types.Lens t))

  let lens_serial (lens, columns, typ) =
    bind lens
      (fun lens ->
         lift (Special (LensSerial {lens; columns; typ}), Types.Lens typ))

  let lens_drop_handle (lens, drop, key, default, typ) =
      bind lens
        (fun lens ->
            bind default
            (fun default ->
               lift (Special (LensDrop {lens; drop; key; default; typ}), Types.Lens typ)))

  let lens_select_handle (lens, pred, typ) =
      bind lens
        (fun lens ->
           match pred with
           | `Dynamic pred ->
             bind pred
               (fun predicate ->
                  let predicate = Dynamic predicate in
                  lift (Special (LensSelect {lens; predicate; typ}), Types.Lens typ))
           | `Static predicate ->
             let predicate = Static predicate in
             lift (Special (LensSelect {lens; predicate; typ}), Types.Lens typ))

  let lens_join_handle (left, right, on, del_left, del_right, typ) =
      bind left
        (fun left ->
          bind right
          (fun right ->
            lift (Special (LensJoin {left; right; on; del_left; del_right; typ}), Types.Lens typ)))

  let lens_check (lens, t) =
      bind lens
         (fun lens ->
            lift (Special (LensCheck (lens, t)), Types.Lens t))

  let lens_get (lens, rtype) =
      bind lens
        (fun lens ->
            lift (Special (LensGet (lens, rtype)), Types.make_list_type rtype))

  let lens_put (lens, data, rtype) =
      bind lens
        (fun lens ->
            bind data
                (fun data ->
                        lift (Special (LensPut (lens, data, rtype)), Types.make_list_type rtype)))

  let wrong t = lift (Special (Wrong t), t)

  let alien (x_info, object_name, language, rest) =
    M.bind (alien_binding (x_info, object_name, language)) rest

  let select (l, e) =
    let t = TypeUtils.select_type l (sem_type e) in
      bind e (fun v -> lift (Special (Select (l, v)), t))

  let offer env (v, cases, t) =
    let cases =
      List.map
        (fun (p, body) -> ([p], fun env -> reify (body env))) cases
    in
      bind v
        (fun e ->
           M.bind
             (comp_binding (Var.info_of_type (sem_type v), Return e))
             (fun var ->
                let nenv, tenv, eff = env in
                let tenv = TEnv.bind var (sem_type v) tenv in
                let (bs, tc) = CompilePatterns.compile_choices (nenv, tenv, eff) (t, var, cases) in
                  reflect (bs, (tc, t))))

  let db_insert _env (tmp, source, rows) =
    bind source
      (fun source ->
        bind rows
          (fun rows ->
            lift (Special (InsertRows (tmp, source, rows)), Types.unit_type)))

  let db_insert_returning _env (tmp, source, rows, returning) =
    bind source
      (fun source ->
        bind rows
          (fun rows ->
            bind returning
              (fun returning ->
                lift (Special (InsertReturning (tmp, source, rows, returning)), Types.int_type))))

  let db_update env (upd, p, source, where, body) =
    let source_type = sem_type source in
    let xt = TypeUtils.table_read_type source_type in
    let xb, x = Var.fresh_var_of_type xt in
    let wrap = CompilePatterns.let_pattern env p (Variable x, xt) in
      bind source
        (fun source ->
          let body_type = sem_type body in
          let body = wrap (reify body, body_type) in
          let where = OptionUtils.opt_map
            (fun where -> wrap (reify where, Types.bool_type)) where in
          let lift_special upd =
            lift (Special (Update (upd, (xb, source), where, body)), Types.unit_type) in

          match upd with
            | None -> lift_special None
            | Some `TransactionUpdate ->
                lift_special (Some Ir.TransactionTimeUpdate)
            | Some `ValidCurrentUpdate ->
                lift_special (Some (Ir.ValidTimeUpdate (Ir.CurrentUpdate)))
            | Some (`ValidNonsequencedUpdate (valid_from, valid_to)) ->
                let valid_from = OptionUtils.opt_map
                  (fun valid_from -> wrap (reify valid_from, Types.datetime_type)) valid_from in
                let valid_to = OptionUtils.opt_map
                  (fun valid_to -> wrap (reify valid_to, Types.datetime_type)) valid_to in
                lift_special
                  (Some (Ir.(ValidTimeUpdate (Ir.NonsequencedUpdate {
                      from_time = valid_from; to_time = valid_to }))))
            | Some (`ValidSequencedUpdate (validity_from, validity_to)) ->
                bind validity_from
                  (fun validity_from ->
                    bind validity_to
                      (fun validity_to ->
                         lift_special
                           (Some (Ir.(ValidTimeUpdate (SequencedUpdate {
                               validity_from; validity_to })))))))

  let db_delete env (del, p, source, where) =
    let source_type = sem_type source in
    let xt = TypeUtils.table_read_type source_type in
    let xb, x = Var.fresh_var_of_type xt in
    let wrap tcomp ty = CompilePatterns.let_pattern env p (Variable x, xt) (reify tcomp, ty) in
    bind source
      (fun source ->
        let lift_special del =
          match where with
            | None ->
                lift (Special (Delete (del, (xb, source), None)), Types.unit_type)
            | Some where ->
                let where = wrap where Types.bool_type in
                    lift (Special (Delete (del, (xb, source), Some where)), Types.unit_type) in
        match del with
          | None -> lift_special None
          | Some `TransactionDelete -> lift_special (Some Ir.TransactionTimeDeletion)
          | Some `ValidCurrentDelete -> lift_special (Some Ir.(ValidTimeDeletion CurrentDeletion))
          | Some (`ValidSequencedDelete (validity_from, validity_to)) ->
              bind validity_from
                (fun validity_from ->
                  bind validity_to
                    (fun validity_to ->
                       lift_special (Some (Ir.(ValidTimeDeletion (SequencedDeletion {
                           validity_from; validity_to }))))))
          | Some `ValidNonsequencedDelete ->
              lift_special (Some (Ir.(ValidTimeDeletion NonsequencedDeletion))))

  let query (range, policy, s) =
    let bs, e = reify s in
      match range with
        | None ->
            lift (Special (Query (None, policy, (bs, e), sem_type s)), sem_type s)
        | Some (limit, offset) ->
            bind limit
              (fun limit ->
                 bind offset
                   (fun offset ->
                      lift (Special (Query (Some (limit, offset), policy, (bs, e), sem_type s)), sem_type s)))

  let letvar (x_info, s, tyvars, body) =
    bind s
      (fun e ->
         M.bind (comp_binding ~tyvars (x_info, e))
           (fun x -> body x))

  let temporal_join (mode, comp) =
    let bs, e = reify comp in
    lift (Special (TemporalJoin (mode, (bs, e), sem_type comp)), sem_type comp)

  let comp env (p, s, body) =
    let vt = sem_type s in
      bind s
        (fun v ->
           let body_type = sem_type body in
           let (bs, tc) = CompilePatterns.let_pattern env p (v, vt) (reify body, body_type) in
             reflect (bs, (tc, body_type)))

  let escape (k_info, eff, body) =
    let kt = Var.info_type k_info in
    let kb, k = Var.fresh_var k_info in
    let body = body k in
    let body_type = sem_type body in
    let body = reify body in
    let ft = Types.Function (Types.make_tuple_type [kt], eff, body_type) in
    let f_info = Var.make_local_info (ft, "") in
    let rest f : tail_computation sem = lift (Special (CallCC (Variable f)),
                                              body_type) in
      M.bind (fun_binding (f_info, ([], [kb], body), loc_unknown, false)) rest

  let letfun (f_info, (tyvars, (body_env, ps, body)), location, unsafe) rest =
    let ft = Var.info_type f_info in
    let xsb : binder list =
      (* It is important to rename the quantifiers in the type to be
         those used in the body of the function. *)
      match Instantiate.replace_quantifiers ft tyvars with
        | Types.ForAll (_, t')
        | t' ->
            begin match TypeUtils.concrete_type t' with
              | Types.Function _ | Types.Lolli _ as ft' ->
                  let args = TypeUtils.arg_types ft' in
                    List.map (fun arg -> Var.fresh_binder_of_type arg) args
              | _ -> assert false
            end in

    let body_type = sem_type body in
    let body =
      List.fold_left2
        (fun body p (xb : binder) ->
           let x  = Var.var_of_binder  xb in
           let xt = Var.type_of_binder xb in
             CompilePatterns.let_pattern body_env p (Variable x, xt) (body, body_type))
        (reify body)
        ps
        xsb
    in
      M.bind (fun_binding (f_info, (tyvars, xsb, body), location, unsafe)) rest

  let letrec defs rest =
    let defs =
      List.map
        (fun (f_info, (tyvars, (body_env, ps, body)), location, unsafe) ->
           let ft = Var.info_type f_info in
           let xsb : binder list =
             (* It is important to rename the quantifiers in the type to be those used in
                the body of the function. *)
             match Instantiate.replace_quantifiers ft tyvars with
               | Types.ForAll (_, t')
               | t' ->
                   begin match TypeUtils.concrete_type t' with
                     | Types.Function _ as ft' ->
                         let args = TypeUtils.arg_types ft' in
                           List.map (Var.fresh_binder_of_type) args
                     | _ -> assert false
                   end in
           let body fs =
             let body = body fs in
             let body_type = sem_type body in
               List.fold_left2
                 (fun body p xb ->
                    let x  = Var.var_of_binder  xb in
                    let xt = Var.type_of_binder xb in
                      CompilePatterns.let_pattern body_env p (Variable x, xt) (body, body_type))
                 (reify body)
                 ps
                 xsb
           in
             (f_info, (tyvars, xsb, body), location, unsafe))
        defs
    in
      M.bind (rec_binding defs) rest

  let do_operation (name, vs, t) =
    let vs = lift_list vs in
    M.bind vs (fun vs -> lift (Special (DoOperation (name, vs, t)), t))

  let handle env (m, val_cases, eff_cases, params, desc) =
    let params =
      List.map
        (fun (body, p, t) -> p, reify (body env), t) params
    in
    let val_cases, eff_cases =
      let reify cases =
        List.map
          (fun (p, body) -> ([p], fun env -> reify (body env))) cases
      in
      reify val_cases, reify eff_cases
    in
    let comp = reify m in
    let (bs, tc) = CompilePatterns.compile_handle_cases env (val_cases, eff_cases, params, desc) comp in
    let (_,_,_,t) = desc.Sugartypes.shd_types in
    reflect (bs, (tc, t))

  let switch env (v, cases, t) =
    let cases =
      List.map
        (fun (p, body) -> ([p], fun env -> reify (body env))) cases
    in
      bind v
        (fun e ->
           M.bind
             (comp_binding (Var.info_of_type (sem_type v), Return e))
             (fun var ->
                let nenv, tenv, eff = env in
                let tenv = TEnv.bind var (sem_type v) tenv in
                let (bs, tc) = CompilePatterns.compile_cases (nenv, tenv, eff) (t, var, cases) in
                  reflect (bs, (tc, t))))

  let tabstr (tyvars, s) =
    let t = Types.for_all (tyvars, sem_type s) in
      bind s (fun v -> lift (TAbs (tyvars, v), t))

  let tappl (s, tyargs) =
    let t = Instantiate.apply_type (sem_type s) tyargs in
      bind s (fun v -> lift (TApp (v, tyargs), t))
end


module Eval(I : INTERPRETATION) =
struct
  open PrimaryKind

  let extend xs vs (nenv, tenv, eff) =
    List.fold_left2
      (fun (nenv, tenv, eff) x (v, t) ->
         (NEnv.bind x v nenv, TEnv.bind v t tenv, eff))
      (nenv, tenv, eff)
      xs
      vs

  let (++) (nenv, tenv, _) (nenv', tenv', eff') = (NEnv.extend nenv nenv', TEnv.extend tenv tenv', eff')

  let rec eval : env -> Sugartypes.phrase -> tail_computation I.sem =
    fun env {node=e; pos} ->
      let lookup_var name =
        let x, xt = lookup_name_and_type name env in
          I.var (x, xt) in
      let instantiate name tyargs =
        let x, xt = lookup_name_and_type name env in
          match tyargs with
            | [] -> I.var (x, xt)
            | _ ->
                try
                  I.tappl (I.var (x, xt), tyargs)
                with
                    Instantiate.ArityMismatch (expected, provided) ->
                      raise (Errors.TypeApplicationArityMismatch { pos; name; expected; provided }) in

      let rec is_pure_primitive e =
        let open Sugartypes in
        match WithPos.node e with
          | TAbstr (_, e)
          | TAppl (e, _) -> is_pure_primitive e
          | Var f when Lib.is_pure_primitive f -> true
          | _ -> false in

      let eff = lookup_effects env in

      let instantiate_mb name = instantiate name [(Row, eff)] in
      let cofv = I.comp_of_value in
      let ec = eval env in
      let ev = evalv env in
      let evs = List.map ev in
      let ir_temporal_insert tmp =
        match tmp with
          | None -> None
          | Some (Sugartypes.ValidTimeInsertion Sugartypes.CurrentInsertion) ->
              Some (Ir.ValidTimeInsertion Ir.CurrentInsertion)
          | Some (Sugartypes.ValidTimeInsertion Sugartypes.SequencedInsertion) ->
              Some (Ir.ValidTimeInsertion Ir.SequencedInsertion)
          | Some (Sugartypes.TransactionTimeInsertion) ->
              Some Ir.TransactionTimeInsertion
      in
        let open Sugartypes in
        match e with
          | Constant c -> cofv (I.constant c)
          | Var x -> cofv (lookup_var x)
          | FreezeVar x -> cofv (lookup_var x)
          | RangeLit (low, high) ->
              I.apply (instantiate_mb "intRange", [ev low; ev high])
          | ListLit ([], Some t) ->
              cofv (instantiate "Nil" [(Type, t)])
          | ListLit (e::es, Some t) ->
              cofv (I.apply_pure(instantiate "Cons" [(Type, t); (Row, eff)],
                                 [ev e; ev (WithPos.make ~pos (ListLit (es, Some t)))]))
          | Escape (bndr, body) when Binder.has_type bndr ->
             let k  = Binder.to_name bndr in
             let kt = Binder.to_type bndr in
             I.escape (Var.make_local_info (kt, k), eff, fun v -> eval (extend [k] [(v, kt)] env) body)
          | Section Section.Minus | FreezeSection Section.Minus -> cofv (lookup_var "-")
          | Section Section.FloatMinus | FreezeSection Section.FloatMinus -> cofv (lookup_var "-.")
          | Section (Section.Name name) | FreezeSection (Section.Name name) -> cofv (lookup_var name)
          | Conditional (p, e1, e2) ->
              I.condition (ev p, ec e1, ec e2)
          | InfixAppl ((tyargs, BinaryOp.Name ((">" | ">=" | "==" | "<" | "<=" | "<>") as op)), e1, e2) ->
              cofv (I.apply_pure (instantiate op tyargs, [ev e1; ev e2]))
          | InfixAppl ((tyargs, BinaryOp.Name "++"), e1, e2) ->
              cofv (I.apply_pure (instantiate "Concat" tyargs, [ev e1; ev e2]))
          | InfixAppl ((tyargs, BinaryOp.Name "!"), e1, e2) ->
              I.apply (instantiate "Send" tyargs, [ev e1; ev e2])
          | InfixAppl ((tyargs, BinaryOp.Name n), e1, e2) when Lib.is_pure_primitive n ->
              cofv (I.apply_pure (instantiate n tyargs, [ev e1; ev e2]))
          | InfixAppl ((tyargs, BinaryOp.Name n), e1, e2) ->
              I.apply (instantiate n tyargs, [ev e1; ev e2])
          | InfixAppl ((tyargs, BinaryOp.Cons), e1, e2) ->
              cofv (I.apply_pure (instantiate "Cons" tyargs, [ev e1; ev e2]))
          | InfixAppl ((tyargs, BinaryOp.FloatMinus), e1, e2) ->
              cofv (I.apply_pure (instantiate "-." tyargs, [ev e1; ev e2]))
          | InfixAppl ((tyargs, BinaryOp.Minus), e1, e2) ->
              cofv (I.apply_pure (instantiate "-" tyargs, [ev e1; ev e2]))
          | InfixAppl ((_tyargs, BinaryOp.And), e1, e2) ->
              (* IMPORTANT: we compile boolean expressions to
                 conditionals in order to faithfully capture
                 short-circuit evaluation *)
              I.condition (ev e1, ec e2, cofv (I.constant (Constant.Bool false)))
          | InfixAppl ((_tyargs, BinaryOp.Or), e1, e2) ->
              I.condition (ev e1, cofv (I.constant (Constant.Bool true)), ec e2)
          | UnaryAppl ((_tyargs, UnaryOp.Minus), e) ->
              cofv (I.apply_pure(instantiate_mb "negate", [ev e]))
          | UnaryAppl ((_tyargs, UnaryOp.FloatMinus), e) ->
              cofv (I.apply_pure(instantiate_mb "negatef", [ev e]))
          | UnaryAppl ((tyargs, UnaryOp.Name n), e) when Lib.is_pure_primitive n ->
              cofv (I.apply_pure(instantiate n tyargs, [ev e]))
          | UnaryAppl ((tyargs, UnaryOp.Name n), e) ->
              I.apply (instantiate n tyargs, [ev e])
          | FnAppl ({node=Var f; _}, es) when Lib.is_pure_primitive f ->
              cofv (I.apply_pure (I.var (lookup_name_and_type f env), evs es))
          | FnAppl ({node=TAppl ({node=Var f; _}, tyargs); _}, es)
               when Lib.is_pure_primitive f ->
              cofv (I.apply_pure (instantiate f (List.map (snd ->- val_of) tyargs), evs es))
          | FnAppl (e, es) when is_pure_primitive e ->
              cofv (I.apply_pure (ev e, evs es))
          | FnAppl (e, es) ->
              I.apply (ev e, evs es)
          | TAbstr (tyvars, e) ->
              let v = ev e in
              let qs = List.map SugarQuantifier.get_resolved_exn tyvars in
                cofv (I.tabstr (qs, v))
          | TAppl (e, tyargs) ->
              let v = ev e in
              let vt = I.sem_type v in
                begin
                  try
                    cofv (I.tappl (v, List.map (snd ->- val_of) tyargs))
                  with
                      Instantiate.ArityMismatch (expected, provided) ->
                        raise (Errors.TypeApplicationArityMismatch { pos;
                          name=(Types.string_of_datatype vt); expected; provided })
                end
          | TupleLit [e] ->
              (* It isn't entirely clear whether there should be any 1-tuples at this stage,
                 but if there are we should get rid of them.

                 The parser certainly doesn't disallow them.
              *)
              ec e
          | TupleLit es ->
              let fields = mapIndex (fun e i -> (string_of_int (i+1), ev e)) es in
                cofv (I.record (fields, None))
          | RecordLit (fields, rest) ->
              cofv
                (I.record
                   (List.map (fun (name, e) -> (name, ev e)) fields,
                    opt_map ev rest))
          | Projection (e, name) ->
              cofv (I.project (ev e, name))
          | With (e, fields) ->
              cofv (I.update
                      (ev e,
                       List.map (fun (name, e) -> (name, ev e)) fields))
          | TypeAnnotation (e, _) ->
              (* we might consider getting rid of type annotations before here *)
              ec e
          | Upcast (e, (_, Some t), _) ->
              cofv (I.coerce (ev e, t))
          | ConstructorLit (name, None, Some t) ->
              cofv (I.inject (name, I.record ([], None), t))
          | ConstructorLit (name, Some e, Some t) ->
              cofv (I.inject (name, ev e, t))
          | DoOperation (op, ps, Some t, _) ->
            let name =
              let o = (object (o)
                inherit SugarTraversals.fold as super
                val mutable opname = None

                method opname = match opname with
                  | Some name -> name
                  | None -> failwith "Operation with no name"

                method! phrasenode = function
                  | Operation name -> opname <- Some name ; o
                  | p -> super#phrasenode p
              end)#phrase op in
              o#opname
            in
            let vs = evs ps in
            I.do_operation (name, vs, t)
          | Operation _ -> assert false
          (* FIXME: I don't know what's this. I suppose it is related to semantics,
             but Unlet and Linlet do not influence semantics. *)
          | Unlet _ -> assert false
          | Linlet _ -> assert false
          | Handle { sh_expr; sh_effect_cases; sh_value_cases; sh_descr } ->
             (* it happens that the ambient effects are the right ones
                for all of the patterns here (they match those of the
                initial computations for parameterised handlers and
                the bodies of the cases) *)
             let eff = lookup_effects env in
             let henv, params =
               let empty_env = (NEnv.empty, TEnv.empty, eff) in
                match (sh_descr.shd_params) with
                | None -> empty_env, []
                | Some { shp_bindings = bindings; shp_types = types } ->
                   let env, bindings =
                     List.fold_right2
                       (fun (p, body) t (env, bindings) ->
                         let p, penv = CompilePatterns.desugar_pattern eff p in
                         let bindings = ((fun env -> eval env body), p, t) :: bindings in
                         ((env ++ penv), bindings))
                       bindings types (empty_env, [])
                   in
                   env, List.rev bindings
             in
             let eff_cases =
               List.map
                 (fun (p, body) ->
                   let p, penv = CompilePatterns.desugar_pattern eff p in
                   (p, fun env -> eval ((env ++ henv) ++ penv) body))
                 sh_effect_cases
             in
             let val_cases =
                List.map
                  (fun (p, body) ->
                    let p, penv = CompilePatterns.desugar_pattern eff p in
                    (p, fun env -> eval ((env ++ henv) ++ penv) body))
                  sh_value_cases
             in
             I.handle env (ec sh_expr, val_cases, eff_cases, params, sh_descr)
          | Switch (e, cases, Some t) ->
              let cases =
                List.map
                  (fun (p, body) ->
                     let p, penv = CompilePatterns.desugar_pattern (lookup_effects env) p in
                       (p, fun env ->  eval (env ++ penv) body))
                  cases
              in
                I.switch env (ev e, cases, t)
          | DatabaseLit (name, (None, _)) ->
              I.database (ev (WithPos.make ~pos (RecordLit ([("name", name)],
                                          Some (WithPos.make ~pos (FnAppl (WithPos.make ~pos (Var "getDatabaseConfig"), [])))))))
          | DatabaseLit (name, (Some driver, args)) ->
              let args =
                match args with
                  | None -> WithPos.make ~pos (Sugartypes.Constant (Constant.String ""))
                  | Some args -> args
              in
                I.database
                  (ev (WithPos.make ~pos (RecordLit ([("name", name); ("driver", driver); ("args", args)], None))))
          | LensLit (table, Some t) ->
              let table = ev table in
                I.lens_handle (table, t)
          | LensSerialLit (lens, columns, Some t) ->
              let lens = ev lens in
              let columns = Lens.Alias.Set.of_list columns in
                I.lens_serial (lens, columns, t)
          | LensDropLit (lens, drop, key, default, Some t) ->
              let lens = ev lens in
              let default = ev default in
                I.lens_drop_handle (lens, drop, key, default, t)
          | LensSelectLit (lens, pred, Some t) ->
              let lens = ev lens in
              let trow = Lens.Type.sort t |> Lens.Sort.record_type in
              if Lens_sugar_conv.is_static trow pred then
                let pred = Lens_sugar_conv.lens_sugar_phrase_of_sugar pred |> Lens.Phrase.of_sugar in
                I.lens_select_handle (lens, `Static pred, t)
              else
                let pred = ev pred in
                I.lens_select_handle (lens, `Dynamic pred, t)
          | LensJoinLit (lens1, lens2, on, left, right, Some t) ->
              let lens1 = ev lens1 in
              let lens2 = ev lens2 in
              let on = Lens_sugar_conv.cols_of_phrase on in
              let left = Lens_sugar_conv.lens_sugar_phrase_of_sugar left |> Lens.Phrase.of_sugar in
              let right = Lens_sugar_conv.lens_sugar_phrase_of_sugar right |> Lens.Phrase.of_sugar in
                I.lens_join_handle (lens1, lens2, on, left, right, t)
          | LensCheckLit (lens, Some t) ->
              let lens = ev lens in
                I.lens_check (lens, t)
          | LensGetLit (lens, Some t) ->
              let lens = ev lens in
                I.lens_get (lens, t)
          | LensPutLit (lens, data, Some t) ->
              let lens = ev lens in
              let data = ev data in
                I.lens_put (lens, data, t)
          | TableLit {
              tbl_name; tbl_type = (tmp, _, Some (readtype, writetype, neededtype));
              tbl_keys; tbl_temporal_fields; tbl_database; _ } ->
              I.table_handle (ev tbl_database, ev tbl_name, ev tbl_keys,
                (tmp, readtype, writetype, neededtype), tbl_temporal_fields)
(*          (name, (_, Some (readtype, writetype, neededtype)), _constraints, keys, db) -> *)
          | Xml (tag, attrs, attrexp, children) ->
               if tag = "#" then
                 cofv (I.concat (instantiate "Nil"
                                   [(Type, Types.Primitive Primitive.XmlItem)],
                                 instantiate "Concat"
                                   [ (Type, Types.Primitive Primitive.XmlItem)
                                   ; (Row, eff)],
                                 List.map ev children))
                else
                  let attrs    = alistmap (List.map ev) attrs in
                  let children = List.map ev children in
                  let body     = I.xml (instantiate "^^" [(Row, eff)], tag, attrs,
                                        children) in
                  begin match attrexp with
                  | None   -> cofv body
                  | Some e -> cofv (I.apply_pure (instantiate_mb "addAttributes",
                                                 [body; ev e]))
                  end
          | TextNode name ->
              cofv
                (I.apply_pure
                   (instantiate_mb "stringToXml",
                    [ev (WithPos.make ~pos (Sugartypes.Constant (Constant.String name)))]))
          | Block (bs, e) -> eval_bindings Scope.Local env bs e
          | Query (range, policy, e, _) ->
              I.query (opt_map (fun (limit, offset) -> (ev limit, ev offset)) range, policy, ec e)
          | DBInsert (tmp, source, _fields, rows, None) ->
              let tmp = ir_temporal_insert tmp in
              let source = ev source in
              let rows = ev rows in
              I.db_insert env (tmp, source, rows)
          | DBInsert (tmp, source, _fields, rows, Some returning) ->
              let tmp = ir_temporal_insert tmp in
              let source = ev source in
              let rows = ev rows in
              let returning = ev returning in
              I.db_insert_returning env (tmp, source, rows, returning)
          | DBUpdate (upd, p, source, where, fields) ->
              let p, penv = CompilePatterns.desugar_pattern (lookup_effects env) p in
              let env' = env ++ penv in
              let source = ev source in
              let eval_opt = opt_map (eval env') in
              let where = eval_opt where in
              (* We need to do some annoying wrapping / unwrapping here to get the right
                  info across to db_update *)
              let upd =
                opt_map (fun upd ->
                    match upd with
                      | Sugartypes.TransactionTimeUpdate -> `TransactionUpdate
                      | Sugartypes.ValidTimeUpdate CurrentUpdate -> `ValidCurrentUpdate
                      | Sugartypes.ValidTimeUpdate (SequencedUpdate { validity_from; validity_to }) ->
                          let validity_from = ev validity_from in
                          let validity_to = ev validity_to in
                          `ValidSequencedUpdate (validity_from, validity_to)
                      | Sugartypes.ValidTimeUpdate (NonsequencedUpdate { from_time; to_time }) ->
                          let from_time = eval_opt from_time in
                          let to_time = eval_opt to_time in
                          `ValidNonsequencedUpdate (from_time, to_time)
                ) upd
              in
              let body = eval env' (WithPos.make ~pos (RecordLit (fields, None))) in
                I.db_update env (upd, p, source, where, body)
          | DBDelete (del, p, source, where) ->
              let p, penv = CompilePatterns.desugar_pattern (lookup_effects env) p in
              let env' = env ++ penv in
              let source = ev source in
              let eval_opt = opt_map (eval env') in
              let where = eval_opt where in
              let del =
                opt_map (fun del ->
                    match del with
                      | Sugartypes.TransactionTimeDeletion ->
                          `TransactionDelete
                      | Sugartypes.ValidTimeDeletion CurrentDeletion ->
                          `ValidCurrentDelete
                      | Sugartypes.ValidTimeDeletion NonsequencedDeletion ->
                          `ValidNonsequencedDelete
                      | Sugartypes.ValidTimeDeletion (SequencedDeletion {
                          validity_from; validity_to }) ->
                          `ValidSequencedDelete (ev validity_from, ev validity_to)
                ) del
              in
                I.db_delete env (del, p, source, where)
          | DBTemporalJoin (mode, e, _) -> I.temporal_join (mode, ec e)
          | Select (l, e) ->
             I.select (l, ev e)
          | Offer (e, cases, Some t) ->
             let eff = lookup_effects env in
             let cases =
                List.map
                  (fun (p, body) ->
                     let p, penv = CompilePatterns.desugar_pattern eff p in
                       (p, fun env ->  eval (env ++ penv) body))
                  cases
              in
                I.offer env (ev e, cases, t)

                  (* These things should all have been desugared already *)
          | Spawn _
          | Receive _
          | Section (Section.Project _)
          | FreezeSection (Section.Project _)
          | FunLit _
          | Iteration _
          | InfixAppl ((_, BinaryOp.RegexMatch _), _, _)
          | Regex _
          | Formlet _
          | Page _
          | FormletPlacement _
          | PagePlacement _
          | FormBinding _
          | ListLit _
          | Escape _
          | Upcast _
          | ConstructorLit _
          | Switch _
          | TableLit _
          | LensLit _
          | LensSerialLit _
          | LensDropLit _
          | LensSelectLit _
          | LensJoinLit _
          | LensCheckLit _
          | LensGetLit _
          | LensPutLit _
          | LensFunDepsLit _
          | LensKeysLit _
          | Offer _
          | QualifiedVar _
          | DoOperation _
          | TryInOtherwise _
          | Raise
          | Instantiate _ | Generalise _
          | CP _ ->
              Debug.print ("oops: " ^ show_phrasenode e);
              assert false

  and eval_bindings scope env bs' e =
    let ec = eval env in
    let ev = evalv env in
      match bs' with
        | [] -> ec e
        | { node = b; _ }::bs ->
            begin
              let open Sugartypes in
              match b with
                | Val ({node=Pattern.Variable bndr; _}, (tyvars, body), _, _)
                     when Binder.has_type bndr ->
                    let x  = Binder.to_name bndr in
                    let xt = Binder.to_type bndr in
                    let x_info = Var.make_info xt x scope in
                    let qs = List.map SugarQuantifier.get_resolved_exn tyvars in
                      I.letvar
                        (x_info,
                         ec body,
                         qs,
                         fun v ->
                           eval_bindings scope (extend [x] [(v, xt)] env) bs e)
                | Val (p, (_, body), _, _) ->
                    let p, penv = CompilePatterns.desugar_pattern (lookup_effects env) p in
                    let env' = env ++ penv in
                    let s = ev body in
                    let ss = eval_bindings scope env' bs e in
                      I.comp env (p, s, ss)
                | Fun { fun_binder           = bndr;
                        fun_definition       = (tyvars, NormalFunlit ([ps], body));
                        fun_location         = location;
                        fun_unsafe_signature = unsafe; _ }
                     when Binder.has_type bndr ->
                    let f  = Binder.to_name bndr in
                    let ft = Binder.to_type bndr in
                    let eff = TypeUtils.effect_row ft in
                    let ps, body_env =
                      List.fold_right
                        (fun p (ps, body_env) ->
                           let p, penv = CompilePatterns.desugar_pattern eff p in
                             p::ps, body_env ++ penv)
                        ps
                        ([], with_effects env eff) in
                    let body = eval body_env body in
                    let qs = List.map SugarQuantifier.get_resolved_exn tyvars in
                      I.letfun
                        (Var.make_info ft f scope, (qs, (body_env, ps, body)), location, unsafe)
                        (fun v -> eval_bindings scope (extend [f] [(v, ft)] env) bs e)
                | Exp e' ->
                    I.comp env (CompilePatterns.Pattern.Any, ev e', eval_bindings scope env bs e)
                | Funs defs ->
                   (* FIXME: inner and outers should be the same now,
                      so we shouldn't need to do all of this *)
                    let fs, inner_fts, outer_fts =
                      List.fold_right
                        (fun { rec_binder = bndr; rec_definition = ((_tyvars, inner_opt), _); _ }
                             (fs, inner_fts, outer_fts) ->
                          let f = Binder.to_name bndr in
                          let outer  = Binder.to_type bndr in
                          let (inner, _) = OptionUtils.val_of inner_opt in
                              (f::fs, inner::inner_fts, outer::outer_fts))
                        (nodes_of_list defs)
                        ([], [], []) in
                    let defs =
                      List.map
                        (fun { rec_binder           = bndr;
                               rec_definition       = ((tyvars, _), fnlit);
                               rec_location         = location;
                               rec_unsafe_signature = unsafe; _ } ->
                          let (pss, body) = Sugartypes.get_normal_funlit fnlit in
                          assert (List.length pss = 1);
                          let f  = Binder.to_name bndr in
                          let ft = Binder.to_type bndr in
                          let eff = TypeUtils.effect_row ft in
                          let ps = List.hd pss in
                          let qs = List.map SugarQuantifier.get_resolved_exn tyvars in
                          let ps, body_env =
                             List.fold_right
                               (fun p (ps, body_env) ->
                                  let p, penv = CompilePatterns.desugar_pattern eff p in
                                    p::ps, body_env ++ penv)
                               ps
                               ([], with_effects env eff) in
                           let body = fun vs -> eval (extend fs (List.combine vs inner_fts) body_env) body in
                           (Var.make_info ft f scope, (qs, (body_env, ps, body)), location, unsafe))
                        (nodes_of_list defs)
                    in
                    I.letrec defs (fun vs -> eval_bindings scope (extend fs (List.combine vs outer_fts) env) bs e)
                | Foreign alien ->
                   let binder =
                     fst (Alien.declaration alien)
                   in
                   assert (Binder.has_type binder);
                   let x  = Binder.to_name binder in
                   let xt = Binder.to_type binder in
                   I.alien (Var.make_info xt x scope, Alien.object_name alien, Alien.language alien,
                            fun v -> eval_bindings scope (extend [x] [(v, xt)] env) bs e)
                | Aliases _
                | Infix _ ->
                    (* Ignore type alias and infix declarations - they
                       shouldn't be needed in the IR *)
                    eval_bindings scope env bs e
                | Import _ | Open _ | Fun _
                | AlienBlock _ | Module _  -> assert false
            end

  and evalv env e =
    I.value_of_comp (eval env e)

  (* Given a program, return a triple consisting of:

     - globals
     - the main computation (a list of locals and a tail computation)
     - an environment mapping source names of global bindings to IR names

     The globals list contains all top-level bindings on which global
     bindings may depend, i.e., all top-level bindings from the start
     of the file up to and including the last global binding.

     The locals list contains all top-level bindings on which global
     bindings may not depend, i.e., all top-level bindings after the
     last global binding. *)
  let partition_program : program -> binding list * computation * nenv =
    fun (bs, main) ->
    let rec partition (globals, locals, nenv) =
      function
        | [] -> List.rev globals, List.rev locals, nenv
        | b::bs ->
            begin
              match b with
              | Let (b', _) when Var.(Scope.is_global (scope_of_binder b')) ->
                 let x = Var.var_of_binder b' in
                 let x_name = Var.name_of_binder b' in
                 partition (b::locals @ globals, [], Env.String.bind x_name x nenv) bs
              | Fun {fn_binder = b'; _} when Var.(Scope.is_global (scope_of_binder b')) ->
                 let f = Var.var_of_binder b' in
                 let f_name = Var.name_of_binder b' in
                 partition (b::locals @ globals, [], Env.String.bind f_name f nenv) bs
              | Rec defs ->
                 (* we depend on the invariant that mutually
                     recursive definitions all have the same scope *)
                 let scope, nenv =
                   List.fold_left
                     (fun (scope, nenv) {fn_binder = b'; _} ->
                       match Var.scope_of_binder b' with
                       | Scope.Global ->
                          let nenv' =
                            Env.String.bind (Var.name_of_binder b') (Var.var_of_binder b') nenv
                          in
                          Scope.Global, nenv'
                       | Scope.Local -> scope, nenv)
                     (Scope.Local, nenv) defs
                 in
                 begin
                   match scope with
                   | Scope.Global ->
                      partition (b::locals @ globals, [], nenv) bs
                   | Scope.Local ->
                      partition (globals, b::locals, nenv) bs
                 end
              | Alien { binder; _ }
                   when Var.Scope.is_global (Var.scope_of_binder binder) ->
                 let f = Var.var_of_binder binder in
                 let f_name = Var.name_of_binder binder in
                 partition (b::locals @ globals, [], Env.String.bind f_name f nenv) bs
              | _ -> partition (globals, b::locals, nenv) bs
            end in
    let globals, locals, nenv = partition ([], [], Env.String.empty) bs in
    globals, (locals, main), nenv


  let compile env (bindings, body) =
    Debug.if_set Basicsettings.show_stages (fun () -> "Compiling to IR...");
    let body =
      match body with
      | None -> WithPos.dummy (Sugartypes.RecordLit ([], None))
      | Some body -> body in
    let s = eval_bindings Scope.Global env bindings body in
    let r = (I.reify s) in
    Debug.if_set Basicsettings.show_stages (fun () -> "...compiled IR");
    Debug.if_set show_compiled_ir (fun () -> Ir.string_of_program r);
    r, I.sem_type s
end

module C = Eval(Interpretation(BindingListMonad))

let desugar_expression : env -> Sugartypes.phrase -> Ir.computation =
  fun env e ->
    let (bs, body), _ = C.compile env ([], Some e) in
      (bs, body)

let desugar_program : env -> Sugartypes.program -> Ir.binding list * Ir.computation * nenv =
  fun env p ->
    let (bs, body), _ = C.compile env p in
      C.partition_program (bs, body)

let desugar_definitions : env -> Sugartypes.binding list -> Ir.binding list * nenv =
  fun env bs ->
    let globals, _, nenv = desugar_program env (bs, None) in
      globals, nenv

type result =
  { globals: Ir.binding list;
    program: Ir.program;
    datatype: Types.datatype;
    context: Context.t }

let program : Context.t -> Types.datatype -> Sugartypes.program -> result
  = fun context datatype program ->
  let (nenv, _, _) as env =
    let nenv = Context.name_environment context in
    let tenv = Context.typing_environment context in
    let venv = Context.variable_environment context in
    (nenv, venv, tenv.Types.effect_row)
  in
  let program', _ = C.compile env program in
  let globals, program'', nenv' = C.partition_program program' in
  let nenv'' = Env.String.extend nenv nenv' in
  let venv =
    let tenv = Context.typing_environment context in
    Var.varify_env (nenv'', tenv.Types.var_env)
  in
  { globals; datatype; program = program'';
    context = Context.({ context with name_environment = nenv'';
                                      variable_environment = venv }) }
