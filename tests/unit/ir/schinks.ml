open Links_core
open Utility
open State.Syntax

(* look at the .mli file if you just want to use this to write test cases *)

module CT = CommonTypes

module Repr = Schinks_repr

open Repr

type 'a t = 'a Repr.t

let reify x = make x

(* helpers *)

let id = Utility.identity

let mk_const c = State.return (Ir.Constant c) |> State.return

let mk_primitive p : Types.datatype t =
  State.return (Types.Primitive p) |> State.return

let default_subkind = Sugartypes.default_subkind

let check_assoc_list_for_duplicates assoc description =
  let has_duplicates =
    List.fold_left
      (fun (seen, dupls) (key, _) ->
        (StringSet.add key seen, dupls && StringSet.mem key seen))
      (StringSet.empty, [] <> assoc)
      assoc
    |> snd
  in
  if has_duplicates then
    raise (SchinksError (Printf.sprintf "Duplicate fields in %s!" description))

(* Got a list of tuples (with the second arg being a t) and another arg *)
let helper_tuple1_with_transform :
      'a 'b 'c 'd 'e 'r.
      ('a * 'b t) list ->
      'c ->
      arg1_transf1:('c -> 'd Repr.stage1) ->
      arg1_transf2:('d -> 'e lookup) ->
      (('a * 'b) list -> 'e -> 'r) ->
      'r t =
 fun tuples arg1 ~arg1_transf1 ~arg1_transf2 builder ->
  let* arg1 = arg1_transf1 arg1 in
  let+ tuples =
    State.List.map
      ~f:(fun (x, y) ->
        let+ y = y in
        (x, y))
      tuples
  in
  let stage2 =
    let* arg1 = arg1_transf2 arg1 in
    let+ tuples =
      State.List.map
        ~f:(fun (x, y) ->
          let+ y = y in
          (x, y))
        tuples
    in
    builder tuples arg1
  in
  stage2

(* let helper_tuple1_with_transform : 'a 'b 'c 'd 'e 'r.
 * (('a * 'b t) list) -> ('c) -> arg1_transf1 : ('c -> 'd t) -> arg1_transf2 : ('d lookup -> 'e lookup) ->
 *     ( ('a * 'b) list -> 'e -> 'r) -> 'r t = fun
 *     tuples arg1 ~arg1_transf1 ~arg1_transf2 builder ->
 *   let* arg1 = arg1_transf1 arg1 in
 *   let+ tuples =
 *     State.List.map
 *       ~f:(fun (x, y) ->
 *         let+ y = y in
 *         (x, y))
 *       tuples
 *   in
 *   let stage2 =
 *     let* arg1 = arg1_transf2 arg1 in
 *     let+ tuples =
 *       State.List.map
 *         ~f:(fun (x, y) ->
 *           let+ y = y in
 *           (x, y))
 *         tuples
 *     in
 *     builder tuples arg1
 *   in
 *   stage2 *)

let helper_tuple1 tuples arg1 =
  helper_tuple1_with_transform tuples arg1 ~arg1_transf1:id ~arg1_transf2:id

(* Got a list and one argument *)
let helper_list1 (builder : 'a list -> 'b -> 'r) (arg1 : 'a t list)
    (arg2 : 'b t) : 'r t =
  let* arg1 = State.List.lift arg1 in
  let+ arg2 = arg2 in
  let stage2 =
    let* arg1 = State.List.lift arg1 in
    let+ arg2 = arg2 in
    builder arg1 arg2
  in
  stage2

(* Got Just one argument *)
let _helper1 (builder : 'a -> 'r) (arg : 'a t) : 'r t =
  let+ arg = arg in
  let+ arg = arg in
  builder arg

let helper2 (builder : 'a -> 'b -> 'r) (arg1 : 'a t) (arg2 : 'b t) : 'r t =
  let* arg1 = arg1 in
  let+ arg2 = arg2 in
  let* arg1 = arg1 in
  let+ arg2 = arg2 in
  builder arg1 arg2

(*
 *
 * Links types
 *
 *)

let lift_type t =
  let tids = Types.free_bound_type_vars t |> List.map fst in
  let+ () = Repr.add_tids ~tids in
  State.return t

let unit_t : Types.datatype t = State.return Types.unit_type |> State.return

let int = mk_primitive CT.Primitive.Int
let string = mk_primitive CT.Primitive.String

let forall = helper_list1 (fun qs t -> Types.ForAll (qs, t))

let tvar ?(pk = CT.PrimaryKind.Type) ?(sk = default_subkind) tname =
  let kind = (pk, sk) in
  let+ () = Repr.add_tname ~tname in
  let+ id = Repr.lookup_tname ~tname in
  Types.Meta (Unionfind.fresh (Types.Var (id, kind, `Rigid)))

let wi_tvar ?(pk = CT.PrimaryKind.Type) ?(sk = default_subkind) tid : Types.t t
    =
  let kind = (pk, sk) in
  let+ () = Repr.add_tid ~tid in
  Types.Meta (Unionfind.fresh (Types.Var (tid, kind, `Rigid))) |> State.return

let tvar_row ?(sk = default_subkind) name = tvar ~pk:CT.PrimaryKind.Row ~sk name

let targ ?(pk = CT.PrimaryKind.Type) t =
  let+ t = t in
  let+ t = t in
  (pk, t)

let fun_t ?effects parameters codomain =
  let* p = State.List.map ~f:id parameters in
  let params_tupled =
    let+ p = State.List.map ~f:id p in
    Types.make_tuple_type p
  in
  let* c = codomain in
  (* TODO: This could probably be rewritten using state.option *)
  match effects with
  | None ->
      let stage2 =
        let* params_tupled = params_tupled in
        let+ c = c in
        Types.Function (params_tupled, Types.make_empty_closed_row (), c)
      in
      stage2 |> State.return
  | Some et ->
      let+ e = et in
      let stage2 =
        let* params_tupled = params_tupled in
        let* e = e in
        let+ c = c in
        Types.Function (params_tupled, e, c)
      in
      stage2

(* need to make sure that d .-->{e} c
   becomes Function(d,c,e) even though the operator is just binary, not ternary *)
let ( .-->{} ) lhs middle rhs = fun_t ~effects:middle lhs rhs

let fun_ct parameters codomain = fun_t parameters codomain

let ( |--> ) = fun_ct

let wild_fun_ct parameters codomain =
  let effects =
    State.return
      (Types.make_singleton_closed_row ("wild", Types.Present Types.unit_type))
    |> State.return
  in
  fun_t ~effects parameters codomain

let ( |~~> ) = wild_fun_ct

let record_t ?row_var assoc =
  let row_var =
    match row_var with
    | Some rv -> rv
    | None -> Unionfind.fresh Types.Closed |> State.return |> State.return
  in
  check_assoc_list_for_duplicates assoc "record type";
  helper_tuple1 assoc row_var (fun assoc row_var ->
      let map = StringMap.from_alist assoc in
      Types.Record (Types.Row (map, row_var, false)))

let variant ?row_var assoc =
  check_assoc_list_for_duplicates assoc "variant type";
  let row_var =
    match row_var with
    | Some rv -> rv
    | None -> Unionfind.fresh Types.Closed |> State.return |> State.return
  in
  helper_tuple1 assoc row_var (fun assoc row_var ->
      let map = StringMap.from_alist assoc in
      Types.Variant (Types.Row (map, row_var, false)))

(* Rows *)
let closed = Unionfind.fresh Types.Closed |> State.return |> State.return

let row_var rv =
  let sk = default_subkind in
  let+ () = Repr.add_tname ~tname:rv in
  let+ id = Repr.lookup_tname ~tname:rv in
  Unionfind.fresh (Types.Var (id, (CT.PrimaryKind.Row, sk), `Rigid))

let row assoc rv =
  let mk_row assoc rv =
    let map = StringMap.from_alist assoc in
    Types.Row (map, rv, false)
  in
  check_assoc_list_for_duplicates assoc "row";
  helper_tuple1 assoc rv mk_row

(* Presence information *)

let present t =
  let+ t = t in
  let+ t = t in
  Types.Present t

let absent = Types.Absent |> State.return |> State.return

let presence_var v = tvar ~pk:CT.PrimaryKind.Presence v

(* Quantifiers *)

let wi_q ?(pk = CT.PrimaryKind.Type) ?(sk = default_subkind) tid =
  let kind = (pk, sk) in
  let+ () = Repr.add_tid ~tid in
  (tid, kind) |> State.return

let q ?(pk = CT.PrimaryKind.Type) ?(sk = default_subkind) tname =
  let kind = (pk, sk) in
  let* () = Repr.add_tname ~tname in
  let stage2 =
    let+ id = Repr.lookup_tname ~tname in
    (id, kind)
  in
  State.return stage2

let q_row ?(sk = default_subkind) name = q ~pk:CT.PrimaryKind.Row ~sk name

(*
 * IR constructs
 *)

let int_const i = mk_const (CT.Constant.Int i)
let i = int_const

let string_const s = mk_const (CT.Constant.String s)
let s = string_const

let var name =
  let* () = Repr.add_name ~name in
  let stage2 =
    let+ id = Repr.lookup_name ~name in
    Ir.Variable id
  in
  stage2 |> State.return

let wi_var id =
  let+ () = Repr.add_id ~id in
  Ir.Variable id |> State.return

let closure function_name tyargs value =
  let* tyargs = State.List.map ~f:id tyargs in
  let* value = value in
  let+ () = Repr.add_name ~name:function_name in
  let stage2 =
    let* tyargs = State.List.map ~f:id tyargs in
    let* value = value in
    let+ id = Repr.lookup_name ~name:function_name in
    Ir.Closure (id, tyargs, value)
  in
  stage2

let tapp v targs = helper_tuple1 targs v (fun targs v -> Ir.TApp (v, targs))

let tabs = helper_list1 (fun quants v -> Ir.TAbs (quants, v))

let inject name = helper2 (fun v ty -> Ir.Inject (name, v, ty))

let binder ?(scope = Var.Scope.Local) name ty =
  let* ty = ty in
  let+ id_candidate =
    match name with
    | "_" -> Repr.fresh_id
    | _ ->
        let+ () = Repr.add_name ~name in
        -1
  in
  let stage2 =
    let* ty = ty in
    let var_info = Var.make_info ty name scope in
    let+ id =
      match name with
      | "_" -> State.return id_candidate
      | _ -> Repr.lookup_name ~name
    in
    Var.make_binder id var_info
  in
  stage2

let wi_binder ?(scope = Var.Scope.Local) id ty =
  let* ty = ty in
  let+ () = Repr.add_id ~id in
  let stage2 =
    let+ ty = ty in
    let fake_name = Printf.sprintf "gen_%d" id in
    let var_info = Var.make_info ty fake_name scope in
    Var.make_binder id var_info
  in
  stage2

let build_record (extendee : Ir.value t option)
    (assoc : (string * Ir.value t) list) : Ir.value t =
  let _, has_duplicates =
    List.fold_left
      (fun (seen, dupls) (key, _) ->
        (StringSet.add key seen, dupls && StringSet.mem key seen))
      (StringSet.empty, [] <> assoc)
      assoc
  in
  let stage2 (extendee : Ir.value lookup option)
      (assoc : (string * Ir.value Repr.lookup) list) : Ir.value lookup =
    let* assoc =
      State.List.map
        ~f:(fun (x, y) ->
          let+ y = y in
          (x, y))
        assoc
    in
    let map = StringMap.from_alist assoc in
    let finalize e =
      if has_duplicates then raise (SchinksError "Duplicate fields in record!")
      else Ir.Extend (map, e)
    in
    match extendee with
    | None -> State.return (finalize None)
    | Some e ->
        let+ e = e in
        finalize (Some e)
  in
  let assoc : (string * Ir.value lookup) list stage1 =
    State.List.map
      ~f:(fun (x, y) ->
        let+ y = y in
        (x, y))
      assoc
  in
  let* (assoc : (string * Ir.value lookup) list) = assoc in
  match extendee with
  | Some (e : Ir.value t) ->
      let+ (e : Ir.value lookup) = e in
      stage2 (Some e) assoc
  | None -> State.return (stage2 None assoc)

let record assoc = build_record None assoc

let extend_record extendee assoc = build_record (Some extendee) assoc

let unit : Ir.value t = record []

let return value =
  let+ v = value in
  let stage2 =
    let+ v = v in
    Ir.Return v
  in
  stage2

let if_ cond then_ else_ =
  let* c = cond in
  let* t = then_ in
  let+ e = else_ in
  let stage2 =
    let* c = c in
    let* t = t in
    let+ e = e in
    Ir.If (c, t, e)
  in
  stage2

let apply f args =
  let* f = f in
  let+ args = State.List.map ~f:id args in
  let stage2 =
    let* f = f in
    let+ args = State.List.map ~f:id args in
    Ir.Apply (f, args)
  in
  stage2

let case (v : Ir.value t) ?(default : (Ir.binder t * Ir.computation t) option)
    (cases : (string * Ir.binder t * Ir.computation t) list) :
    Ir.tail_computation t =
  let* v = v in
  let f (x, y) =
    let* x = x in
    let* y = y in
    State.return (Some (x, y))
  in
  let g (variant, x, y) =
    let* x = x in
    let+ y = y in
    (variant, x, y)
  in
  let* default = State.Option.bind ~f default in
  let+ cases = State.List.map ~f:g cases in
  let* v = v in
  let* default = State.Option.bind ~f default in
  let+ cases = State.List.map ~f:g cases in
  let assoc = List.map (fun (a, b, c) -> (a, (b, c))) cases in
  check_assoc_list_for_duplicates assoc "variants of case";
  let case_map = StringMap.from_alist assoc in
  Ir.Case (v, case_map, default)

(*
 *
 * IR computations
 *
 *)

let tc_to_comp tail_computation =
  let+ tc = tail_computation in
  let stage2 =
    let+ tc = tc in
    ([], tc)
  in
  stage2

let binding_to_comp binding =
  let* binding = binding in
  let+ unit = return unit in
  let stage2 =
    let* binding = binding in
    let bs = [ binding ] in
    let+ unit = unit in
    (bs, unit)
  in
  stage2

let bindings_to_comp bindings =
  let* bs = State.List.map ~f:id bindings in
  let+ unit = return unit in
  let stage2 =
    let* bs = State.List.map ~f:id bs in
    let+ unit = unit in
    (bs, unit)
  in
  stage2

let computation bindings tail_comp =
  let* bs = State.List.map ~f:id bindings in
  let+ tc = tail_comp in
  let stage2 =
    let* bs = State.List.map ~f:id bs in
    let+ tc = tc in
    (bs, tc)
  in
  stage2

(*
 *
 * IR bindings
 *
 *)

let wi_let_ binder ?(tparams = []) body =
  let* bin = binder in
  let* tparams = State.List.map ~f:id tparams in
  let+ body = body in
  let stage2 =
    let* bin = bin in
    let* tparams = State.List.map ~f:id tparams in
    let+ body = body in
    Ir.Let (bin, (tparams, body))
  in
  stage2

let let_ name ty ?(tparams = []) ?scope body =
  let bin = binder ?scope name ty in
  wi_let_ bin ~tparams body

let wi_def bin ?(tparams = []) params ?closure_var
    ?(location = CT.Location.Server) ?(unsafe_sig = false) body =
  let* bin = bin in
  let* tparams = State.List.lift tparams in
  let* params = State.List.lift params in
  let* body = body in
  match closure_var with
  | None ->
      let stage2 =
        let* bin = bin in
        let* tparams = State.List.lift tparams in
        let* fn_params = State.List.lift params in
        let+ fn_body = body in
        {
          Ir.fn_binder = bin;
          fn_tyvars = tparams;
          fn_params;
          fn_body;
          fn_closure = None;
          fn_location = location;
          fn_unsafe = unsafe_sig;
        }
      in
      stage2 |> State.return
  | Some cv_binder ->
      let+ cv_binder = cv_binder in
      let stage2 =
        let* bin = bin in
        let* tparams = State.List.lift tparams in
        let* fn_params = State.List.lift params in
        let* fn_body = body in
        let+ cv_binder = cv_binder in
        {
          Ir.fn_binder = bin;
          fn_tyvars = tparams;
          fn_params;
          fn_body;
          fn_closure = Some cv_binder;
          fn_location = location;
          fn_unsafe = unsafe_sig;
        }
      in
      stage2

let def name ty ?(tparams = []) params ?scope ?closure_var
    ?(location = CT.Location.Server) ?(unsafe_sig = false) body =
  let bin = binder ?scope name ty in
  let params =
    List.map
      (fun (p_name, p_ty) -> binder ~scope:Var.Scope.Local p_name p_ty)
      params
  in
  let closure_var =
    Option.map
      (fun (cv_name, cv_ty) -> binder ~scope:Var.Scope.Local cv_name cv_ty)
      closure_var
  in
  wi_def bin ~tparams params ?closure_var ~location ~unsafe_sig body

let rec_ fun_defs =
  let+ fun_defs = State.List.lift fun_defs in
  let stage2 =
    let+ fun_defs = State.List.lift fun_defs in
    Ir.Rec fun_defs
  in
  stage2

let wi_fun_ binder ?tparams params ?closure_var ?location ?unsafe_sig body =
  let+ fun_def =
    wi_def binder ?tparams params ?closure_var ?location ?unsafe_sig body
  in
  let stage2 =
    let+ fun_def = fun_def in
    Ir.Fun fun_def
  in
  stage2

let fun_ name ty ?tparams params ?closure_var ?location ?unsafe_sig body =
  let+ fun_def =
    def name ty ?tparams params ?closure_var ?location ?unsafe_sig body
  in
  let stage2 =
    let+ fun_def = fun_def in
    Ir.Fun fun_def
  in
  stage2
