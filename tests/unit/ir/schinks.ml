open Links_core
open Utility
open State.Syntax

(* look at the .mli file if you just want to use this to write test cases *)

let id x = x

module CT = CommonTypes

module Repr = Schinks_repr

open Repr

type 'a t = 'a maker

let reify x = make x

(* helpers *)

let mk_const c = State.return (Ir.Constant c) |> State.return

let mk_primitive p : Types.datatype maker =
  State.return (Types.Primitive p) |> State.return

let default_subkind = Sugartypes.default_subkind

(*
 *
 * Links types
 *
 *)

let unit : Types.datatype maker = State.return Types.unit_type |> State.return

let int = mk_primitive CT.Primitive.Int
let string = mk_primitive CT.Primitive.String

let forall qs t =
  let* qs = State.List.map ~f:id qs in
  let+ t = t in
  let stage2 =
    let* qs = State.List.lift qs in
    let+ t = t in
    Types.ForAll (qs, t)
  in
  stage2

let tvar ?(pk = CT.PrimaryKind.Type) ?(sk = default_subkind) tname =
  let kind = (pk, sk) in
  let+ () = Repr.add_tname ~tname in
  let+ id = Repr.lookup_tname ~tname in
  Types.Meta (Unionfind.fresh (Types.Var (id, kind, `Rigid)))

let wi_tvar ?(pk = CT.PrimaryKind.Type) ?(sk = default_subkind) tid : Types.t t =
  let kind = (pk, sk) in
  let+ () = Repr.add_tid ~tid in
  Types.Meta (Unionfind.fresh (Types.Var (tid, kind, `Rigid))) |> State.return

let tvar_row ?(sk = default_subkind) name = tvar ~pk:CT.PrimaryKind.Row ~sk name

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

let binder ?(scope = Var.Scope.Local) name ty =
  let* ty = ty in
  let+ () = Repr.add_name ~name in
  let stage2 =
    let* ty = ty in
    let var_info = Var.make_info ty name scope in
    let+ id = Repr.lookup_name ~name in
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

let unit_value : Ir.value maker =
  Ir.Extend (StringMap.empty, None) |> State.return |> State.return

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
  let+ unit = return unit_value in
  let stage2 =
    let* binding = binding in
    let bs = [ binding ] in
    let+ unit = unit in
    (bs, unit)
  in
  stage2

let bindings_to_comp bindings =
  let* bs = State.List.map ~f:id bindings in
  let+ unit = return unit_value in
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
