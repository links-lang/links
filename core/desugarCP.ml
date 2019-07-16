open Utility
open Sugartypes
open SugarConstructors.DummyPositions
open SourceCode.WithPos

module TyEnv = Env.String

let accept_str    = "accept"
let close_str     = "closeBang"
let link_sync_str = "linkSync"
let new_str       = "new"
let receive_str   = "receive"
let request_str   = "request"
let send_str      = "send"
let wait_str      = "wait"
let wild_str      = "wild"

class desugar_cp env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method! phrasenode = function
    | CP p ->
       let rec desugar_cp = fun o {node = p; _} ->
         match p with
         | CPUnquote (bs, e) ->
            let envs = o#backup_envs in
            let (o, bs) = TransformSugar.listu o (fun o -> o#binding) bs in
            let (o, e, t) = o#phrase e in
            let o = o#restore_envs envs in
            o, block_node (bs, e), t
         | CPGrab ((c, _), None, p) ->
            let (o, e, t) = desugar_cp o p in
            o, block_node
                ([val_binding (any_pat dp) (fn_appl_var wait_str c)],
                 with_dummy_pos e), t
         | CPGrab ((c, Some (`Input (_a, s), grab_tyargs)), Some bndr, p) -> (* FYI: a = u *)
            let x = Binder.to_name bndr in
            let u = Binder.to_type bndr in
            let envs = o#backup_envs in
            let venv = TyEnv.bind (TyEnv.bind (o#get_var_env ()) (x, u))
                                  (c, s) in
            let o = {< var_env = venv >} in
            let (o, e, t) = desugar_cp o p in
            let o = o#restore_envs envs in
            o, block_node
                 ([val_binding (with_dummy_pos (
                                    Pattern.Record ([("1", variable_pat ~ty:u x);
                                                     ("2", variable_pat ~ty:s c)], None)))
                               (fn_appl receive_str grab_tyargs [var c])],
                 with_dummy_pos e), t
         | CPGive ((c, _), None, p) ->
            let (o, e, t) = desugar_cp o p in
            o, block_node
                ([val_binding (any_pat dp) (fn_appl_var close_str c)],
                 with_dummy_pos e), t
         | CPGive ((c, Some (`Output (_t, s), give_tyargs)), Some e, p) ->
            let envs = o#backup_envs in
            let o = {< var_env = TyEnv.bind (o#get_var_env ()) (c, s) >} in
            let (o, e, _typ) = o#phrase e in
            let (o, p, t) = desugar_cp o p in
            let o = o#restore_envs envs in
            o, block_node
                ([val_binding (variable_pat ~ty:s c)
                              (fn_appl send_str give_tyargs [e; var c])],
                 with_dummy_pos p), t
         | CPGiveNothing bndr ->
            let c = Binder.to_name bndr in
            let t = Binder.to_type bndr in
            o, Var c, t
         | CPSelect (bndr, label, p) ->
            let c = Binder.to_name bndr in
            let s = Binder.to_type bndr in
            let envs = o#backup_envs in
            let o = {< var_env = TyEnv.bind (o#get_var_env ()) (c, TypeUtils.select_type label s) >} in
            let (o, p, t) = desugar_cp o p in
            let o = o#restore_envs envs in
            o, block_node
                ([val_binding (variable_pat ~ty:(TypeUtils.select_type label s) c)
                               (with_dummy_pos (Select (label, var c)))],
                 with_dummy_pos p), t
         | CPOffer (bndr, cases) ->
            let c = Binder.to_name bndr in
            let s = Binder.to_type bndr in
            let desugar_branch (label, p) (o, cases) =
              let envs = o#backup_envs in
              let o = {< var_env = TyEnv.bind (o#get_var_env ()) (c, TypeUtils.choice_at label s) >} in
              let (o, p, t) = desugar_cp o p in
              let pat : Pattern.with_pos = with_dummy_pos (Pattern.Variant (label,
                      Some (variable_pat ~ty:(TypeUtils.choice_at label s) c))) in
              o#restore_envs envs, ((pat, with_dummy_pos p), t) :: cases in
            let (o, cases) = List.fold_right desugar_branch cases (o, []) in
            (match List.split cases with
                | (_, []) -> assert false (* Case list cannot be empty *)
                | (cases, t :: _ts) ->
                    o, Offer (var c, cases, Some t), t)
         | CPLink (bndr, bndr') ->
            let c = Binder.to_name bndr in
            let ct = Binder.to_type bndr in
            let d = Binder.to_name bndr' in
            o, fn_appl_node link_sync_str [`Type ct; `Row o#lookup_effects]
                            [var c; var d],
            Types.make_endbang_type
         | CPComp (bndr, left, right) ->
            let c = Binder.to_name bndr in
            let s = Binder.to_type bndr in
            let envs = o#backup_envs in
            let (o, left, _typ) = desugar_cp {< var_env = TyEnv.bind (o#get_var_env ()) (c, s) >} left in
            let (o, right, t) = desugar_cp {< var_env = TyEnv.bind (o#get_var_env ()) (c, Types.dual_type s) >} right in
            let o = o#restore_envs envs in

            let eff_fields, eff_row, eff_closed = Types.flatten_row o#lookup_effects in
            let eff_fields = StringMap.remove wild_str eff_fields in
            let eff_fields =
              if Settings.get_value Basicsettings.Sessions.exceptions_enabled then
                StringMap.remove Value.session_exception_operation eff_fields
              else
                eff_fields
            in

            let left_block =
                spawn Angel NoSpawnLocation (block (
                    [ val_binding (variable_pat ~ty:s c) (fn_appl_var accept_str c);
                      val_binding (variable_pat ~ty:Types.make_endbang_type c)
                                  (with_dummy_pos left)],
                    fn_appl_var close_str c))
                      ~row:(eff_fields, eff_row, eff_closed) in
            let o = o#restore_envs envs in
            o, block_node
                  ([val_binding (variable_pat ~ty:(`Application (Types.access_point, [`Type s])) c)
                                (fn_appl new_str [] []);
                    val_binding (any_pat dp) left_block;
                    val_binding (variable_pat ~ty:(Types.dual_type s) c)
                                (fn_appl_var request_str c)],
                   with_dummy_pos right), t
         | _ -> assert false in
       desugar_cp o p
    | e -> super#phrasenode e
end

let desugar_cp env = ((new desugar_cp env) : desugar_cp :> TransformSugar.transform)

let desugar_program : TransformSugar.program_transformer =
  fun env program -> snd3 ((desugar_cp env)#program program)

let desugar_sentence : TransformSugar.sentence_transformer =
  fun env sentence -> snd ((desugar_cp env)#sentence sentence)
