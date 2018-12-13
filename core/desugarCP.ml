open Utility
open Sugartypes

module TyEnv = Env.String

class desugar_cp env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method! phrasenode = function
    | `CP p ->
       let rec desugar_cp = fun o {node = p; pos} ->
         let add_pos x = with_pos pos x in
         match p with
         | `Unquote (bs, e) ->
            let envs = o#backup_envs in
            let (o, bs) = TransformSugar.listu o (fun o -> o#binding) bs in
            let (o, e, t) = o#phrase e in
            let o = o#restore_envs envs in
            o, `Block (bs, e), t
         | `Grab ((c, _), None, p) ->
            let (o, e, t) = desugar_cp o p in
            o, `Block
                ([add_pos (`Val (add_pos `Any, ([],
                                      add_pos (`FnAppl (add_pos (`Var "wait"),
                                                       [add_pos (`Var c)]))),
                                      `Unknown, None))],
                 add_pos e), t
         | `Grab ((c, Some (`Input (_a, s), grab_tyargs)), Some {node=x, Some u; _}, p) -> (* FYI: a = u *)
            let envs = o#backup_envs in
            let venv = TyEnv.bind (TyEnv.bind (o#get_var_env ())
                                              (x, u))
                                  (c, s) in
            let o = {< var_env = venv >} in
            let (o, e, t) = desugar_cp o p in
            let o = o#restore_envs envs in
            o, `Block
                ([add_pos (`Val (add_pos (`Record ([("1", add_pos (`Variable (make_binder x u pos)));
                                                    ("2", add_pos (`Variable (make_binder c s pos)))], None)),
                                 ([], add_pos (`FnAppl (add_pos (Sugartypes.tappl (`Var "receive", grab_tyargs)),
                                                        [add_pos (`Var c)]))),
                                 `Unknown, None))],
                 add_pos e), t
         | `Give ((c, _), None, p) ->
            let (o, e, t) = desugar_cp o p in
            o, `Block
                ([add_pos (`Val (add_pos `Any,
                                 ([], add_pos (`FnAppl (add_pos (`Var "close"),
                                                        [add_pos (`Var c)]))),
                                 `Unknown, None))],
                 add_pos e), t
         | `Give ((c, Some (`Output (_t, s), give_tyargs)), Some e, p) ->
            let envs = o#backup_envs in
            let o = {< var_env = TyEnv.bind (o#get_var_env ()) (c, s) >} in
            let (o, e, _typ) = o#phrase e in
            let (o, p, t) = desugar_cp o p in
            let o = o#restore_envs envs in
            o, `Block
                ([add_pos (`Val (add_pos (`Variable (make_binder c s pos)),
                                 ([], add_pos (`FnAppl (add_pos (Sugartypes.tappl (`Var "send", give_tyargs)),
                                                        [e; add_pos (`Var c)]))),
                                 `Unknown, None))],
                 add_pos p), t
         | `GiveNothing ({node=c, Some t; _}) ->
            o, `Var c, t
         | `Select ({node=c, Some s; _}, label, p) ->
            let envs = o#backup_envs in
            let o = {< var_env = TyEnv.bind (o#get_var_env ()) (c, TypeUtils.select_type label s) >} in
            let (o, p, t) = desugar_cp o p in
            let o = o#restore_envs envs in
            o, `Block
                ([add_pos (`Val (add_pos (`Variable (make_binder c (TypeUtils.select_type label s) pos)),
                                 ([], add_pos (`Select (label, (add_pos (`Var c))))),
                                 `Unknown, None))],
                 add_pos p), t
         | `Offer ({node=c, Some s; _}, cases) ->
            let desugar_branch (label, p) (o, cases) =
              let envs = o#backup_envs in
              let o = {< var_env = TyEnv.bind (o#get_var_env ()) (c, TypeUtils.choice_at label s) >} in
              let (o, p, t) = desugar_cp o p in
              let pat : pattern = add_pos (`Variant (label, Some (add_pos (`Variable (make_binder c (TypeUtils.choice_at label s) pos))))) in
              o#restore_envs envs, ((pat, add_pos p), t) :: cases in
            let (o, cases) = List.fold_right desugar_branch cases (o, []) in
            (match List.split cases with
                | (_, []) -> assert false (* Case list cannot be empty *)
                | (cases, t :: _ts) ->
                    o, `Offer (add_pos (`Var c),
                               cases,
                               Some t), t)
         | `Link ({node=c, Some ct; _}, {node=d, Some _; _}) ->
            o, `FnAppl (add_pos (Sugartypes.tappl (`Var "linkSync", [`Type ct; `Row o#lookup_effects])),
                        [add_pos (`Var c); add_pos (`Var d)]), Types.make_endbang_type
         | `Comp ({node=c, Some s; _}, left, right) ->
            let envs = o#backup_envs in
            let (o, left, _typ) = desugar_cp {< var_env = TyEnv.bind (o#get_var_env ()) (c, s) >} left in
            let (o, right, t) = desugar_cp {< var_env = TyEnv.bind (o#get_var_env ()) (c, Types.dual_type s) >} right in
            let o = o#restore_envs envs in
            let left_block = add_pos (`Spawn (`Angel, `NoSpawnLocation, add_pos (`Block (
                                     [ add_pos (`Val (add_pos (`Variable (make_binder c s pos)),
                                                      ([], add_pos (`FnAppl (add_pos (`Var "accept"), [add_pos (`Var c)]))),
                                                      `Unknown, None));
                                       add_pos (`Val (add_pos (`Variable (make_binder c Types.make_endbang_type pos)),
                                                      ([], add_pos left), `Unknown, None))],
                                       add_pos (`FnAppl (add_pos (`Var "close"), [add_pos (`Var c)])))),
                                              Some (Types.make_singleton_closed_row ("wild", `Present Types.unit_type)))) in
            let o = o#restore_envs envs in
            o, `Block
                  ([add_pos (`Val (add_pos (`Variable (make_binder c (`Application (Types.access_point, [`Type s])) pos)),
                                   ([], add_pos (`FnAppl (add_pos (`Var "new"), []))),
                                   `Unknown, None));
                    add_pos (`Val (add_pos `Any, ([], left_block), `Unknown, None));
                    add_pos (`Val (add_pos (`Variable (make_binder c (Types.dual_type s) pos)),
                                   ([], add_pos (`FnAppl (add_pos (`Var "request"), [add_pos (`Var c)]))),
                                   `Unknown, None))],
                   add_pos right), t
         | _ -> assert false in
       desugar_cp o p
    | e -> super#phrasenode e
end

let desugar_cp env = ((new desugar_cp env) : desugar_cp :> TransformSugar.transform)
