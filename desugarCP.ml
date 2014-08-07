open Utility
open Sugartypes

module TyEnv = Env.String

class desugar_cp env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method phrasenode = function
    | `CP (bs, p) ->
       let rec desugar_cp = fun o (p, pos) ->
         let add_pos x = (x, pos) in
         match p with
         | `Unquote (e, vs) ->
            let (o, e, t) = o#phrase e in
            o, `FnAppl (e, List.map (fun v -> add_pos (`Var v)) vs)
         | `Grab ((c, _), None, p) ->
            let o, e = desugar_cp o p in
            o, `Block
                  ([add_pos (`Val ([], add_pos `Any,
                                   add_pos (`FnAppl (add_pos (`Var "wait"),
                                                     [add_pos (`Var c)])),
                                   `Unknown, None))],
                   add_pos e)
         | `Grab ((c, Some (`Input (_a, s), grab_tyargs) as cbind), Some (x, Some u, _), p) -> (* FYI: a = u *)
            let envs = o#backup_envs in
            let venv = TyEnv.bind (TyEnv.bind (o#get_var_env ())
                                              (x, u))
                                  (c, s) in
            let o = {< var_env = venv >} in
            let o, e = desugar_cp o p in
            let o = o#restore_envs envs in
            o, `Block
                  ([add_pos (`Val ([], add_pos (`Record ([("1", add_pos (`Variable (x, Some u, pos)));
                                                          ("2", add_pos (`Variable (c, Some s, pos)))], None)),
                                   add_pos (`FnAppl (add_pos (Sugartypes.tappl (`Var "grab", grab_tyargs)),
                                                     [add_pos (`Var c)])),
                                   `Unknown, None))],
                  add_pos e)
         | `Give ((c, Some (`Output (_t, s), give_tyargs) as cbind), ((_, Some u, _) as dbind), p, q) ->
            let envs = o#backup_envs in
            let o, dbind = o#binder dbind in
            let o, p = desugar_cp o p in
            let o = o#restore_envs envs in
            let o = {< var_env = TyEnv.bind (o#get_var_env ()) (c, s) >} in
            let o, q = desugar_cp o q in
            let o = o#restore_envs envs in
            o, `Block
                  ([add_pos (`Val ([], add_pos (`Variable (c, Some s, pos)),
                                   add_pos (`FnAppl (add_pos (Sugartypes.tappl (`Var "give", give_tyargs)),
                                                     [add_pos (`FnAppl (add_pos (`Var "linfork"),
                                                                        [add_pos (`FunLit (Some [Types.make_tuple_type [u],Types.make_empty_closed_row ()],
                                                                                           `Lin, ([[add_pos (`Variable dbind)]], add_pos p)))]));
                                                      add_pos (`Var c)])),
                                   `Unknown, None))],
                   add_pos q)
         | `GiveNothing c ->
            o, `FnAppl (add_pos (`Var "close"), [add_pos (`Var c)])
         | `Select ((c, Some s, _ as cbind), label, p) ->
            let envs = o#backup_envs in
            let o = {< var_env = TyEnv.bind (o#get_var_env ()) (c, TypeUtils.select_type label s) >} in
            let o, p = desugar_cp o p in
            let o = o#restore_envs envs in
            o, `Block
                 ([add_pos (`Val ([], add_pos (`Variable (c, Some (TypeUtils.select_type label s), pos)),
                                  add_pos (`Select (label, (add_pos (`Var c)))),
                                  `Unknown, None))],
                  add_pos p)
         | `Offer ((c, Some s, _ as cbind), cases) ->
            let desugar_branch (label, p) (o, cases) =
              let envs = o#backup_envs in
              let o = {< var_env = TyEnv.bind (o#get_var_env ()) (c, TypeUtils.choice_at label s) >} in
              let o, p = desugar_cp o p in
              let pat : pattern = add_pos (`Variant (label, Some (add_pos (`Variable (c, Some (TypeUtils.choice_at label s), pos))))) in
              o#restore_envs envs, (pat, add_pos p) :: cases in
            let (o, cases) = List.fold_right desugar_branch cases (o, []) in
            o, `Offer (add_pos (`Var c),
                       cases,
                       Some Types.unit_type)
         | `Fuse ((c, Some ct, _), (d, Some dt, _)) ->
            o, `FnAppl (add_pos (Sugartypes.tappl (`Var "fuse", [`Type ct; `Row o#lookup_effects])),
                        [add_pos (`Var c); add_pos (`Var d)])
         | `Comp ((c, Some s, _ as cbind), left, right) ->
            let envs = o#backup_envs in
            let o, left = desugar_cp {< var_env = TyEnv.bind (o#get_var_env ()) (c, s) >} left in
            let o, right = desugar_cp {< var_env = TyEnv.bind (o#get_var_env ()) (c, Types.dual_type s) >} right in
            let o = o#restore_envs envs in
            let left_block = add_pos (`Spawn (add_pos (`Block ([add_pos (`Val ([], add_pos (`Variable (c, Some s, pos)),
                                                                               add_pos (`FnAppl (add_pos (`Var "accept"), [add_pos (`Var c)])),
                                                                               `Unknown, None))],
                                                               add_pos left)),
                                              Some (Types.make_singleton_closed_row ("wild", `Present Types.unit_type)))) in
            let o = o#restore_envs envs in
            o, `Block
                  ([add_pos (`Val ([], add_pos (`Variable (c, Some (`Application (Types.access_point, [`Type s])), pos)),
                                   add_pos (`FnAppl (add_pos (`Var "new"), [])),
                                   `Unknown, None));
                    add_pos (`Val ([], add_pos (`Any), left_block, `Unknown, None));
                    add_pos (`Val ([], add_pos (`Variable (c, Some (Types.dual_type s), pos)),
                                   add_pos (`FnAppl (add_pos (`Var "request"), [add_pos (`Var c)])),
                                   `Unknown, None))],
                   add_pos right)
         | `In ((c, _, _), e) ->
            let o, e, _ = o#phrase e in
            o, `FnAppl (add_pos (`Var "give"),
                        [e; add_pos (`Var c)])
         | `Out ((c, _, _), d, p) ->
            let envs = o#backup_envs in
            let o, d = o#binder d in
            let o, p = desugar_cp o p in
            let o = o#restore_envs envs in
            o, `Block ([add_pos (`Val ([], add_pos (`Variable d),
                                       add_pos (`FnAppl (add_pos (`Var "grab"),
                                                         [add_pos (`Var c)])),
                                       `Unknown, None))],
                       add_pos p)
         | `Link ((c, _, _), (d, _, _)) ->
            o, `FnAppl (add_pos (`Var "fuse"),
                        [add_pos (`Var c); add_pos (`Var d)])
         | _ -> assert false in
       let rec build_lambda e = function
         | [] -> e
         | b :: bs -> assert false in
       let result_type = assert false in
       let o, e = desugar_cp o p in
       o, build_lambda e bs, result_type
    | e -> super#phrasenode e
end

let desugar_cp env = ((new desugar_cp env) : desugar_cp :> TransformSugar.transform)
