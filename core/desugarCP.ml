open Utility
open Sugartypes

module TyEnv = Env.String

class desugar_cp env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method! phrasenode = function
    | `CP p ->
       let rec desugar_cp = fun o (p, pos) ->
         let add_pos x = (x, pos) in
         match p with
         | `Unquote (bs, e) ->
            let envs = o#backup_envs in
            let (o, bs) = TransformSugar.listu o (fun o -> o#binding) bs in
            let (o, e, t) = o#phrase e in
            let o = o#restore_envs envs in
            o, `Block (bs, e), t
         | `Grab ((c, _), None, p) ->
            let (o, e, t) = desugar_cp o p in
            let q = QualifiedName.of_name "wait" in
            let q' = QualifiedName.of_name c in
            o, `Block
                  ([add_pos (`Val ([], add_pos `Any,
                                   add_pos (`FnAppl (add_pos (`Var q),
                                                     [add_pos (`Var q')])),
                                   `Unknown, None))],
                   add_pos e), t
         | `Grab ((c, Some (`Input (_a, s), grab_tyargs)), Some (x, Some u, _), p) -> (* FYI: a = u *)
            let envs = o#backup_envs in
            let venv = TyEnv.bind (TyEnv.bind (o#get_var_env ())
                                              (x, u))
                                  (c, s) in
            let o = {< var_env = venv >} in
            let (o, e, t) = desugar_cp o p in
            let o = o#restore_envs envs in
            let q = QualifiedName.of_name "receive" in
            let q' = QualifiedName.of_name c in
            o, `Block
                  ([add_pos (`Val ([], add_pos (`Record ([("1", add_pos (`Variable (x, Some u, pos)));
                                                          ("2", add_pos (`Variable (c, Some s, pos)))], None)),
                                   add_pos (`FnAppl (add_pos (Sugartypes.tappl (`Var q, grab_tyargs)),
                                                     [add_pos (`Var q')])),
                                   `Unknown, None))],
                  add_pos e), t
         | `Give ((c, _), None, p) ->
            let (o, e, t) = desugar_cp o p in
            let q = QualifiedName.of_name "close" in
            let q' = QualifiedName.of_name c in
            o, `Block
                  ([add_pos (`Val ([], add_pos `Any,
                                   add_pos (`FnAppl (add_pos (`Var q),
                                                     [add_pos (`Var q')])),
                                   `Unknown, None))],
                   add_pos e), t
         | `Give ((c, Some (`Output (_t, s), give_tyargs)), Some e, p) ->
            let envs = o#backup_envs in
            let o = {< var_env = TyEnv.bind (o#get_var_env ()) (c, s) >} in
            let (o, e, _typ) = o#phrase e in
            let (o, p, t) = desugar_cp o p in
            let o = o#restore_envs envs in
            let q = QualifiedName.of_name "send" in
            let q' = QualifiedName.of_name c in
            o, `Block
                  ([add_pos (`Val ([], add_pos (`Variable (c, Some s, pos)),
                                   add_pos (`FnAppl (add_pos (Sugartypes.tappl (`Var q, give_tyargs)),
                                                     [e; add_pos (`Var q')])),
                                   `Unknown, None))],
                   add_pos p), t
         | `GiveNothing ((c, Some t, _)) ->
            let q = QualifiedName.of_name c in
            o, `Var q, t
         | `Select ((c, Some s, _), label, p) ->
            let envs = o#backup_envs in
            let o = {< var_env = TyEnv.bind (o#get_var_env ()) (c, TypeUtils.select_type label s) >} in
            let (o, p, t) = desugar_cp o p in
            let o = o#restore_envs envs in
            let q = QualifiedName.of_name c in
            o, `Block
                 ([add_pos (`Val ([], add_pos (`Variable (c, Some (TypeUtils.select_type label s), pos)),
                                  add_pos (`Select (label, (add_pos (`Var q)))),
                                  `Unknown, None))],
                  add_pos p), t
         | `Offer ((c, Some s, _), cases) ->
            let desugar_branch (label, p) (o, cases) =
              let envs = o#backup_envs in
              let o = {< var_env = TyEnv.bind (o#get_var_env ()) (c, TypeUtils.choice_at label s) >} in
              let (o, p, t) = desugar_cp o p in
              let pat : pattern = add_pos (`Variant (label, Some (add_pos (`Variable (c, Some (TypeUtils.choice_at label s), pos))))) in
              o#restore_envs envs, ((pat, add_pos p), t) :: cases in
            let (o, cases) = List.fold_right desugar_branch cases (o, []) in
            (match List.split cases with
                | (_, []) -> assert false (* Case list cannot be empty *)
                | (cases, t :: _ts) ->
                   let q = QualifiedName.of_name c in
                    o, `Offer (add_pos (`Var q),
                               cases,
                               Some t), t)
         | `Link ((c, Some ct, _), (d, Some _dt, _)) ->
            let (q, q', q'') = QualifiedName.(of_name "linkSync", of_name c, of_name d) in
            o, `FnAppl (add_pos (Sugartypes.tappl (`Var q, [`Type ct; `Row o#lookup_effects])),
                        [add_pos (`Var q'); add_pos (`Var q'')]), Types.make_endbang_type
         | `Comp ((c, Some s, _), left, right) ->
            let envs = o#backup_envs in
            let (o, left, _typ) = desugar_cp {< var_env = TyEnv.bind (o#get_var_env ()) (c, s) >} left in
            let (o, right, t) = desugar_cp {< var_env = TyEnv.bind (o#get_var_env ()) (c, Types.dual_type s) >} right in
            let o = o#restore_envs envs in
            let left_block =
              let (q, q', q'') = QualifiedName.(of_name "accept", of_name "close", of_name c) in
              add_pos (`Spawn (`Angel, `NoSpawnLocation, add_pos (`Block ([add_pos (`Val ([], add_pos (`Variable (c, Some s, pos)),
                                                                                          add_pos (`FnAppl (add_pos (`Var q), [add_pos (`Var q'')])),
                                                                                          `Unknown, None));
                                                            add_pos (`Val ([], add_pos (`Variable (c, Some Types.make_endbang_type, pos)),
                                                                            add_pos left,
                                                                            `Unknown, None))],
                                                            add_pos (`FnAppl (add_pos (`Var q'),
                                                                              [add_pos (`Var q'')])))),
                                              Some (Types.make_singleton_closed_row ("wild", `Present Types.unit_type)))) in
            let o = o#restore_envs envs in
            let (q, q', q'') = QualifiedName.(of_name "new", of_name "request", of_name c) in
            o, `Block
                  ([add_pos (`Val ([], add_pos (`Variable (c, Some (`Application (Types.access_point, [`Type s])), pos)),
                                   add_pos (`FnAppl (add_pos (`Var q), [])),
                                   `Unknown, None));
                    add_pos (`Val ([], add_pos (`Any), left_block, `Unknown, None));
                    add_pos (`Val ([], add_pos (`Variable (c, Some (Types.dual_type s), pos)),
                                   add_pos (`FnAppl (add_pos (`Var q'), [add_pos (`Var q'')])),
                                   `Unknown, None))],
                   add_pos right), t
         | _ -> assert false in
       desugar_cp o p
    | e -> super#phrasenode e
end

let desugar_cp env = ((new desugar_cp env) : desugar_cp :> TransformSugar.transform)
