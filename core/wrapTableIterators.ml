open Utility
open CommonTypes
open Sugartypes
(*
 * Ensures that table iterators are wrapped in a query block:
 *
 * for (x <-- y) M
 *  --->
 * query { for (x <-- y) M }
 *
 * Note that since we can't "break out" of a query block, we
 * do not have to perform the pass inside query blocks.
 *)
class wrap_iterators env =
  let open TransformSugar in
  object (o : 'self_type)
    inherit (TransformSugar.transform env) as super

    method! phrasenode = function
      | Query (_, _, _, Some t) as q ->
          (* We don't need to perform this pass inside a query block.
           * Indeed, if we do, it results in nontermination. *)
          (o, q, t)
      | Iteration (gens, body, cond, orderby) ->
          let envs = o#backup_envs in
          let dp = SourceCode.WithPos.dummy in
          let (o, gens) = listu o (fun o -> o#iterpatt) gens in
          let (o, body, t) = o#phrase body in
          let (o, cond, _) = option o (fun o -> o#phrase) cond in
          let (o, orderby, _) = option o (fun o -> o#phrase) orderby in
          let is_query = List.exists
              (function
                 | List  _ -> false
                 | Table _ -> true) gens in
          let iter = Iteration (gens, body, cond, orderby) in
          let node =
            if is_query then
              Query (None, QueryPolicy.Default, dp iter, Some t)
            else
              iter in
          let o = o#restore_envs envs in
          (o, node, t)
      | p -> super#phrasenode p
  end


let wrap_iterators env =
  ((new wrap_iterators env) : wrap_iterators :> TransformSugar.transform)

module Typeable
  = Transform.Typeable.Make(struct
        let name = "wrap table iterators"
        let obj env =
          (wrap_iterators env : TransformSugar.transform :>
            Transform.Typeable.sugar_transformer)
      end)
