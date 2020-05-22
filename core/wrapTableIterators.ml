open Utility
open CommonTypes
open Sugartypes
open SourceCode
open SourceCode.WithPos

(*
 * Ensures that table iterators are wrapped in a query block:
 *
 * for (x <-- y) M
 *  --->
 * query { for (x <-- y) M }
 *
 * Note that this is safe if we're in another query block:
 *
 * query { for (x <-- y) M }
 *  --->
 * query { query { for (x <-- y) M } }
 *
 * Since nested query blocks are removed by the normaliser.
*)
class wrap_iterators env =
  let open TransformSugar in
  object (o : 'self_type)
    inherit (TransformSugar.transform env) as super

    method! phrase = function
      | { node = Iteration (gens, body, cond, orderby); pos } ->
          let wp = WithPos.make ~pos in
          let envs = o#backup_envs in
          let (o, gens) = listu o (fun o -> o#iterpatt) gens in
          let (o, body, t) = o#phrase body in
          let (o, cond, _) = option o (fun o -> o#phrase) cond in
          let (o, orderby, _) = option o (fun o -> o#phrase) orderby in
          let o = o#restore_envs envs in
          let is_query = List.exists
              (function
                 | List  _ -> false
                 | Table _ -> true) gens in
          let node =
            if is_query then
              Query (None, QueryPolicy.Default,
                wp (Iteration (gens, body, cond, orderby)), Some t)
            else
              Iteration (gens, body, cond, orderby) in
          (o, wp node, t)
      | p -> super#phrase p
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
