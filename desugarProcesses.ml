open Utility
module Types = Types_links
open Sugartypes

(*
   spawn {e}
 -->
   spawn (fun () {e})

   spawnWait {e}
 -->
   spawnWait (fun () {e})
*)


let dp = Sugartypes.dummy_position

class desugar_processes env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) = function
    | `Spawn (`Wait, body, Some inner_eff) ->
        (* bring the inner effects into scope, then restore the
           outer effects afterwards *)

        let outer_eff = o#lookup_effects in
        let o = o#with_effects inner_eff in
        let (o, body, body_type) = o#phrase body in
        let o = o#with_effects outer_eff in

        let e : phrasenode =
          `FnAppl
            ((`TAppl ((`Var "spawnWait", dp), [`Row inner_eff; `Type body_type; `Row outer_eff]), dp),
             [(`FunLit (Some [(Types.make_tuple_type [], inner_eff)], `Unl, ([[]], body), `Unknown), dp)])
        in
          (o, e, body_type)
    | `Spawn (k, body, Some inner_eff) ->
        (* bring the inner effects into scope, then restore the
           outer effects afterwards *)
        let process_type = `Application (Types.process, [`Row inner_eff]) in

        let outer_eff = o#lookup_effects in
        let o = o#with_effects inner_eff in
        let (o, body, body_type) = o#phrase body in
        let o = o#with_effects outer_eff in

        let spawn_fun =
          match k with
          | `Demon -> "spawn"
          | `Angel -> "spawnAngel"
          | `Wait  -> assert false in

        let e : phrasenode =
          `FnAppl
            ((`TAppl ((`Var spawn_fun, dp), [`Row inner_eff; `Type body_type; `Row outer_eff]), dp),
             [(`FunLit (Some [(Types.make_tuple_type [], inner_eff)], `Unl, ([[]], body), `Unknown), dp)])
        in
          (o, e, process_type)
    | `Receive (cases, Some t) ->
        let fields, row_var, false = o#lookup_effects in
        let other_effects = StringMap.remove "hear" (StringMap.remove "wild" fields), row_var, false in
          begin
            match StringMap.find "hear" fields with
              | (`Present mbt) ->
                  o#phrasenode
                    (`Switch
                       ((`FnAppl
                           ((`TAppl ((`Var "recv", dp), [`Type mbt; `Row other_effects]), dp),
                            []), dp),
                        cases,
                        Some t))
              | _ -> assert false
        end
    | e -> super#phrasenode e
end

let desugar_processes env = ((new desugar_processes env) : desugar_processes :> TransformSugar.transform)

let has_no_processes =
object
  inherit SugarTraversals.predicate as super

  val has_no_processes = true
  method satisfied = has_no_processes

  method phrasenode = function
    | `Spawn _
    | `Receive _ -> {< has_no_processes = false >}
    | e -> super#phrasenode e
end
