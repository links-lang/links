open Utility
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

  method! phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) = function
    | `Spawn (`Wait, spawn_loc, body, Some inner_eff) ->
        assert (spawn_loc = `NoSpawnLocation);
        (* bring the inner effects into scope, then restore the
           outer effects afterwards *)

        let outer_eff = o#lookup_effects in
        let o = o#with_effects inner_eff in
        let (o, body, body_type) = o#phrase body in
        let o = o#with_effects outer_eff in

        let e : phrasenode =
          let q = QualifiedName.of_name "spawnWait" in
          `FnAppl
            ((`TAppl ((`Var q, dp), [`Row inner_eff; `Type body_type; `Row outer_eff]), dp),
             [(`FunLit (Some [(Types.make_tuple_type [], inner_eff)], `Unl, ([[]], body), `Unknown), dp)])
        in
          (o, e, body_type)
    | `Spawn (k, spawn_loc, body, Some inner_eff) ->
        (* bring the inner effects into scope, then restore the
           outer effects afterwards *)
        let process_type = `Application (Types.process, [`Row inner_eff]) in

        let outer_eff = o#lookup_effects in
        let o = o#with_effects inner_eff in
        let (o, spawn_loc) = o#given_spawn_location spawn_loc in
        let (o, body, body_type) = o#phrase body in
        let o = o#with_effects outer_eff in

        let spawn_loc_phr =
          let (q, q') = QualifiedName.(of_name "there", of_name "here") in
          match spawn_loc with
            | `ExplicitSpawnLocation phr -> phr
            | `SpawnClient -> (`FnAppl ((`Var q, dp), [(`TupleLit [], dp)]), dp)
            | `NoSpawnLocation -> (`FnAppl ((`Var q', dp), [(`TupleLit [], dp)]), dp) in

        let spawn_fun =
          match k with
          | `Demon  -> "spawnAt"
          | `Angel  -> "spawnAngelAt"
          | `Wait   -> assert false in

        (* At this point, the location in the funlit doesn't matter -- we'll have an explicit
         * location in the form of spawn_loc_phr. It was useless before anyway, given that it
         * corresponded to the spawn type. *)

        let e : phrasenode =
          let q = QualifiedName.of_name spawn_fun in
          `FnAppl
            ((`TAppl ((`Var q, dp), [`Row inner_eff; `Type body_type; `Row outer_eff]), dp),
             [(`FunLit (Some [(Types.make_tuple_type [], inner_eff)], `Unl, ([[]], body), `Unknown), dp);
              spawn_loc_phr])
        in
          (o, e, process_type)
    | `Receive (cases, Some t) ->
        let fields, row_var, _ = o#lookup_effects in
        let other_effects = StringMap.remove "hear" (StringMap.remove "wild" fields), row_var, false in
          begin
            match StringMap.find "hear" fields with
            | (`Present mbt) ->
               let q = QualifiedName.of_name "recv" in
                  o#phrasenode
                    (`Switch
                       ((`FnAppl
                           ((`TAppl ((`Var q, dp), [`Type mbt; `Row other_effects]), dp),
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

  method! phrasenode = function
    | `Spawn _
    | `Receive _ -> {< has_no_processes = false >}
    | e -> super#phrasenode e
end
