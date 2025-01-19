open Utility
open CommonTypes
open Sugartypes
open SugarConstructors.DummyPositions

(*
   spawn {e}
 -->
   spawn (fun () {e})

   spawnWait {e}
 -->
   spawnWait (fun () {e})
*)


class desugar_processes env =
let open PrimaryKind in
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method! phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) = function
    | Spawn (Wait, spawn_loc, body, Some inner_eff) ->
        assert (spawn_loc = NoSpawnLocation);
        (* bring the inner effects into scope, then restore the
           outer effects afterwards *)

        let outer_eff = o#lookup_effects in
        let o = o#with_effects inner_eff in
        let (o, body, body_type) = o#phrase body in
        let o = o#with_effects outer_eff in

        let e : phrasenode =
          fn_appl_node "spawnWait" [(Row, inner_eff); (Type, body_type); (Row, outer_eff)]
            [fun_lit ~args:[(Types.make_tuple_type [], inner_eff)] dl_unl [[]] body]
        in
          (o, e, body_type)
    | Spawn (k, spawn_loc, body, Some inner_eff) ->
        (* bring the inner effects into scope, then restore the
           outer effects afterwards *)
        let process_type = Types.Application (Types.process, [(PrimaryKind.Row, inner_eff)]) in
        let fun_effects = Types.row_with Types.wild_present inner_eff in
        let fun_effects =
          if Settings.get Basicsettings.Sessions.exceptions_enabled then
            let ty = Types.make_pure_function_type [] (Types.empty_type) in
            Types.row_with (Value.session_exception_operation, Types.Present ty) fun_effects
          else fun_effects
        in

        let outer_eff = o#lookup_effects in
        let o = o#with_effects inner_eff in
        let (o, spawn_loc) = o#given_spawn_location spawn_loc in
        let (o, body, body_type) = o#phrase body in
        let o = o#with_effects outer_eff in

        let spawn_loc_phr =
          match spawn_loc with
            | ExplicitSpawnLocation phr -> phr
            | SpawnClient -> fn_appl "there" [(Row, outer_eff)] []
            | NoSpawnLocation -> fn_appl "here" [(Row, outer_eff)] [] in

        let spawn_fun =
          match k with
          | Demon  -> "spawnAt"
          | Angel  -> "spawnAngelAt"
          | Wait   -> assert false in

        (* At this point, the location in the funlit doesn't matter -- we'll have an explicit
         * location in the form of spawn_loc_phr. It was useless before anyway, given that it
         * corresponded to the spawn type. *)

        let e : phrasenode =
          fn_appl_node spawn_fun [(Row, inner_eff); (Type, body_type); (Row, outer_eff)]
             [spawn_loc_phr;
              fun_lit ~args:[(Types.make_tuple_type [], fun_effects)] dl_lin [[]] body]
        in
          (o, e, process_type)
    | Receive (cases, Some t) ->
        let (fieldenv, rho, _) = TypeUtils.extract_row_parts (o#lookup_effects) in
        let other_effects =
          Types.(remove_field hear (remove_field wild (Row (fieldenv, rho, false))))
        in
        begin
          match StringMap.find Types.hear fieldenv with
          | (Types.Present mbt) ->
             o#phrasenode
               (Switch (fn_appl "recv" [(Type, mbt); (Row, other_effects)] [],
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
    | Spawn _
    | Receive _ -> {< has_no_processes = false >}
    | e -> super#phrasenode e
end

module Typeable
  = Transform.Typeable.Make(struct
        let name = "processes"
        let obj env = (desugar_processes env : TransformSugar.transform :> Transform.Typeable.sugar_transformer)
      end)
