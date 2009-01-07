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

  method phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) = function
    | `Spawn (body, Some inner_eff) ->
        (* bring the inner mailbox type into scope, then restore the
           outer mailbox type afterwards *)
        let mbt, inner_mb = 
          let fields, _ = inner_eff in
          let _, t = StringMap.find "hear" fields in
            t, `Application (Types.mailbox, [`Type t]) in
        let outer_eff = o#lookup_effects in
        let o = o#with_effects inner_eff in
        let (o, body, body_type) = o#phrase body in
        let o = o#with_effects outer_eff in

        let e : phrasenode =
          `FnAppl
            ((`TAppl ((`Var "spawn", dp), [`Type mbt; `Type body_type; `Row outer_eff]), dp),
             [(`FunLit (Some [(Types.make_tuple_type [], inner_eff)], ([[]], body)), dp)])
        in
          (o, e, inner_mb)
    | `SpawnWait (body, Some inner_eff) ->
        (* bring the inner mailbox type into scope, then restore the
           outer mailbox type afterwards *)

        let mbt = 
          let fields, _ = inner_eff in
          let _, t = StringMap.find "hear" fields in
            t in

        let outer_eff = o#lookup_effects in
        let o = o#with_effects inner_eff in
        let (o, body, body_type) = o#phrase body in
        let o = o#with_effects outer_eff in
          
        let e : phrasenode =
          `FnAppl
            ((`TAppl ((`Var "spawnWait", dp), [ `Type mbt; `Type body_type; `Row outer_eff]), dp),
             [(`FunLit (Some [(Types.make_tuple_type [], inner_eff)], ([[]], body)), dp)])
        in
          (o, e, body_type)
    | `Receive (cases, Some t) ->
        let fields, _ = o#lookup_effects in
          begin
            match StringMap.find "hear" fields with
              | (`Present, mbt) ->
                  o#phrasenode
                    (`Switch
                       ((`FnAppl
                           ((`TAppl ((`Var "recv", dp), [`Type mbt]), dp),
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
    | `SpawnWait _
    | `Receive _ -> {< has_no_processes = false >}
    | e -> super#phrasenode e
end
