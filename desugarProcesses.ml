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

class desugar_processes {Types.var_env=var_env; Types.tycon_env=tycon_env} =
object (o : 'self_type)
  inherit (TransformSugar.transform (var_env, tycon_env)) as super

  method phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) = function
    | `Spawn (body, Some inner_mbt) ->
        (* bring the inner mailbox type into scope, then restore the
           outer mailbox type afterwards *)
        let outer_mbt = o#lookup_mb () in
        let o = o#with_mb inner_mbt in
        let (o, body, body_type) = o#phrase body in
        let o = o#with_mb outer_mbt in

        let e : phrasenode =
          `FnAppl
            ((`TAppl ((`Var "spawn", dp), [`Type inner_mbt; `Type outer_mbt; `Type body_type]), dp),
             [(`FunLit (Some [(Types.unit_type, inner_mbt)], ([[]], body)), dp)])
        in
          (o, e, inner_mbt)
    | `SpawnWait (body, Some inner_mbt) ->
        (* bring the inner mailbox type into scope, then restore the
           outer mailbox type afterwards *)
        let outer_mbt = o#lookup_mb () in
        let o = o#with_mb inner_mbt in
        let (o, body, body_type) = o#phrase body in
        let o = o#with_mb outer_mbt in
          
        let e : phrasenode =
          `FnAppl
            ((`TAppl ((`Var "spawnWait", dp), [ `Type body_type; `Type outer_mbt; `Type inner_mbt]), dp),
             [(`FunLit (Some [(Types.unit_type, inner_mbt)], ([[]], body)), dp)])
        in
          (o, e, body_type)
    | `Receive (cases, Some t) ->
        o#phrasenode
          (`Switch
             ((`FnAppl
                 ((`TAppl ((`Var "recv", dp), [`Type (o#lookup_mb ())]), dp),
                  []), dp),
              cases, 
              Some t))
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
