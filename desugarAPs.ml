(*
 * Desugars the various AP AST nodes into a plain application of newAP.
 * Very much subject to change as I refine the location API...
 *
 * new ()
 * -->
 * newAP (here)
 *
 * newClientAP ()
 * -->
 * newAP (there)
 *
 * newServerAP ()
 * -->
 * newAP (here)
 *)


class desugar_aps env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method! phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) = function
    | `NewAP (`ExplicitSpawnLocation p) -> ...
    | `NewAP (`SpawnClient) ->
        
    | `NewAP (`NoSpawnLocation) -> ...
    | x -> super#phrasenode x
