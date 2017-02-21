(*
 * Desugars the various AP AST nodes into a plain application of newAP.
 * Very much subject to change as I refine the location API...
 *
 * Should this be done after typing? If so, how to construct the type?
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

let desugar_aps_inner =
object(self)
  inherit SugarTraversals.map as super

  method! phrase : Sugartypes.phrase -> Sugartypes.phrase = function
    | (`NewAP (`ExplicitSpawnLocation p), pos) ->
        let p = self#phrase p in
        `FnAppl ((`Var "newAP", pos), [p]), pos
    | `NewAP (`SpawnClient), pos ->
        let loc_phr = (`FnAppl (((`Var "there"), pos), [(`TupleLit [], pos)]), pos) in
        `FnAppl ((`Var "newAP", pos), [loc_phr]), pos
    | `NewAP (`NoSpawnLocation), pos ->
        let loc_phr = (`FnAppl ((`Var "here", pos), [(`TupleLit [], pos)]), pos) in
        `FnAppl ((`Var "newAP", pos), [loc_phr]), pos
    | x -> super#phrase x
end

let desugar_aps prog =
 (* Printf.printf "before AP desugar: %s\n" (Sugartypes.Show_program.show prog); *)
  let res = desugar_aps_inner#program prog in
  Printf.printf "after AP desugar: %s\n" (Sugartypes.Show_program.show res);
  res

