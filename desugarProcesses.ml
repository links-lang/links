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
             [(`FunLit (Some [(Types.make_tuple_type [], inner_mbt)], ([[]], body)), dp)])
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
             [(`FunLit (Some [(Types.make_tuple_type [], inner_mbt)], ([[]], body)), dp)])
        in
          (o, e, body_type)
    | `Receive (cases, Some t) ->
        (*
          ISSUE:
          
          We currently distinguish between the type of function bodies
          that definitely receive and those that are polymorphic in
          whether they receive by wrapping the type annotation on an
          arrow in a mailbox type for the former case:

            `Function (a, b, c)

          vs:
          
            `Function (a, `Mailbox b, c)

          Using Sam's proposed effect system these will become:

            `Function (a, ... Mailbox:b.d ..., c)

          and:
 
            `Function (a, ... Mailbox:b.present ..., c)

          Currently we do not print out polymorphic arrow
          annotations. This behaviour is often what we want. It isn't
          always, though, as there may be some constraint relating
          different arrow annotations.

          For instance we might want to ascribe the following type to
          the map function:

            map : forall a,b,c.(a -b-> c, [a]) -b-> [c]

          but:
  
            map : (a -> b, [a]) -> b

          could mean:

            map : forall a,b,c.(a -b-> c, [a]) -b> [c]

          or it could mean:

            map : forall a,b,c,d.(a -b-> c, [a]) -d-> [c]

          One possible solution would be to only print out polymorphic
          effect annotations when they are constrained.         
        *)
        begin
          match TypeUtils.concrete_type (o#lookup_mb ()) with
            | `Application (mb, [t]) when Types.Abstype.name mb = "Mailbox" -> 
                o#phrasenode
                  (`Switch
                     ((`FnAppl
                         ((`TAppl ((`Var "recv", dp), [`Type t]), dp),
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
