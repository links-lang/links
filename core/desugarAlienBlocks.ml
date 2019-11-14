(* Desugars "alien modules" into plain modules containing alien declarations.
 * alien javascript "test.js" {
 *   setTitle : (String) ~> ()
 *   alertBox : (String) ~> ()
 * }
 *
 * --->
 *
 * alien javascript "test.js" setTitle : (String) ~> ()
 * alien javascript "test.js" alertBox : (String) ~> ()
 *)

open Utility
open SourceCode.WithPos
open Sugartypes
open SugarConstructors.DummyPositions

let rec flatten_simple = fun () ->
object(self)
  inherit SugarTraversals.map as super

  method! phrasenode : phrasenode -> phrasenode = function
    | Block (bs, phr) ->
        let flattened_bindings =
          List.concat (
            List.map (fun b -> ((flatten_bindings ())#binding b)#get_bindings) bs
          ) in
        let flattened_phrase = self#phrase phr in
        Block (flattened_bindings, flattened_phrase)
    | x -> super#phrasenode x
end
and flatten_bindings = fun () ->
object(self)
  inherit SugarTraversals.fold

  val bindings = []
  method add_binding x = {< bindings = x :: bindings >}
  method get_bindings = List.rev bindings

  method! binding = function
    | {node=AlienBlock alien; _} ->
       self#list
         (fun o ((bnd, dt)) ->
           let alien =
             Foreign (Alien.single
                        (Alien.language alien)
                        (Alien.object_file alien)
                        bnd dt)
           in
           o#add_binding (with_dummy_pos alien))
      (Alien.declarations alien)
    | {node=Module ({ members; _ } as module') ; _} ->
        let flattened_bindings =
          List.concat (
            List.map (fun b -> ((flatten_bindings ())#binding b)#get_bindings) members
          ) in
        self#add_binding (with_dummy_pos (Module { module' with members = flattened_bindings }))
    | b -> self#add_binding ((flatten_simple ())#binding b)

  method! program = function
    | (bindings, _body) -> self#list (fun o -> o#binding) bindings
end

let flatten_prog prog =
  let (_, phr) = prog in
  let o = (flatten_bindings())#program prog in
  (o#get_bindings, phr)

let transform_alien_blocks = flatten_prog

(* This has the same problem as the above (c.f. issue #604). *)
let sentence = function
  | Definitions _ as sentence ->
     let o = (flatten_bindings ())#sentence sentence in
     Definitions (o#get_bindings)
  | sentence -> sentence

