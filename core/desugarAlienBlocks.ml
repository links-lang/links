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
open Sugartypes
open SugarConstructors.Make

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
    | {node=AlienBlock (lang, lib, decls); _} ->
        self#list (fun o ((bnd, dt)) ->
          let name = name_of_binder bnd in
          o#add_binding (with_dummy_pos (Foreign (bnd, name, lang, lib, dt)))) decls
    | {node=Module (name, bindings); _} ->
        let flattened_bindings =
          List.concat (
            List.map (fun b -> ((flatten_bindings ())#binding b)#get_bindings) bindings
          ) in
        self#add_binding (with_dummy_pos (Module (name, flattened_bindings)))
    | b -> self#add_binding ((flatten_simple ())#binding b)

  method! program = function
    | (bindings, _body) -> self#list (fun o -> o#binding) bindings
end

let flatten_prog prog =
  let (_, phr) = prog in
  let o = (flatten_bindings())#program prog in
  (o#get_bindings, phr)

let transform_alien_blocks = flatten_prog

