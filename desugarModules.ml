(*
 * Desugars modules into plain binders.
 * Bindingnode -> [Bindingnode]
 *
 * module Foo {
 *    val bobsleigh = ...;
 *    fun X() {
 *    }
 *
 *    module Bar {
 *      fun Y() {
 *      }
 *    }
 * }
 * val X = ...;
 *
 *  --->
 *
 * val Foo.bobsleigh = ...;
 * fun Foo.X() { ...}
 * fun Foo.Bar.Y() { ... }
 * val X = ...;
 *
*)
open Utility
open Sugartypes
open Printf

(* Flatten modules out. By this point the renaming will already have
 * happened.
 *)
let flatten_modules =
object(self)
  inherit SugarTraversals.fold as super

  val bindings = []
  method add_binding x = {< bindings = x :: bindings >}
  method get_bindings = List.rev bindings

  method binding = function
    | (`Module (_, (`Block (bindings, _), _)), _) ->
        (* We'll reverse the whole list later, so need to add
         * these bindings reversed first *)
        List.fold_left (fun acc binding -> acc#binding binding) self (List.rev bindings)
    | (x, pos) -> self#add_binding (x, pos)
end


let performFlattening : program -> program =
  fun programToFlatten ->
    let (bindings, phrase) = programToFlatten in
    let flattened_bindings =
      (flatten_modules#program programToFlatten)#get_bindings in
    (flattened_bindings, phrase)

(* Add module prefix to all internal binder names and variables *)
let rec add_module_prefix prefix =
object(self)
  inherit SugarTraversals.map as super

  method phrasenode : phrasenode -> phrasenode = function
    | `Var old_name ->
        (* Add prefix onto var name*)
        let new_name = prefix ^ old_name in
        (`Var new_name)
    | x -> super#phrasenode x

  method binder : binder -> binder = function
    | (old_name, dt, pos) ->
        let new_name = prefix ^ old_name in
        (new_name, dt, pos)

  method bindingnode : bindingnode -> bindingnode = function
    | `Module (name, (`Block (bindings, dummy_phrase), pos)) ->
        (* Recursively rename everything in the phrase *)
        let new_prefix = prefix ^ "." ^ name in
        let renamed_bindings =
          List.map (fun binding -> (add_module_prefix new_prefix)#binding binding) bindings in
          (* self#list (fun o -> o#binding) bindings in *)
        `Module (new_prefix, (`Block (renamed_bindings, dummy_phrase), pos))
    | x -> super#bindingnode x

  method program = function
    | (bindings, body) ->
        let renamed_bindings = self#list (fun o -> o#binding) bindings in
        let renamed_body = self#option (fun o -> o#phrase) body in
        (renamed_bindings, renamed_body)

end

let performRenaming : program -> program =
  (add_module_prefix "")#program

(* 1) Perform a renaming pass to expand names in modules to qualified names
 * 2) Peform a flattening pass to flatten modules to lists of bindings
 *)
let desugar_modules program =
  let renamedProgram = performRenaming program in
  let (renamedBindings, renamedBody) = renamedProgram in
  let flattenedProgram = performFlattening renamedProgram in
  flattenedProgram


