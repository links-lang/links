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


(* Flatten modules out. By this point the renaming will already have
 * happened.
 *)
let flatten_modules name =
object(self)
  inherit SugarTraversals.fold as super

  val bindings = []
  method add_binding x = {< bindings = x :: bindings >}
  method add_bindings xs = {< bindings = xs @ bindings >}

  method bindingnode = function
    | `Module (name, (`Block (bindings, p))) ->
        


(* Add module prefix to all internal binder names and variables *)
let add_module_prefix prefix =
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
    | `Module (name, phrase) ->
        (* Recursively rename everything in the phrase *)
        let new_prefix = prefix ^ "." ^ name in
        `Module (name, addModulePrefixToPhrase prefix phraseToModify)
    | x -> super#bindingnode x

let addModulePrefix prefix bindingToModify =
  (add_module_prefix prefix)#binding bindingToModify

let addModulePrefixToPhrase prefix phraseToModify =
  (add_module_prefix prefix)#phrase phraseToModify
  (* Recursive step on  
  method bindingnode : bindingnode -> bindingnode = function
    | `Module 

let desugar_modules =
object(self)
  inherit SugarTraversals.map as super
