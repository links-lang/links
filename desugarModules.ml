(*
 * Desugars modules into plain binders.
 * Bindingnode -> [Bindingnode]
 *
 * module Foo {
 *    val bobsleigh = ...;
 *    fun x() {
 *    }
 *
 *    module Bar {
 *      fun y() {
 *      }
 *    }
 * }
 * val X = ...;
 *
 *  --->
 *
 * val Foo.bobsleigh = ...;
 * fun Foo.x() { ...}
 * fun Foo.Bar.y() { ... }
 * val x = ...;
 *
*)
open Utility
open Sugartypes
open Printf

(* Reference tree node: name of module, set of names in the module, list of
 * submodules *)
type reference_tree_node = (name * string Set.t * (name, reference_tree_node) Hashtbl.t)

let empty_tree name = (name, Set.empty, Hashtbl.create 50)

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


(* Build an environment mapping module prefixes to (Plain |-> Qualified)
 * environments.
 *
 * Needs to be a separate pass as we need to collect all names within the
 * module prior to the renaming pass.
 *
 *)
let build_env module_name node =
object(self)
  inherit SugarTraversals.fold as super
  val tree_node = node

  method add plain_name =
    let (module_name, names, children) = tree_node in
    {< tree_node = (module_name, StringSet.add plain_name names, children) >}

  method add_child child_name children =
    let (module_name, names, children) = tree_node in
    Hashtbl.add child_name children;
    self

  method get_tree = tree_node

  method phrasenode = function
    | `Var name -> add name
    | x -> super#phrasenode x

  method binder : binder -> binder = function
    | (name, dt, pos) -> add name

  method bindingnode : bindingnode -> bindingnode = function
    | `Module (child_name, block) ->
        let child_node =
          (* Recursive step: generate a reference tree node for module *)
          ((build_env name (empty_tree name))#binding block)#get_tree in
        add_child child_name child_node
    | x -> super#bindingnode x
end

let generateReferenceTree prog =
  ((build_env prog (empty_tree ""))#program prog)#get_tree


let getSubstFor module_name open_modules reference_tree plain_var_name =
  let splitPath path = Str.split "\." path in

  (* Checks whether the module at the given path contains plain_var_name *)
  let rec isContainedIn (_, vars, tbl) split_path = (
     match split_path with
       | [] -> Set.mem plain_var_name vars (* TODO: Handle qualified variables here *)
       | m::ms ->
           let child_node = Hashtbl.find tbl m in
           isContainedIn child_node tbl) in
  (* For the current module, and all open modules, check whether the
   * plain name is contained there. *)
  List.filter (fun p ->
    let split_p = splitPath p in
    isContainedIn reference_tree 

(* Add module prefix to all internal binder names and variables *)
let rec add_module_prefix prefix =
object(self)
  inherit SugarTraversals.map as super

  method phrasenode : phrasenode -> phrasenode = function
    | `Var old_name ->
        (* Add prefix onto var name*)
        let new_name =
          if prefix == "" then old_name else prefix ^ "." ^ old_name in
        (`Var new_name)
    | x -> super#phrasenode x

  method binder : binder -> binder = function
    | (old_name, dt, pos) ->
        let new_name =
          if prefix == "" then old_name else prefix ^ "." ^ old_name in
        (new_name, dt, pos)

  method bindingnode : bindingnode -> bindingnode = function
    | `Module (name, (`Block (bindings, dummy_phrase), pos)) ->
        (* Recursively rename everything in the phrase *)
        let new_prefix =
          if prefix == "" then name else prefix ^ "." ^ name in
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


