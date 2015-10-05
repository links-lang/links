(*pp deriving *)

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
type reference_tree_node =
    ReferenceTreeNode of (name * stringset * (name, reference_tree_node) Hashtbl.t)

let empty_tree name = `ReferenceTreeNode(name, StringSet.empty, Hashtbl.create 50)

let rec print_tree = function
  | `ReferenceTreeNode(n, ss, ht) ->
      printf "Module Name: %s\n, variable names: %s\n"
          n (StringSet.fold (fun x acc -> acc ^ " " ^ x) ss "");
      Hashtbl.iter(fun x y -> print_tree y) ht


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
let rec build_env module_name node =
object(self)
  inherit SugarTraversals.fold as super
  val tree_node = node

  method add plain_name =
    let `ReferenceTreeNode (module_name, names, children) = tree_node in
    {< tree_node = `ReferenceTreeNode (module_name, StringSet.add plain_name names, children) >}

  method add_child child_name new_child =
    let `ReferenceTreeNode (module_name, names, children) = tree_node in
    Hashtbl.add children child_name new_child;
    self

  method get_tree = tree_node

  method phrasenode = function
    | `Var name -> self#add name
    | x -> super#phrasenode x

  method binder = function
    | (name, dt, pos) -> self#add name

  method bindingnode = function
    | `Module (child_name, block) ->
        let child_node =
          (* Recursive step: generate a reference tree node for module *)
          ((build_env child_name (empty_tree child_name))#phrase block)#get_tree in
        self#add_child child_name child_node
    | `Fun (b, _, _, _, _) -> self#binder b
    | x -> super#bindingnode x
end

let generateReferenceTree prog =
  ((build_env "" (empty_tree ""))#program prog)#get_tree


let checkSubsts plain_var_name poss_substs pos =
  (* TODO: Is there a "proper" way to report errors when not in typeSugar? *)
  let rec print_list = function
        | [] -> ""
        | e::[] -> e
        | e::xs -> e ^ ", " ^ (print_list xs) in
  let printed_list xs = "[" ^ (print_list xs) ^ "]" in

  match poss_substs with
    | [] -> plain_var_name
    | [subst_name] -> subst_name
    | xs -> failwith ("Name " ^ plain_var_name ^
      " is ambiguous. Possible options: " ^ printed_list xs ^ ".")

let getSubstFor module_name open_modules reference_tree plain_var_name var_pos =
  let splitPath path = Str.split (Str.regexp "\\.") path in

  (* Checks whether the module at the given path contains plain_var_name *)
  let rec isContainedIn ((`ReferenceTreeNode (_, vars, tbl)) as rtn) split_path = (
     match split_path with
       | [] -> StringSet.mem plain_var_name vars (* FIXME: Handle qualified variables here *)
       | m::ms ->
           let child_node = Hashtbl.find tbl m in
           isContainedIn child_node ms) in
  (* For the current module, and all open modules, check whether the
   * plain name is contained there. *)
  let substs = ListUtils.filter_map (fun p ->
    let split_p = splitPath p in
    isContainedIn reference_tree split_p)
      (fun p -> if p = "" then plain_var_name else p ^ "." ^ plain_var_name)
      (module_name::open_modules) in
  checkSubsts plain_var_name substs var_pos


(* Add module prefix to all internal binder names and variables *)
let rec add_module_prefix prefix reference_tree =
object(self)
  inherit SugarTraversals.map as super

  method phrase : phrase -> phrase = function
    | (`Var old_name, pos) ->
        (* Add prefix onto var name*)
        let new_name =
          (* TEMP until we get Open working *)
          getSubstFor prefix [] reference_tree old_name pos in
        (`Var new_name, pos)
    | x -> super#phrase x


  method binder : binder -> binder = function
    | (old_name, dt, pos) ->
        let new_name =
          if prefix = "" then old_name else prefix ^ "." ^ old_name in
        (new_name, dt, pos)

  method bindingnode : bindingnode -> bindingnode = function
    | `Module (name, (`Block (bindings, dummy_phrase), pos)) ->
        (* Recursive step: change current path, recursively rename modules *)
        let new_prefix =
          if prefix = "" then name else prefix ^ "." ^ name in
        let renamed_bindings =
          List.map (fun binding ->
            (add_module_prefix new_prefix reference_tree)#binding binding) bindings in
        `Module (new_prefix, (`Block (renamed_bindings, dummy_phrase), pos))
    | x -> super#bindingnode x

  method program = function
    | (bindings, body) ->
        let renamed_bindings = self#list (fun o -> o#binding) bindings in
        let renamed_body = self#option (fun o -> o#phrase) body in
        (renamed_bindings, renamed_body)

end

let performRenaming prog =
  let ref_tree = generateReferenceTree prog in
  (* print_tree ref_tree; *)
  (add_module_prefix "" ref_tree)#program prog

(* 1) Perform a renaming pass to expand names in modules to qualified names
 * 2) Peform a flattening pass to flatten modules to lists of bindings
 *)
let desugar_modules program =
  let renamedProgram = performRenaming program in
  let (renamedBindings, renamedBody) = renamedProgram in
  let flattenedProgram = performFlattening renamedProgram in
  flattenedProgram


