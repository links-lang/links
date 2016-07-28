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
 * val x = ...;
 *
 *  --->
 *
 * val Foo:::bobsleigh = ...;
 * fun Foo:::x() { ...}
 * fun Foo:::Bar:::y() { ... }
 * val x = ...;
 *
*)
open Utility
open Sugartypes
open Printf
open ModuleUtils

(* Only `Module and `Import don't preserve structure. We can just do a map on everything
 * else... *)
let rec flatten_simple = fun () ->
object(self)
  inherit SugarTraversals.map as super

  method phrasenode : phrasenode -> phrasenode = function
    | `Block (bs, phr) ->
        let flattened_bindings =
          List.concat (
            List.map (fun b -> ((flatten_bindings ())#binding b)#get_bindings) bs
          ) in
        let flattened_phrase = self#phrase phr in
        `Block (flattened_bindings, flattened_phrase)
    | x -> super#phrasenode x
end

(* Flatten modules out. By this point the renaming will already have
 * happened.
 * Also, remove import statements (as they will have been used by the renaming
 * pass already, and we won't need them any more)
 *)
and flatten_bindings = fun () ->
object(self)
  inherit SugarTraversals.fold as super

  val bindings = []
  method add_binding x = {< bindings = x :: bindings >}
  method get_bindings = List.rev bindings

  method binding = function
    | (`Module (_, (`Block (bindings, _), _)), _) ->
        List.fold_left (fun acc binding -> acc#binding binding) self bindings
    | (`Import _, _) -> self
    | b -> self#add_binding ((flatten_simple ())#binding b)

  method program = function
    | (bindings, _body) -> self#list (fun o -> o#binding) bindings

end

let performFlattening : program -> program =
  fun programToFlatten ->
    let (bindings, phrase) = programToFlatten in
    let flattened_bindings =
      ((flatten_bindings ())#program programToFlatten)#get_bindings in
    (flattened_bindings, phrase)

(* Given a plain module name, checks whether it is in scope according to the
 * current stack of open modules *)

(* Given a module stack, a reference tree, and a plain variable name, resolves
 * the fully-qualified name according to the priority of the stack.
 * If there are no matches, leave it as it is (either FQ already or erroneous) *)
let rec substituteVar seen_bindings binding_stack current_module_prefix var_name =
  (*
  printf "Trying to substitute for %s in module %s. Seen: %s; Binding stack: %s\n"
    var_name current_module_prefix (print_list (StringSet.elements seen_bindings)) (print_stack binding_stack);
 *)
  (* printf "Trying to substitute for %s in module %s; Binding stack: %s\n"
    var_name current_module_prefix (print_stack binding_stack); *)
  match binding_stack with
    | [] -> var_name
    | (`LocalVarBinding x)::xs ->
        if x = var_name then
          prefixWith x current_module_prefix
        else
          substituteVar seen_bindings xs current_module_prefix var_name
    | (`OpenStatement x)::xs ->
        let fully_qual = prefixWith var_name x in
        if StringSet.mem fully_qual seen_bindings then
          fully_qual
        else
          substituteVar seen_bindings xs current_module_prefix var_name

(* Add module prefix to all internal binder names and variables *)
let rec add_module_prefix prefix init_seen_modules init_seen_bindings init_binding_stack =
object(self)
  inherit SugarTraversals.fold_map as super

  val seen_modules = init_seen_modules
  val seen_bindings = init_seen_bindings
  val binding_stack = init_binding_stack

  method add_seen_module name =
    {< seen_modules = StringSet.add name seen_modules >}

  method push_module name =
    {< binding_stack = (`OpenStatement name) :: binding_stack >}

  method push_var_binding name =
    {< binding_stack = (`LocalVarBinding name) :: binding_stack >}

  method add_seen_binding name =
    {< seen_bindings = StringSet.add name seen_bindings >}

  method get_binding_stack = binding_stack
  method get_seen_modules = seen_modules
  method get_seen_bindings = seen_bindings

  method set_stack s = {< binding_stack = s >}

  method phrase = function
    | (`Var old_name, pos) ->
        (* Add prefix onto var name*)
        let new_name = substituteVar seen_bindings binding_stack prefix old_name in
        (self, (`Var new_name, pos))
    | x -> super#phrase x


  method binder = function
    | (old_name, dt, pos) ->
        let new_name = prefixWith old_name prefix in
        let o = (self#push_var_binding old_name) in
        (o#add_seen_binding new_name, (new_name, dt, pos))

  method bindingnode = function
    | `Import name ->
        (* Check to see whether module is in our seen list. If so, we're golden,
         * push onto open module stack (fully-qualified).
         * If not, throw an error. *)
        (match moduleInScope seen_modules binding_stack name with
           | Some(fully_qualified_name) ->
               (self#push_module fully_qualified_name, `Import fully_qualified_name)
           | None ->
               failwith ("Trying to import unknown module " ^ name))
    | `Module (name, p) ->
        let new_prefix = prefixWith name prefix in
        (* Add fully-qualified module to seen module list *)
        (* Add fully-qualified module to module stack *)
        let o = self#add_seen_module new_prefix in
        let o1 = o#push_module new_prefix in
        let (o2, renamed_phrase) =
          (add_module_prefix new_prefix o1#get_seen_modules o1#get_seen_bindings
            ((`OpenStatement new_prefix)::binding_stack))#phrase p in
        let o3 = {< seen_modules = o2#get_seen_modules; seen_bindings = o2#get_seen_bindings >} in
        (o3, `Module (new_prefix, renamed_phrase))
        (* printf "Renamed bindings for module %s: %s\n" name (print_list
         * (List.map (Sugartypes.Show_binding.show)  (List.rev reversed_renamed_bindings))); *)
    | x -> super#bindingnode x

  method phrasenode = function
    | `Block (bs, p) ->
        (* Recursively process bindings / phrase *)
        let (o1, reversed_renamed_bindings) =
          List.fold_left (fun (o_acc, bs_acc) binding ->
            let (o_acc1, new_binding) = o_acc#binding binding in
            (o_acc1, new_binding :: bs_acc)
          ) (self, []) bs in
        (* Restore the previous binding stack by making a copy of this object w/ new
         * seen modules / bindings *)
        let (o2, new_phrase) = o1#phrase p in
        let o2 = {< seen_modules = o2#get_seen_modules; seen_bindings = o2#get_seen_bindings >} in
        (o2, `Block (List.rev reversed_renamed_bindings, new_phrase))
    | p -> super#phrasenode p
end

let performRenaming prog =
  snd ((add_module_prefix "" (StringSet.empty) (StringSet.empty) [`OpenStatement ""])#program prog)

let has_no_modules =
object
  inherit SugarTraversals.predicate as super

  val has_no_modules = true
  method satisfied = has_no_modules

  method bindingnode = function
    | `Import _
    | `Module _ -> {< has_no_modules = false >}
    | b -> super#bindingnode b
end

let requires_desugar prog = (not ((has_no_modules#program prog)#satisfied))

(* 1) Perform a renaming pass to expand names in modules to qualified names
 * 2) Peform a flattening pass to flatten modules to lists of bindings
 *)
let desugar_modules program =
  (* Chaser.print_external_deps program; *)
  if (requires_desugar program) then
    let renamedProgram = performRenaming program in
    let (renamedBindings, renamedBody) = renamedProgram in
    let flattenedProgram = performFlattening renamedProgram in
    (*
    printf "\n=============================================================\n";
    printf "\n=============================================================\n";
    printf "Before module desugar: %s\n" (Sugartypes.Show_program.show program);
    printf "\n=============================================================\n";
    printf "After module desugar: %s\n" (Sugartypes.Show_program.show flattenedProgram);
    *)
    flattenedProgram
  else program


