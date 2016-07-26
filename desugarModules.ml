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

let module_sep = ":::"

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

let print_list xs =
  let rec print_list_inner = function
      | [] -> ""
      | e::[] -> e
      | e::xs -> e ^ ", " ^ (print_list_inner xs) in
  "[" ^ print_list_inner xs ^ "]"


let prefixWith name prefix =
  if prefix = "" then name else prefix ^ module_sep ^ name

(* Given a plain module name, checks whether it is in scope according to the
 * current stack of open modules *)
let rec moduleInScope seen_modules module_scope_stack module_name =
  match module_scope_stack with
    | [] ->
        if StringSet.mem module_name seen_modules then Some(module_name) else None
    | x::xs ->
        let fully_qual = prefixWith module_name x in
        if StringSet.mem fully_qual seen_modules then
          Some(fully_qual)
        else
          moduleInScope seen_modules xs module_name

(* Given a module stack, a reference tree, and a plain variable name, resolves
 * the fully-qualified name according to the priority of the stack.
 * If there are no matches, leave it as it is (either FQ already or erroneous) *)
let rec substituteVar seen_bindings module_scope_stack current_module_prefix var_name =
  match module_scope_stack with
    | [] -> var_name
    | x::xs ->
        let fully_qual = prefixWith var_name x in
        if StringSet.mem fully_qual seen_bindings then
          fully_qual
        else
          substituteVar seen_bindings xs current_module_prefix var_name



(* Add module prefix to all internal binder names and variables *)
let rec add_module_prefix prefix init_seen_modules init_seen_bindings init_module_scope_stack =
object(self)
  inherit SugarTraversals.fold_map as super

  val seen_modules = init_seen_modules
  val seen_bindings = init_seen_bindings
  val module_scope_stack = init_module_scope_stack

  method add_seen_module name =
    {< seen_modules = StringSet.add name seen_modules >}

  method push_module name =
    {< module_scope_stack = name :: module_scope_stack >}

  method add_seen_binding name =
    {< seen_bindings = StringSet.add name seen_bindings >}

  method get_module_stack = module_scope_stack
  method get_seen_modules = seen_modules
  method get_seen_bindings = seen_bindings

  method set_stack s = {< module_scope_stack = s >}

  method phrase = function
    | (`Var old_name, pos) ->
        (* Add prefix onto var name*)
        let new_name = substituteVar seen_bindings module_scope_stack prefix old_name in
        (self, (`Var new_name, pos))
    | x -> super#phrase x


  method binder = function
    | (old_name, dt, pos) ->
        let new_name = prefixWith old_name prefix in
        (self#add_seen_binding new_name, (new_name, dt, pos))

  method bindingnode = function
    | `Import name ->
        (* Check to see whether module is in our seen list. If so, we're golden,
         * push onto open module stack (fully-qualified).
         * If not, throw an error. *)
        (match moduleInScope seen_modules module_scope_stack name with
           | Some(fully_qualified_name) ->
               (self#push_module fully_qualified_name, `Import name)
           | None ->
               failwith ("Trying to import unknown module " ^ name))
    | `Module (name, (`Block (bindings, dummy_phrase), pos)) ->
        let new_prefix = prefixWith name prefix in
        (* Add fully-qualified module to seen module list *)
        (* Add fully-qualified module to module stack *)
        let o = (add_module_prefix new_prefix (StringSet.add new_prefix seen_modules)
          seen_bindings (new_prefix :: module_scope_stack)) in
        let (o1, reversed_renamed_bindings) =
          List.fold_left (fun (o_acc, bs_acc) binding ->
            let (o_acc1, new_binding) = o_acc#binding binding in
            (o_acc1, new_binding :: bs_acc)
          ) (o, []) bindings in
        (* printf "Renamed bindings for module %s: %s\n" name (print_list
         * (List.map (Sugartypes.Show_binding.show)  (List.rev reversed_renamed_bindings))); *)
        let o2 = {< seen_modules = o1#get_seen_modules; seen_bindings = o1#get_seen_bindings >} in
        (o2, `Module (new_prefix, (`Block (List.rev reversed_renamed_bindings, dummy_phrase), pos)))
        (* let (o1, renamed_bindings) = o#list (fun o -> o#binding) bindings in *)
        (* Restore old stack *)
        (* printf "Renamed bindings for module %s: %s\n" name (print_list
         * (List.map (Sugartypes.Show_binding.show)  (List.rev reversed_renamed_bindings))); *)
        (* (o2, `Module (new_prefix, (`Block (renamed_bindings, dummy_phrase), pos))) *)
    | x -> super#bindingnode x


  method program = function
    | (bindings, body) ->
        let (o1, renamed_bindings) = self#list (fun o -> o#binding) bindings in
        let (o2, renamed_body) = o1#option (fun o -> o#phrase) body in
        (o2, (renamed_bindings, renamed_body))

end

let performRenaming prog =
  snd ((add_module_prefix "" (StringSet.empty) (StringSet.empty) [""])#program prog)

(* 1) Perform a renaming pass to expand names in modules to qualified names
 * 2) Peform a flattening pass to flatten modules to lists of bindings
 *)
let desugar_modules program =
  let renamedProgram = performRenaming program in
  let (renamedBindings, renamedBody) = renamedProgram in
  let flattenedProgram = performFlattening renamedProgram in
  (*
  printf "\n=============================================================\n";
  printf "\n=============================================================\n";
  printf "Before: %s\n" (Sugartypes.Show_program.show program);
  printf "\n=============================================================\n";
  printf "After: %s\n" (Sugartypes.Show_program.show flattenedProgram);
  *)
  flattenedProgram


