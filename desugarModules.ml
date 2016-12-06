(* Implementation of desugarModules, simplified *)
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
 * val Foo.bobsleigh = ...;
 * fun Foo.x() { ...}
 * fun Foo.Bar.y() { ... }
 * val x = ...;
 *
*)
open Utility
open Sugartypes
open Printf
open ModuleUtils

(* After renaming, we can simply discard modules and imports. *)
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
    | (`Module (_, bindings), _) ->
        self#list (fun o -> o#binding) bindings
    | (`QualifiedImport _, _) -> self
    | b -> self#add_binding ((flatten_simple ())#binding b)

  method program = function
    | (bindings, _body) -> self#list (fun o -> o#binding) bindings
end

let flatten_prog prog =
  let (_, phr) = prog in
  let o = (flatten_bindings ())#program prog in
  (o#get_bindings, phr)

type env_map = string list stringmap

type module_info = {
    simple_name : string; (* Note: not fully-qualified *)
    inner_modules : string list;
    type_names : string list;
    decl_names : string list
  }

let make_module_info simple_name inner_modules type_names decl_names =
  { simple_name = simple_name; inner_modules = inner_modules;
    type_names = type_names; decl_names = decl_names }

type module_info_map = module_info stringmap


let get_pat_vars () =
  object(self)
    inherit SugarTraversals.fold as super
    val bindings = []
    method add_binding x = {< bindings = x :: bindings >}
    method get_bindings = bindings (* Order doesn't matter *)

    method patternnode = function
      | `Variant (_n, p_opt) ->
           self#option (fun o p -> o#pattern p) p_opt
      (* | `Negative ns -> self#list (fun o p -> o#add_binding p) ns *)
      | `Record (ls, p_opt) ->
          let o1 = self#list (fun o (_, p) -> o#pattern p) ls in
          o1#option (fun o p -> o#pattern p) p_opt
      | `Variable (n, _, _) -> self#add_binding n
      | p -> super#patternnode p
  end

let get_pattern_variables p = ((get_pat_vars ())#pattern p)#get_bindings

let make_path_string xs name =
  if name = "" then "" else
    let xs1 = xs @ [name] in
    String.concat module_sep xs1

let create_module_info_map program =
  (* Helper functions *)
  let module_map = ref StringMap.empty in
  let add_module_info fq_module_name info =
    let mm = !module_map in
    module_map := StringMap.add fq_module_name info mm in

  let rec create_and_add_module_info parent_path name bindings =
    (* Helper functions: traversing modules, and getting binding names *)
    (* Recursively traverse a list of modules *)
    let rec traverse_modules = function
      | [] -> []
      | (`Module (submodule_name, mod_bs), _) :: bs ->
          (* Recursively process *)
          let new_path = if name = "" then [] else parent_path @ [name] in
          create_and_add_module_info new_path submodule_name mod_bs;
          (* Add the name to the list, process remainder. *)
          submodule_name :: (traverse_modules bs)
      | _bs -> assert false in (* List should only contain modules *)

    (* Getting binding names -- we're interested in function and value names *)
    let rec get_binding_names = function
      | [] -> []
      | (`Val (_, pat, _, _, _), _) :: bs -> (get_pattern_variables pat) @ get_binding_names bs
      | (`Fun ((n, _, _), _, _, _, _), _) :: bs -> n :: (get_binding_names bs)
      | _ :: bs -> get_binding_names bs in (* Other binding types are uninteresting for this pass *)

    (* Getting type names -- we're interested in typename decls *)
    let rec get_type_names = function
      | [] -> []
      | (`Type (n, _, _), _) :: bs -> n :: (get_type_names bs)
      | _ :: bs -> get_type_names bs in

    (* Next, separate out bindings *)
    let (inner_modules, other_bindings) = separate_modules bindings in
    (* Next, use our helper functions *)
    let inner_module_names = traverse_modules inner_modules in
    let binding_names = get_binding_names other_bindings in
    let type_names = get_type_names other_bindings in
    (* Finally, construct the module info, and add to the table. *)
    let path_str = make_path_string parent_path name in
    let mod_info = make_module_info name inner_module_names type_names binding_names in

    add_module_info path_str mod_info in

  (* Toplevel *)
  let (bindings, _) = program in
  create_and_add_module_info [] "" bindings;
  !module_map

(* Given a binding name and fully-qualified name, adds it to the top of
 * the binding stack in the name shadowing table. For example, shadowing name foo with A.foo,
 * given a table
 *   foo |-> [B.foo]
 *   bar |-> [A.bar]
 * will result in
 *   foo |-> [A.foo, B.foo]
 *   bar |-> [A.bar]
 *)
let shadow_binding : string -> string -> (string list) stringmap -> (string list) stringmap = fun name fqn ht ->
  try
    let xs = StringMap.find name ht in
    StringMap.add name (fqn :: xs) ht
  with _ ->
    StringMap.add name [fqn] ht

(* Given a *fully qualified* module name and a name shadowing table, shadows
 * the appropriate bindings. For example, given a name resolution table:
 *   foo |-> [A.foo, foo]
 *   bar |-> [bar]
 * and a module B:
 *   module B {
 *     module C {
 *       def pines() { .. }
 *     }
 *
 *     def foo() { .. }
 *     val baz
 *   }
 *  the resulting table would be:
 *  foo |-> [B.foo, A.foo, foo]
 *  bar |-> [bar]
 *  baz |-> [B.baz]
 *  B   |-> [B]
 *  C   |-> [B.C]
type module_info = {
    simple_name : string; (* Note: not fully-qualified *)
    inner_modules : string list;
    type_names : string list;
    decl_names : string list
  }

 *)

let print_mod_info k mi =
   printf "MODULE: %s\n" k;
   printf "Inner modules: %s\n" (print_list mi.inner_modules);
   printf "Type names: %s\n" (print_list mi.type_names);
   printf "Decl names: %s\n" (print_list mi.decl_names)

let print_mt mt =
  printf "MT:\n";
  List.iter (fun (k, mi) -> print_mod_info k mi) (StringMap.bindings mt)

let shadow_open module_plain module_fqn module_table ht =
  let mod_info = StringMap.find module_fqn module_table in
  (* Firstly, shadow all bindings *)
  let shadowed_binding_ht = List.fold_left (fun acc plain_binding_name ->
    let fq_binding_name = String.concat module_sep (module_fqn :: [plain_binding_name]) in
    shadow_binding plain_binding_name fq_binding_name acc) ht mod_info.decl_names in
  (* Next, do the modules *)
  let shadowed_module_ht = List.fold_left (fun acc plain_module_name ->
    let fq_module_name = String.concat module_sep (module_fqn :: [plain_module_name]) in
    shadow_binding plain_module_name fq_module_name acc) shadowed_binding_ht mod_info.inner_modules in
  (* Finally, need to add this module of course! *)
  let shadowed_ht = shadow_binding module_plain module_fqn shadowed_module_ht in
  shadowed_ht

let lst_to_path = String.concat module_sep

(* Given a *plain* name and a name shadowing table, looks up the FQN *)
let resolve name ht =
  let xs = StringMap.find name ht in
  List.hd xs

let rec perform_renaming module_table path ht =
  object(self)
    inherit SugarTraversals.fold_map as super

    val shadow_table = ht
    method get_shadow_table = shadow_table
    method bind_shadow name fqn = {< shadow_table = shadow_binding name fqn shadow_table >}
    method bind_open name fqn = {< shadow_table = shadow_open name fqn module_table shadow_table >}
    method binder = function
      | (n, dt_opt, pos) ->
          let fqn = make_path_string path n in
          let o = self#bind_shadow n fqn in
          (o, (fqn, dt_opt, pos))
    method bindingnode = function
      | `Val (tvs, pat, phr, loc, dt_opt) ->
          (* First off, process the phrase. The returned map won't be needed. *)
          let (_, phr') = self#phrase phr in
          (* Next, add the variable to the shadowing table, and proceed *)
          let (o, pat') = self#pattern pat in
          (o, `Val (tvs, pat', phr', loc, dt_opt))
      | `Fun (bnd, lin, (tvs, fnlit), loc, dt_opt) ->
          let (o, bnd') = self#binder bnd in
          let (_, fnlit') = self#funlit fnlit in
          (o, `Fun (bnd', lin, (tvs, fnlit'), loc, dt_opt))
      | `QualifiedImport ns ->
          (* Try to resolve head of PQN. This will either resolve to itself, or
           * to a prefix. Once we have the prefix, we can construct the FQN. *)
          (* Qualified names must (by parser construction) be of at least length 1. *)
          let hd :: tl = ns in
          let prefix = resolve hd shadow_table in
          let fqn = String.concat module_sep (prefix :: tl) in
          (* We return the QI unmodified here -- it'll be removed on the flattening pass *)
          (self#bind_open hd fqn, `QualifiedImport ns)
      | `Module (n, bs) ->
          let new_path = path @ [n] in
          let fqn = lst_to_path new_path in
          (* New FQN for module must shadow n *)
          let o = self#bind_shadow n fqn in
          let o_ht = o#get_shadow_table in
          (* Recursively perform the renaming (with new path) to get the renamed bindings.
           * Ignore the resulting object, as it pertains to the inner scope. *)
          let inner_o = (perform_renaming module_table new_path o_ht) in
          (* Builtin o#list didn't want to play ball, for some reason?? *)
          let (_, bs') =
            List.fold_left (fun (o, bs_acc) b ->
              let (o', b') = o#binding b in (o', b' :: bs_acc)) (inner_o, []) bs in
          let bs' = List.rev bs' in
          (* let (_, bs') = inner_o#list (fun o b -> o#binding b) bs in *)
          (* Finally, return `Module with updated bindings. The module itself
           * will be flattened out on the flattening pass. *)
          (* Now, this has the same effect as opening the module *)
          let o_final = self#bind_open n fqn in
          (self#bind_open n fqn, `Module (n, bs'))
      | bnd -> super#bindingnode bnd
    method phrasenode = function
      | `Block (bs, phr) ->
          (* Bindings should be processed one-by-one, then phrase should be processed,
           * then original scope should be restored *)
          let (o, bs') = self#list (fun o b -> o#binding b) bs in
          let (_, phr') = o#phrase phr in
          (self, `Block (bs', phr'))
      | `QualifiedVar ns ->
          (* Hm, pretty sure this is similar to qualified imports... *)
          let hd :: tl = ns in
          let prefix = resolve hd shadow_table in
          let fqn = String.concat module_sep (prefix :: tl) in
          (self, `Var fqn)
      | phr -> super#phrasenode phr

    (* Standard worry that this is catching too many names, but eh, we'll see *)
    method name s = (self, resolve s shadow_table)

    method program (bindings, phr_opt) =
      let (o, bs') = self#list (fun o b -> o#binding b) bindings in
      let (o, phr_opt') = o#option (fun o p -> o#phrase p) phr_opt in
      (o, (bs', phr_opt'))
  end

let rename_prog mt prog =
  let (_, prog') = (perform_renaming mt [] (StringMap.empty))#program prog in
  prog'

let desugarModules prog =
  let module_map = create_module_info_map prog in
  let renamed_prog = rename_prog module_map prog in
  flatten_prog renamed_prog
