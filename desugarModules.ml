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


(* Given a *plain* name and a name shadowing table, looks up the FQN *)
let resolve name ht =
  try
    let xs = StringMap.find name ht in
    List.hd xs
  with _ ->
    (* For now, don't rename, and let this be picked up later.
     * It'd be better to change this at some point, when we get the prelude
     * better integrated with the module system. *)
    name

let rec perform_term_renaming module_table path ht =
  object(self)
    inherit SugarTraversals.fold_map as super

    val shadow_table = ht
    method get_shadow_table = shadow_table
    method bind_shadow name fqn = {< shadow_table = shadow_binding name fqn shadow_table >}
    method bind_open name fqn = {< shadow_table = shadow_open_terms name fqn module_table shadow_table >}

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
          let inner_o = (perform_term_renaming module_table new_path o_ht) in
          (* Builtin o#list didn't want to play ball, for some reason?? *)
          let (_, bs') =
            List.fold_left (fun (o, bs_acc) b ->
              let (o', b') = o#binding b in (o', b' :: bs_acc)) (inner_o, []) bs in
          let bs' = List.rev bs' in
          (* Finally, return `Module with updated bindings. The module itself
           * will be flattened out on the flattening pass. *)
          (* Now, this has the same effect as opening the module *)
          (self#bind_open n fqn, `Module (n, bs'))
      | bnd -> super#bindingnode bnd

    method phrasenode = function
      | `Block (bs, phr) ->
          (* Bindings should be processed one-by-one, then phrase should be processed,
           * then original scope should be restored *)
          let (o, bs') = self#list (fun o b -> o#binding b) bs in
          let (_, phr') = o#phrase phr in
          (self, `Block (bs', phr'))
      | `Var n -> (self, `Var (resolve n shadow_table))
      | `QualifiedVar ns ->
          (* Hm, pretty sure this is similar to qualified imports... *)
          let hd :: tl = ns in
          let prefix = resolve hd shadow_table in
          let fqn = String.concat module_sep (prefix :: tl) in
          (self, `Var fqn)
      | phr -> super#phrasenode phr

    method datatype dt = (self, dt)
    method datatype' dt' = (self, dt')

    (* Standard worry that this is catching too many names, but eh, we'll see *)
    (* method name s = (self, resolve s shadow_table) *)

    method program (bindings, phr_opt) =
      let (o, bs') = self#list (fun o b -> o#binding b) bindings in
      let (o, phr_opt') = o#option (fun o p -> o#phrase p) phr_opt in
      (o, (bs', phr_opt'))
  end


let rec perform_type_renaming module_table path ht =
  object(self)
    inherit SugarTraversals.fold_map as super

    val shadow_table = ht
    method get_shadow_table = shadow_table
    method bind_shadow name fqn = {< shadow_table = shadow_binding name fqn shadow_table >}
    method bind_open name fqn = {< shadow_table = shadow_open_types name fqn module_table shadow_table >}

    method bindingnode = function
      | `Type (n, tvs, dt) ->
          (* Add type binding *)
          let fqn = make_path_string path n in
          let o = self#bind_shadow n fqn in
          let (o, dt') = o#datatype' dt in
          (o#bind_shadow n fqn, `Type (fqn, tvs, dt'))
      (* I'm not happy *at all* with this repetition, but refactoring it to a separate superclass
       * is seeming to make it worse *)
      | `QualifiedImport ns ->
          let hd :: tl = ns in
          let prefix = resolve hd shadow_table in
          let fqn = String.concat module_sep (prefix :: tl) in
          (self#bind_open hd fqn, `QualifiedImport ns)
      | `Module (n, bs) ->
          let new_path = path @ [n] in
          let fqn = lst_to_path new_path in
          let o = self#bind_shadow n fqn in
          let o_ht = o#get_shadow_table in
          let inner_o = (perform_type_renaming module_table new_path o_ht) in
          let (_, bs') =
            List.fold_left (fun (o, bs_acc) b ->
              let (o', b') = o#binding b in (o', b' :: bs_acc)) (inner_o, []) bs in
          let bs' = List.rev bs' in
          (self#bind_open n fqn, `Module (n, bs'))
      | bn -> super#bindingnode bn

    method datatype = function
      | `TypeApplication (n, args) ->
          let fqn = resolve n shadow_table in
          (self, `TypeApplication (fqn, args))
      | `QualifiedTypeApplication (ns, args) ->
          let hd :: tl = ns in
          let prefix = resolve hd shadow_table in
          let fqn = String.concat module_sep (prefix :: tl) in
          (self, `TypeApplication (fqn, args))
      | dt -> super#datatype dt
  end

let rename_prog mt prog =
  let (_, prog') = (perform_term_renaming mt [] (StringMap.empty))#program prog in
  let (_, prog') = (perform_type_renaming mt [] (StringMap.empty))#program prog' in
  prog'

let desugarModules prog =
  let module_map = create_module_info_map prog in
  let renamed_prog = rename_prog module_map prog in
  flatten_prog renamed_prog
