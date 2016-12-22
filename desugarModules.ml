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

(* group_bindings : binding list -> binding list list *)
(* Groups lists of bindings to bindings that are in the same scope. *)
let group_bindings : binding list -> binding list list = fun bindings ->
  let rec group_bindings_inner acc ret = function
    | [] when acc = [] -> List.rev ret
    | [] -> List.rev ((List.rev acc) :: ret)
    | ((`Fun (_, _, _, _, _), _) as bnd) :: bs ->
        group_bindings_inner (bnd :: acc) ret bs
    | b :: bs ->
        (* End block of functions, need to start a new scope *)
        group_bindings_inner [] ([b] :: (List.rev acc) :: ret) bs in
  group_bindings_inner [] [] bindings

(* Come across binding list:
  * - Group bindings into list of lists
  * - Get shadow table for the binding list
  * - Perform renaming
*)
let rec rename_binders_get_shadow_tbl module_table path ht =
  object (self)
    inherit SugarTraversals.fold_map as super

    val shadow_table = ht
    method get_shadow_table = shadow_table
    method bind_shadow name fqn = {< shadow_table = shadow_binding name fqn shadow_table >}
    method bind_open name fqn = {< shadow_table = shadow_open_terms name fqn module_table shadow_table >}

    method binder = function
      | (n, dt_opt, pos) ->
          let fqn = make_path_string path n in
          (self#bind_shadow n fqn, (fqn, dt_opt, pos))

    method bindingnode = function
      | `Fun (bnd, lin, (tvs, fnlit), loc, dt_opt) ->
          let (o, bnd') = self#binder bnd in
          (o, `Fun (bnd', lin, (tvs, fnlit), loc, dt_opt))
      | `Val v -> (self, `Val v)
      | `Exp b -> (self, `Exp b)
      | `QualifiedImport ns ->
          (* Try to resolve head of PQN. This will either resolve to itself, or
           * to a prefix. Once we have the prefix, we can construct the FQN. *)
          (* Qualified names must (by parser construction) be of at least length 1. *)
          let hd :: tl = ns in
          let final = List.hd (List.rev ns) in
          let prefix = resolve hd shadow_table in
          let fqn = String.concat module_sep (prefix :: tl) in
          (self#bind_open final fqn, `QualifiedImport ns)
      | `Module (n, bs) ->
          let new_path = path @ [n] in
          let fqn = lst_to_path new_path in
          (* New FQN for module must shadow n *)
          let o = self#bind_shadow n fqn in
          let o_ht = o#get_shadow_table in
          (* Recursively get *and rename* inner scope *)
          let (_, bindings') = process_binding_list bs module_table new_path o_ht in
          (* Finally, return `Module with updated bindings. The module itself
           * will be flattened out on the flattening pass. *)
          (* Now, this has the same effect as opening the module *)
          (self#bind_open n fqn, `Module (n, bindings'))
      | `Type t -> (self, `Type t)
      | b -> super#bindingnode b
  end

and perform_term_renaming module_table path ht =
  object(self)
    inherit SugarTraversals.fold_map as super

    val shadow_table = ht
    method get_shadow_table = shadow_table
    method bind_shadow name fqn = {< shadow_table = shadow_binding name fqn shadow_table >}

    method binder = function
      | (n, dt_opt, pos) ->
          let fqn = make_path_string path n in
          (self#bind_shadow n fqn, (fqn, dt_opt, pos))

    method bindingnode = function
      | `Module (n, bs) -> (self, `Module (n, bs))
      | `Type t -> (self, `Type t)
      | `Val (tvs, pat, phr, loc, dt_opt) ->
          let (_, phr') = self#phrase phr in
          let (o, pat') = self#pattern pat in
          (o, `Val (tvs, pat', phr', loc, dt_opt))
      | `Fun (bnd, lin, (tvs, fnlit), loc, dt_opt) ->
          (* Binder will have been changed. We need to add the funlit pattern
           * to the env. *)
          let (_, fnlit') = self#funlit fnlit in
          (self, `Fun (bnd, lin, (tvs, fnlit'), loc, dt_opt))
      | b -> super#bindingnode b

    method binop = function
      | `Name n -> (self, `Name (resolve n shadow_table))
      | bo -> super#binop bo

    method unary_op = function
      | `Name n -> (self, `Name (resolve n shadow_table))
      | uo -> super#unary_op uo

    method phrasenode = function
      | `Block (bs, phr) ->
          (* Process bindings, then process the phrase using
           * updated shadow table. *)
          let (ht, bs') = process_binding_list bs module_table path shadow_table in
          let (_, phr') = (perform_term_renaming module_table path ht)#phrase phr in
          (self, `Block (bs', phr'))
      | `Var n -> (self, `Var (resolve n shadow_table))
      | `QualifiedVar ns ->
          (* Similar to qualified imports. *)
          let hd :: tl = ns in
          let prefix = resolve hd shadow_table in
          let fqn = String.concat module_sep (prefix :: tl) in
          (self, `Var fqn)
      | phr -> super#phrasenode phr

    method datatype dt = (self, dt)
    method datatype' dt' = (self, dt')
  end

  and process_binding_list : binding list -> module_info stringmap ->
    string list -> string list stringmap ->
      (string list stringmap * binding list) = fun binding_list mt path ht ->
    (* Group bindings *)
    let binding_group_list = group_bindings binding_list in
    (* For each binding group, get the shadowing table, and then use the shadowing
     * table to do the renaming *)
    let (ht, bnds_rev) = List.fold_left (fun (ht, bnd_acc) bnds ->
      let (o, bnds') =
        (rename_binders_get_shadow_tbl mt path ht)#list (fun o -> o#binding) bnds in
      let ht = o#get_shadow_table in
      let (o, bnds') =
        (perform_term_renaming mt path ht)#list (fun o -> o#binding) bnds' in
      let ht = o#get_shadow_table in
      (* Keep everything in reverse order -- more efficient *)
      (ht, (List.rev bnds') @ bnd_acc)) (ht, []) binding_group_list in
    (ht, List.rev bnds_rev)

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
          let final = List.hd (List.rev ns) in
          let prefix = resolve hd shadow_table in
          let fqn = String.concat module_sep (prefix :: tl) in
          (self#bind_open final fqn, `QualifiedImport ns)
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
          let (o, args') = self#list (fun o -> o#type_arg) args in
          (o, `TypeApplication (fqn, args'))
      | `QualifiedTypeApplication (ns, args) ->
          let hd :: tl = ns in
          let prefix = resolve hd shadow_table in
          let fqn = String.concat module_sep (prefix :: tl) in
          let (o, args') = self#list (fun o -> o#type_arg) args in
          (o, `TypeApplication (fqn, args'))
      | dt -> super#datatype dt
  end

let rename_terms mt (bindings, phr_opt) =
  let (ht, bindings') = process_binding_list bindings mt [] StringMap.empty in
  let (_, phr') = (perform_term_renaming mt [] ht)#option (fun o -> o#phrase ) phr_opt in
  (bindings', phr')

let rename_prog mt prog =
  let prog' = rename_terms mt prog in
  let (_, prog') = (perform_type_renaming mt [] (StringMap.empty))#program prog' in
  prog'

let desugarModules prog =
  let module_map = create_module_info_map prog in
  let renamed_prog = rename_prog module_map prog in
  let flattened_prog = flatten_prog renamed_prog in
  (* printf "Flattened AST: %s\n" (Sugartypes.Show_program.show flattened_prog); *)
  flattened_prog
