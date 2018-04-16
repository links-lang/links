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

let _print_shadow_table st =
    List.iter (fun (n, fqns) -> printf "%s: %s\n" n (print_list fqns))
    (StringMap.bindings st)

(* After renaming, we can simply discard modules and imports. *)
let rec flatten_simple = fun () ->
object(self)
  inherit SugarTraversals.map as super

  method! phrasenode : phrasenode -> phrasenode = function
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
  inherit SugarTraversals.fold

  val bindings = []
  method add_binding x = {< bindings = x :: bindings >}
  method get_bindings = List.rev bindings

  method! binding = function
    | (`Module (_, bindings), _) ->
        self#list (fun o -> o#binding) bindings
    | (`QualifiedImport _, _) -> self
    | b -> self#add_binding ((flatten_simple ())#binding b)

  method! program = function
    | (bindings, _body) -> self#list (fun o -> o#binding) bindings
end

let flatten_prog prog =
  let (_, phr) = prog in
  let o = (flatten_bindings ())#program prog in
  (o#get_bindings, phr)

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
let rec rename_binders_get_shadow_tbl module_table
            path term_ht type_ht =
  object (self)
    inherit SugarTraversals.fold_map as super

    val term_shadow_table = term_ht
    val type_shadow_table = type_ht
    method get_term_shadow_table = term_shadow_table
    method get_type_shadow_table = type_shadow_table

    method bind_shadow_term name fqn =
        {< term_shadow_table = shadow_binding name fqn term_shadow_table >}
    method bind_shadow_type name fqn =
        {< type_shadow_table = shadow_binding name fqn type_shadow_table >}

    method bind_open name fqn =
        let (term_ht, type_ht) =
            shadow_open name fqn module_table term_shadow_table type_shadow_table in
        {< term_shadow_table = term_ht; type_shadow_table = type_ht >}

    method! binder = function
      | (n, dt_opt, pos) ->
          let fqn = make_path_string path n in
          (self#bind_shadow_term n fqn, (fqn, dt_opt, pos))

    method! bindingnode = function
      | `Fun (bnd, lin, (tvs, fnlit), loc, dt_opt) ->
          let (o, bnd') = self#binder bnd in
          (o, `Fun (bnd', lin, (tvs, fnlit), loc, dt_opt))
      | `Type t -> (self, `Type t)
      | `Val v -> (self, `Val v)
      | `Exp b -> (self, `Exp b)
      | `Foreign (bnd, raw_name, lang, ext_file, dt) ->
          let (o, bnd') = self#binder bnd in
          (o, `Foreign (bnd', raw_name, lang, ext_file, dt))
      | `AlienBlock (lang, lib, decls) ->
          let (o, decls') = self#list (fun o (bnd, dt) ->
            let (o, bnd') = o#binder bnd in
            (o, (bnd', dt))) decls in
          (o, `AlienBlock (lang, lib, decls'))
      | `QualifiedImport [] -> assert false
      | `QualifiedImport ((hd :: tl) as ns) ->
          (* Try to resolve head of PQN. This will either resolve to itself, or
           * to a prefix. Once we have the prefix, we can construct the FQN. *)
          (* Qualified names must (by parser construction) be of at least length 1. *)
          let final = List.hd (List.rev ns) in
          let prefix = resolve hd term_shadow_table in
          let fqn = String.concat module_sep (prefix :: tl) in
          (self#bind_open final fqn, `QualifiedImport ns)
      | `Module (n, bs) ->
          let new_path = path @ [n] in
          let fqn = lst_to_path new_path in
          (* New FQN for module must shadow n *)
          let o = self#bind_shadow_term n fqn in
          let o = o#bind_shadow_type n fqn in
          let o_term_ht = o#get_term_shadow_table in
          let o_type_ht = o#get_type_shadow_table in
          (* Recursively get *and rename* inner scope *)
          let (_, _, bindings') =
              process_binding_list bs module_table new_path o_term_ht o_type_ht in
          (* Finally, return `Module with updated bindings. The module itself
           * will be flattened out on the flattening pass. *)
          (o, `Module (n, bindings'))
      | b -> super#bindingnode b
  end

and perform_renaming module_table path term_ht type_ht =
  object(self)
    inherit SugarTraversals.fold_map as super

    val term_shadow_table = term_ht
    val type_shadow_table = type_ht
    method get_term_shadow_table = term_shadow_table
    method get_type_shadow_table = type_shadow_table
    method bind_shadow_term name fqn =
        {< term_shadow_table = shadow_binding name fqn term_shadow_table >}
    method bind_shadow_type name fqn =
        {< type_shadow_table = shadow_binding name fqn type_shadow_table >}

    method! binder = function
      | (n, dt_opt, pos) ->
          let fqn = make_path_string path n in
          (self#bind_shadow_term n fqn, (fqn, dt_opt, pos))

    method! patternnode = function
      | `Variant (n, p_opt) ->
          let fqn = resolve n term_shadow_table in
          let (o, p_opt') = self#option (fun o -> o#pattern) p_opt in
          (o, `Variant (fqn, p_opt'))
      | p -> super#patternnode p

    method! row = function
      | (xs, rv) ->
          let (o, xs') =
            self#list (fun o (name, fspec) ->
              let (o, fspec') = o#fieldspec fspec in
              (o, (name, fspec'))) xs in
          let (_, rv') = o#row_var rv in
          (self, (xs', rv'))

    method! bindingnode = function
      | `Module (n, bs) ->
          (self, `Module (n, bs))
      | `AlienBlock ab ->
          (self, `AlienBlock ab)
      | `Foreign f -> (self, `Foreign f)
      | `Type (n, tvs, dt) ->
          (* Add type binding *)
          let fqn = make_path_string path n in
          let o = self#bind_shadow_type n fqn in
          let (o, dt') = o#datatype' dt in
          (o, `Type (fqn, tvs, dt'))
      | `Val (tvs, pat, phr, loc, dt_opt) ->
          let (_, phr') = self#phrase phr in
          let (o, pat') = self#pattern pat in
          let (o, dt_opt') = o#option (fun o -> o#datatype') dt_opt in
          (o, `Val (tvs, pat', phr', loc, dt_opt'))
      | `Fun (bnd, lin, (tvs, fnlit), loc, dt_opt) ->
          (* Binder will have been changed. We need to add the funlit pattern
           * to the env. *)
          let (_, fnlit') = self#funlit fnlit in
          let (o, dt_opt') = self#option (fun o -> o#datatype') dt_opt in
          (o, `Fun (bnd, lin, (tvs, fnlit'), loc, dt_opt'))
      | b -> super#bindingnode b

    method! binop = function
      | `Name n -> (self, `Name (resolve n term_shadow_table))
      | bo -> super#binop bo

    method! unary_op = function
      | `Name n -> (self, `Name (resolve n term_shadow_table))
      | uo -> super#unary_op uo

    method! phrasenode = function
      | `Block (bs, phr) ->
          (* Process bindings, then process the phrase using
           * updated shadow table. *)
          let (term_ht, type_ht, bs') =
              process_binding_list bs module_table path
                term_shadow_table type_shadow_table in
          let (_, phr') =
              (perform_renaming module_table path
                term_ht type_ht)#phrase phr in
          (self, `Block (bs', phr'))
      | `Var n -> (self, `Var (resolve n term_shadow_table))
      | `RecordLit (xs, p_opt) ->
          let (_, xs') =
            self#list (fun o (n, p) ->
              let (o, p') = o#phrase p in
              (o, (n, p'))) xs in
          let (_, p_opt') = self#option (fun o -> o#phrase) p_opt in
          (self, `RecordLit (xs', p_opt'))
      | `Projection (p, n) ->
          let (_, p') = self#phrase p in
          (self, `Projection (p', n))
      | `ConstructorLit (n, p_opt, dt_opt) ->
          (* Resolve constructor name using term table *)
          let fqn = resolve n term_shadow_table in
          let (_, p_opt') = self#option (fun o -> o#phrase) p_opt in
          (self, `ConstructorLit (fqn, p_opt', dt_opt))
      | `QualifiedVar [] -> assert false
      | `QualifiedVar (hd :: tl) ->
          (* Similar to qualified imports. *)
          let prefix = resolve hd term_shadow_table in
          let fqn = String.concat module_sep (prefix :: tl) in
          (self, `Var fqn)
      | phr -> super#phrasenode phr

    method! datatype = function
      | `Function (dts, row, dt) ->
          let (_, dts') = self#list (fun o -> o#datatype) dts in
          let (_, dt') = self#datatype dt in
          (self, `Function (dts', row, dt'))
      | `TypeApplication (n, args) ->
          let fqn = resolve n type_shadow_table in
          let (_, args') = self#list (fun o -> o#type_arg) args in
          (self, `TypeApplication (fqn, args'))
      | `QualifiedTypeApplication ([], _args) -> assert false
      | `QualifiedTypeApplication (hd :: tl, args) ->
          let prefix = resolve hd type_shadow_table in
          let fqn = String.concat module_sep (prefix :: tl) in
          let (_, args') = self#list (fun o -> o#type_arg) args in
          (self, `TypeApplication (fqn, args'))
      | `Variant (xs, rv) ->
          (* Variants need to have constructors renamed *)
          let (o, xs') =
            self#list (fun o (name, fspec) ->
              let fqn = make_path_string path name in
              let o = o#bind_shadow_term name fqn in
              let (o, fspec') = o#fieldspec fspec in
              (o, (fqn, fspec'))) xs in
          let (o, rv') = o#row_var rv in
          (o, `Variant (xs', rv'))
      | dt -> super#datatype dt

  end

  and process_binding_list : binding list -> module_info stringmap ->
    string list -> string list stringmap -> string list stringmap ->
      (string list stringmap * string list stringmap * binding list) =
          fun binding_list mt path term_ht type_ht ->
    (* Group bindings *)
    let binding_group_list = group_bindings binding_list in
    (* For each binding group, get the shadowing table, and then use the shadowing
     * table to do the renaming *)
    let (term_ht, type_ht, bnds_rev) =
        List.fold_left (fun (term_ht, type_ht, bnd_acc) bnds ->
          (* Rename functions and create shadow table *)
          let (o, bnds') =
            (rename_binders_get_shadow_tbl mt path
                term_ht type_ht)#list (fun o -> o#binding) bnds in
          (* Get shadow tables *)
          let term_ht = o#get_term_shadow_table in
          let type_ht = o#get_type_shadow_table in
          (* Rename each of the bindings *)
          let (o, bnds') =
            (perform_renaming mt path
                term_ht type_ht)#list (fun o -> o#binding) bnds' in
          (* Get final shadow tables *)
          let term_ht = o#get_term_shadow_table in
          let type_ht = o#get_type_shadow_table in
          (* Keep everything in reverse order -- more efficient *)
          (term_ht, type_ht, (List.rev bnds') @ bnd_acc))
      (term_ht, type_ht, []) binding_group_list in
    (term_ht, type_ht, List.rev bnds_rev)

let rename mt (bindings, phr_opt) =
  let (term_ht, ty_ht, bindings') =
      process_binding_list bindings mt [] StringMap.empty StringMap.empty in
  let (_, phr') =
      (perform_renaming mt [] term_ht ty_ht)#option
        (fun o -> o#phrase ) phr_opt in
  (bindings', phr')

let desugarModules prog =
  let module_map = create_module_info_map prog in
  let renamed_prog = rename module_map prog in
  let flattened_prog = flatten_prog renamed_prog in
  (* printf "Flattened AST: %s\n%!" (Sugartypes.show_program flattened_prog); *)
  flattened_prog
