(* New implementation of desugarModules making use of the scope graph. *)
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
open ScopeGraph

let get_fq_resolved_decl decl_name sg u_ast =
  ScopeGraph.make_resolved_plain_name decl_name sg u_ast

(* Wrapper function taking a unique reference, scope graph, and
 * unique AST, and providing a single output reference name.
 *
 * If the resolution is unsuccessful, will simply return the plain name,
 * which will be picked up as an error later.
 *
 * If the resolution is ambiguous, then the function will raise an error.
 * *)
let resolve name sg u_ast =
  match ScopeGraph.resolve_reference name sg u_ast with
    | `UnsuccessfulResolution ->
        (* failwith ("Lookup of " ^ name ^ " was unsuccessful") *)
        Uniquify.lookup_var name u_ast
    | `SuccessfulResolution decl_name ->
        (* printf "Successful resolution of name %s: %s\n" name decl_name; *)
        get_fq_resolved_decl decl_name sg u_ast
    | `AmbiguousResolution decl_names ->
        let plain_names = List.map (fun n -> get_fq_resolved_decl n sg u_ast) decl_names in
        failwith ("Error: ambiguous resolution for " ^ name ^ ":" ^ (print_list decl_names))


let rec get_last_list_value = function
  | [] -> failwith "INTERNAL ERROR: Empty list in get_last_list_value. This can only be caused by an" ^
            "empty qualified name and so should be outlawed by the grammar"
  | [x] -> x
  | x::xs -> get_last_list_value xs


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
    | (`Module (_, (`Block (bindings, _), _)), _) ->
        List.fold_left (fun acc binding -> acc#binding binding) self bindings
    | (`Import _, _) -> self
    | (`QualifiedImport _, _) -> self
    | b -> self#add_binding ((flatten_simple ())#binding b)

  method program = function
    | (bindings, _body) -> self#list (fun o -> o#binding) bindings
end

let perform_renaming scope_graph unique_ast =
object(self)
  inherit SugarTraversals.map as super

  method binder = function
    | (unique_name, dt, pos) ->
        (* Binders should just be resolved to their unique FQ name *)
        let plain_name =
          ScopeGraph.make_resolved_plain_name unique_name scope_graph unique_ast in
        (plain_name, dt, pos)

  method phrasenode = function
    | `Var name ->
        (* Resolve name. If it's ambiguous, throw an error.
         * If it's not found, just put the plain one back in and the error will
         * be picked up later.*)
        (* printf "Attempting to resolve Var %s\n" name; *)
        `Var (resolve name scope_graph unique_ast)
    | `QualifiedVar names ->
        (* Only need to look at the final name here *)
        let name = get_last_list_value names in
        (* printf "Attempting to resolve (qualified) Var %s\n" name; *)
        `Var (resolve name scope_graph unique_ast)
    | pn -> super#phrasenode pn
end


let desugarModules scope_graph unique_ast =
  let unique_prog = Uniquify.get_ast unique_ast in
  (*
  printf "Before module desugar: %s\n" (Sugartypes.Show_program.show unique_prog);
  printf "\n=============================================================\n"; *)
  let plain_prog =
    (perform_renaming scope_graph unique_ast)#program unique_prog in
  let o = (flatten_bindings ())#program plain_prog in
  let flattened_bindings = o#get_bindings in
  let flattened_prog = (flattened_bindings, snd plain_prog) in
  (* Debug *)
  (*
  printf "After module desugar: %s\n" (Sugartypes.Show_program.show flattened_prog); *)
  flattened_prog
