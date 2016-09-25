(* Name of a scope *)
type scope_name = string option

(* Top-level scope *)
type scope_graph = scope intmap

(* Position *)
type position = int

(* Declaration -- generally a binder. Contains name, position, and optional
 * named scope reference *)
type declaration = (string * position * scope option)

(* Name of a reference (function application, argument, etc.) *)
type reference = (string * position)

(* Name of an import (either in-module or out-of-module *)
type import  = (string * position)

(* Lists of declarations, references, imports, and an optional parent scope *)
type scope =
  { declarations : declaration list;
    references : reference list;
    imports : import list;
    parent_scope : scope option
  }

(* Gets a list of scopes from the scope graph *)
let get_scopes sg = sg

(* Gets a list of declarations from a scope *)
let get_declarations scope = scope.declarations

(* Gets a list of references from a scope *)
let get_references scope = scope.references

let get_imports scope = scope.imports

let get_parent scope = scope.parent_scope

(* Scope graph construction *)

(* Scope counter *)
let scope_counter = ref 0
let get_scope_num =
  let counter = scope_counter in
  fun () ->
    begin
      incr counter;
      !counter
    end

(* We need:
  *  top level bindings -> scope
  *  block bindings -> scope
  *  expression -> scope
*)

let add_declaration scope decl =
  { scope with declarations = decl :: scope.declarations }

let add_reference scope reference =
  { scope with references = reference :: scope.references }

let add_import scope import =
  { scope with imports = import :: scope.imports }

let new_scope parent =
  { declarations = [];
    references = [];
    imports = [];
    parent_scope = parent }

let add_scope scope_id scope scope_graph = IntMap.add scope_id scope scope_graph

let rec sg_bindings init_scope init_scope_graph scope_id =
object(self)
  inherit SugarTraversals.fold as super
  (* Current scope *)
  val scope = init_scope
  method get_scope = scope
  method set_scope s = {< scope = s >}

  (* Overall scope graph *)
  val scope_graph = init_scope_graph
  method get_scope_graph = scope_graph

  method bindingnode = function
    | `Def (name, b) ->
        (* Populate inner scope, then add to scope graph *)
        let inner_scope_num = get_scope_num () in
        let o_bindingnode = self#bindingnode b in
        o_bindingnode#add_scope inner_scope_num o_bindingnode#get_scope
    | `Fun ((fn_name, _, _), _lin, (_tyvars, fn_phrase), _loc, _dtopt) ->
        (* Generate a new scope *)
        let fn_scope_num = get_scope_num () in
        (* Functions are assumed to be recursive, so are added to the scope *)
        let fn_scope = add_declaration (new_scope (Some scope_id)) fn_name in
        (* Now, perform an analysis on the function expression *)
        let o_scope = sg_inner_bindings fn_scope scope_graph fn_scope_num in
        (* Finally, get the updated scope graph, and add the new scope. *)
        let (fn_sg, fn_scope1) = (o_scope#get_scope_graph, o_scope#get_scope) in
        let new_sg = add_scope fn_scope_num fn_scope fn_sg in
        {< scope_graph = new_sg >}
    | `Funs _ ->
        (* This is kind of problematic: `Funs is introduced in refineBindings, but
         * this will mean that mutually-recursive functions can't share names at
         * the top-level. Perhaps it could make sense to do two passes: one before (in order
         * to introduce `Funs) and one after (after module desugaring)
         * For now, let it be -- the old one had a similar issue, I think. *)
        assert false
    | `Import n ->
        (* Add to imports list *)
        (* to be continued... *)



end
and sg_inner_bindings init_scope init_scope_graph =
object(self)
  inherit SugarTraversals.fold as super
  val scope = init_scope
  method get_scope = scope
end
and sg_phrase =
object(self)
  inherit SugarTraversals.fold as super
  val scope = init_scope
  method get_scope = scope
end


let create_scope_graph prog =
  let o = (sg_global_bindings (new_scope None) (IntMap.singleton 0) 0)#program prog in
  o#get_scope_graph

