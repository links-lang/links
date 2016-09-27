open Utility
open Printf

(* open Sugartypes *)

type scope_id = int
(* Lists of declarations, references, imports, and an optional parent scope *)
type scope =
  { declarations : declaration list;
    references : reference list;
    imports : import list;
    parent_scope : scope_id option
  }
(* Declaration -- generally a binder. Contains name, position, and optional
 * named scope reference *)
and declaration = (string * scope_id option)

(* Name of a reference (function application, argument, etc.) *)
and reference = string

(* Name of an import (either in-module or out-of-module *)
and import  = string


(* Name of a scope *)
type scope_name = string option

(* Top-level scope *)
type scope_graph = scope intmap

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

(* Declaration with no associated scope *)
let plain_decl d = (d, None)

(* Declaration with associated named scope. S is SCOPE ID. *)
let annotated_decl d s = (d, Some s)

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

let rec construct_sg init_scope init_scope_graph scope_id =
object(self)
  inherit SugarTraversals.fold as super
  (* Current scope *)
  val scope = init_scope
  method get_scope = scope
  method set_scope s = {< scope = s >}

  val scope_id = scope_id
  method get_scope_id = scope_id

  (* Overall scope graph *)
  val scope_graph = init_scope_graph
  method get_scope_graph = scope_graph

  method qn_add_references = function
    | [] -> failwith "Internal error: empty qualified name list"
    | [x] -> self#set_scope (add_reference scope x)
    | x :: xs ->
        (* Add reference to current scope *)
        let scope1 = add_reference scope x in
        (* Create new, top-level scope and add *import* *)
        let qn_scope = new_scope None in
        let qn_scope_id = get_scope_num () in
        let qn_scope1 = add_import qn_scope x in
        (* Recurse using new scope on remainder of list *)
        let o_rec = (construct_sg qn_scope1 scope_graph qn_scope_id)#qn_add_references xs in
        let rec_scope = o_rec#get_scope in
        let rec_sg = o_rec#get_scope_graph in
        let new_sg = add_scope qn_scope_id rec_scope rec_sg in
        {< scope = scope1; scope_graph = new_sg >}

  method phrasenode = function
    | `Var n ->
        (* Add to references for current scope *)
        {< scope = add_reference scope n >}
    | `QualifiedVar ns ->
        self#qn_add_references ns
    | pn -> super#phrasenode pn

  (* *should* be the same for types, but if not, we can distinguish
   * type declarations from term declarations *)
  method datatype = function
    | `TypeVar (n, _, _) ->
        (* Add to references for current scope *)
        {< scope = add_reference scope n >}
    | `QualifiedTypeVar (ns, _, _) ->
        self#qn_add_references ns
    | dt -> super#datatype dt

  method bindingnode = function
    | `Fun ((fn_name, _, _), _lin, (_tyvars, fn_funlit), _loc, _dtopt) ->
        (* Firstly, add the declaration to the current scope *)
        let scope1 = add_declaration scope (plain_decl fn_name) in
        (* Next, generate a new scope *)
        let fn_scope_num = get_scope_num () in
        let fn_scope = new_scope (Some scope_id) in
        (* Now, perform an analysis on the function expression *)
        let o_scope = ((construct_sg fn_scope scope_graph fn_scope_num)#funlit fn_funlit) in
        (* Finally, get the updated scope graph, and add the new scope. *)
        let (fn_sg, fn_scope1) = (o_scope#get_scope_graph, o_scope#get_scope) in
        let new_sg = add_scope fn_scope_num fn_scope1 fn_sg in
        {< scope = scope1; scope_graph = new_sg >}
    | `Funs _ ->
        (* This is kind of problematic: `Funs is introduced in refineBindings, but
         * this will mean that mutually-recursive functions can't share names at
         * the top-level. Perhaps it could make sense to do two passes: one before (in order
         * to introduce `Funs) and one after (after module desugaring)
         * For now, let it be -- the old one had a similar issue, I think. *)
        assert false
    | `Import n ->
        (* Unqualified import. Add to imports list and references list. *)
        let scope1 = add_reference scope n in
        self#set_scope (add_import scope1 n)
    | `QualifiedImport ns ->
        (* Next, we need to properly set up anonymous scopes *)
        (* Firstly, add all segments of the qualified name into the imports list. *)
        let o1 = self#qn_add_references ns in
        let (sc, sc_id, sg) = (o1#get_scope, o1#get_scope_id, o1#get_scope_graph) in
        let sg1 = add_scope sc_id sc sg in
        let o2 = self#list (fun o n ->
            let modified_scope = add_import o#get_scope n in
            o#set_scope modified_scope) ns in
        {< scope_graph = sg1; scope = o2#get_scope >}
    | `Val (_tvs, pat, phr, _loc, _dtopt) ->
        (* (Deviating slightly from the algorithm in the paper here since our pattern
         * language is more complex).
         * Process the phrase using the current scope. Then, create a new scope
         * for the remainder of the binding list (and associated phrase), with
         * declarations of the pattern variables, and return this. *)
        let o = (construct_sg scope scope_graph scope_id)#phrase phr in
        (* Get the scope and new scope graph from the processed phrase *)
        let (phr_scope, phr_sg) = (o#get_scope, o#get_scope_graph) in
        let phr_sg1 = add_scope scope_id phr_scope phr_sg in
        (* This scope declares the new patterns *)
        let o_pattern = (construct_sg phr_scope phr_sg1 scope_id)#pattern pat in
        let (pat_scope, pat_sg) =
          (o_pattern#get_scope, o_pattern#get_scope_graph) in
        (* Save this scope to SG, since it's done now. *)
        let new_sg = add_scope scope_id pat_scope pat_sg in
        (* Add a fresh scope *)
        let following_scope_id = get_scope_num () in
        let following_scope = new_scope (Some scope_id) in
        (* Return new scope with old scope added to SG. *)
        {< scope = following_scope; scope_id = following_scope_id; scope_graph = new_sg >}
    | `Infix -> self
    | `Exp p -> self#phrase p
    | `Foreign ((bnd_name, _, _), _name, _dt) ->
        (* Not entirely sure how FFI works (if at all?) -- just add binder as a
         * decl *)
        {< scope = add_declaration scope (plain_decl bnd_name) >}
    | `Module (n, bl) ->
        (* Create new scope, with current scope as parent *)
        let module_scope_id = get_scope_num () in
        let module_scope = new_scope (Some scope_id) in
        (* Add declaration, with annotation of new scope name *)
        let module_decl = annotated_decl n module_scope_id in
        let our_scope = add_declaration scope module_decl in
        (* Proceed to process declarations using new scope *)
        let o_module = (construct_sg module_scope scope_graph module_scope_id)#phrase bl in
        let (o_scope, o_sg, o_scope_id) = (o_module#get_scope,
          o_module#get_scope_graph, o_module#get_scope_id) in
        (* Add final binding scope to scope graph *)
        let o_sg1 = add_scope o_scope_id o_scope o_sg in
        (* Using returned scope graph, process remainder of declarations *)
        {< scope = our_scope; scope_graph = o_sg1 >}
    | `Type (n, _, _) ->
        (* I suppose it depends on whether we want types to behave like function or Var bindings.
         * Let's treat them like defs (i.e. function bindings) for now *)
        {< scope = add_declaration scope (plain_decl n) >}

    method binder (n, _, _) =
      {< scope = add_declaration scope (plain_decl n) >}

    method program = function
      | (bindings, phr_opt) ->
          let o = self#list (fun o -> o#binding) bindings in
          let (sc, sc_id, sg) = (o#get_scope, o#get_scope_id, o#get_scope_graph) in
          (* No *idea* why o#option wasn't working. *)
          let o_scope =
            (match phr_opt with
               | Some phr ->(construct_sg sc sg sc_id)#phrase phr
               | None -> o) in
          let (sc1, sc_id1, sg1) = (o_scope#get_scope, o_scope#get_scope_id, o_scope#get_scope_graph) in
          (* Add final scope to the SG and we're away *)
          let sg2 = add_scope sc_id1 sc1 sg in
          {< scope = sc1; scope_id = sc_id1; scope_graph = sg2 >}

end

let create_scope_graph prog =
  let o = (construct_sg (new_scope None) (IntMap.empty) 0)#program prog in
  o#get_scope_graph

(* Print DOT file for scope graph *)
let show_scope_graph sg =
  let show_scope scope scope_id =
    let rec show_decls = function
      | [] -> ""
      | (d, assoc_opt) :: xs ->
          (sprintf "%d -> %s\n" scope_id d) ^
          (match assoc_opt with
             | None -> ""
             | Some assoc_id -> (sprintf "%s -> %d[arrowhead=\"empty\"]\n" d assoc_id)) ^
          (show_decls xs) in
    let rec show_references = function
      | [] -> ""
      | r::rs ->
          (sprintf "%s -> %d\n" r scope_id) ^ (show_references rs) in
    let rec show_imports = function
      | [] -> ""
      | i::is ->
          (sprintf "%d -> %s[arrowhead=\"empty\"]\n" scope_id i) ^ (show_imports is) in
    let show_parent = function
      | None -> ""
      | Some p -> sprintf "%d -> %d\n" scope_id p in
    (show_decls scope.declarations) ^ (show_references scope.references) ^
    (show_imports scope.imports) ^ (show_parent scope.parent_scope) in
  "digraph G {\n" ^
    String.concat ""
      (List.map (fun (s_id, s) -> show_scope s s_id) (IntMap.bindings sg))
      ^ "}"

let make_and_print_scope_graph prog =
  let sg = create_scope_graph prog in
  printf "%s\n" (show_scope_graph sg)

