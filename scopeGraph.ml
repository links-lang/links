open Utility
open Printf

(* open Sugartypes *)
type name = Sugartypes.name
type fq_name = Sugartypes.name
type unique_name = Sugartypes.name

type scope_id = int

(* Declaration -- generally a binder. Contains name, position, and optional
 * named scope reference *)
type declaration = (string * scope_id option)

(* Name of a reference (function application, argument, etc.) *)
type reference = string

(* Name of an import (either in-module or out-of-module *)
type import  = string

module Decl = struct
  type t = declaration
  (* Deriving makes it nigh-on impossible to use Pervasives.compare. Great.
   * This is an ungodly awful hack. *)
  let compare = Pervasives.compare
  (*
    let opt_as_str opt =
      (match opt with
         | Some sc1_id -> string_of_int sc1_id
         | None -> "NONE") in
    let opt1_str = opt_as_str opt1 in
    let opt2_str = opt_as_str opt2 in
    String.compare (s1 ^ opt1_str) (s2 ^ opt2_str)
  *)
  module Show_t = Deriving_Show.Show_unprintable (struct type a = t end)
end

module DeclSet = Set.Make(Decl)
type declset = DeclSet.t

module AnnotatedDecl = struct
  type t = (declaration * string)
  let compare = Pervasives.compare
  module Show_t = Deriving_Show.Show_unprintable (struct type a = t end)
end

module AnnotatedDeclSet = Set.Make(AnnotatedDecl)
type annotated_decl_set = AnnotatedDeclSet.t

(* Lists of declarations, references, imports, and an optional parent scope *)
type scope =
  { declarations : declset;
    references : StringSet.t;
    imports : StringSet.t;
    parent_scope : scope_id option
  }

(* Name of a scope *)
type scope_name = string option

(* Top-level scope: Mapping from scope IDs to scopes, and references to scope IDs.*)
type scope_graph = (scope intmap * scope_id stringmap)

let lookup_containing_scope ref (_, ref_map) = StringMap.find ref ref_map

(* Gets a list of scopes from the scope graph *)
let get_scopes sg = sg

(* Gets a list of declarations from a scope *)
let get_declarations scope = scope.declarations

(* Gets a list of references from a scope *)
let get_references scope = scope.references

let get_imports scope = scope.imports

let get_parent scope = scope.parent_scope

let lookup_scope scope_id (scope_map, _) = IntMap.find scope_id scope_map

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
  { scope with declarations = DeclSet.add decl scope.declarations }

let add_reference scope reference =
  { scope with references = StringSet.add reference scope.references }

let add_import scope import =
  { scope with imports = StringSet.add import scope.imports }

let add_ref_scope_mapping : reference -> scope_id -> scope_graph -> scope_graph =
  fun ref scope_id (scope_map, ref_map) ->
    (scope_map, StringMap.add ref scope_id ref_map)

let new_scope parent =
  { declarations = DeclSet.empty;
    references = StringSet.empty;
    imports = StringSet.empty;
    parent_scope = parent }

let add_scope scope_id scope (scope_map, ref_map) =
  (IntMap.add scope_id scope scope_map, ref_map)

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
        let new_sg1 = add_ref_scope_mapping x scope_id new_sg in
        {< scope = scope1; scope_graph = new_sg1 >}

  method phrasenode = function
    | `Var n ->
        (* Add to references for current scope *)
        {< scope = add_reference scope n;
           scope_graph = add_ref_scope_mapping n scope_id scope_graph>}
    | `QualifiedVar ns ->
        self#qn_add_references ns
    | pn -> super#phrasenode pn

  (* *should* be the same for types, but if not, we can distinguish
   * type declarations from term declarations *)
  method datatype = function
    | `TypeVar (n, _, _) ->
        (* Add to references for current scope *)
        {< scope = add_reference scope n;
           scope_graph = add_ref_scope_mapping n scope_id scope_graph>}
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
        let sg1 = add_ref_scope_mapping n scope_id scope_graph in
        {< scope = (add_import scope1 n); scope_graph = sg1 >}
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
  let o = (construct_sg (new_scope None) (IntMap.empty, StringMap.empty) 0)#program prog in
  o#get_scope_graph

(* Print DOT file for scope graph *)
let show_scope_graph (sg, _) =
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
    (show_decls (DeclSet.elements scope.declarations)) ^
    (show_references (StringSet.elements scope.references)) ^
    (show_imports (StringSet.elements scope.imports)) ^
    (show_parent scope.parent_scope) in
  "digraph G {\n" ^
    String.concat ""
      (List.map (fun (s_id, s) -> show_scope s s_id) (IntMap.bindings sg))
      ^ "}"

let make_and_print_scope_graph prog =
  let sg = create_scope_graph prog in
  printf "%s\n" (show_scope_graph sg)

let string_set_map f =
  StringSet.fold (fun s -> StringSet.add (f s)) StringSet.empty

let decl_set_map f =
  DeclSet.fold (fun s -> DeclSet.add (f s)) DeclSet.empty

(* Shadowing Operator *)
let shadow : DeclSet.t -> DeclSet.t -> scope_graph -> Uniquify.unique_ast -> DeclSet.t
  = fun e1 e2 sg unique_ast ->
  (* Next, make a list of pairs of (unique, non-unique) names for e1 and e2 *)
  let annotate_decl_set ds = DeclSet.fold
    (fun x -> AnnotatedDeclSet.add (x, Uniquify.lookup_var (fst x) unique_ast)) ds AnnotatedDeclSet.empty in
  let combined_e1 = annotate_decl_set e1 in
  let combined_e2 = annotate_decl_set e2 in

  (* Use sets of the plain names to compute the overlapping set via intersection *)
  let get_plain_names ds =
    AnnotatedDeclSet.fold (fun ((n, _), _) s -> StringSet.add n s) ds StringSet.empty in
  let set_plain_e1 = get_plain_names combined_e1 in
  let set_plain_e2 = get_plain_names combined_e2 in
  let overlapping_bindings = StringSet.inter set_plain_e1 set_plain_e2 in

  (* Result is the union of sets where the plain var isn't in the overlapping set *)
  let filtered_annotated_decl_set = AnnotatedDeclSet.filter
      (fun (_, x_plain) -> not (StringSet.mem x_plain overlapping_bindings)) combined_e2 in

  let filtered_decl_set =
    AnnotatedDeclSet.fold (fun n acc -> DeclSet.add (fst n) acc)
      filtered_annotated_decl_set DeclSet.empty in
  DeclSet.union e1 filtered_decl_set

(* Name resolution *)
let resolve_name : string -> scope_graph -> Uniquify.unique_ast -> DeclSet.t
  = fun ref sg u_ast ->
  let find_scope scope_id = lookup_scope scope_id sg in
  (* Aaaaaand here we go *)
  let rec resolve_name_inner ref_name seen_imports =
    let containing_scope_id = lookup_containing_scope ref_name sg in
    let containing_scope = find_scope containing_scope_id in
    visible_decls  (StringSet.add ref_name seen_imports) IntSet.empty containing_scope_id

  (* EnvV *)
  and visible_decls : stringset -> intset -> int -> DeclSet.t =
    fun seen_imports seen_scopes scope_id ->
    let envl_s = local_decls seen_imports seen_scopes scope_id in
    let envp_s = parent_decls seen_imports seen_scopes scope_id in
    shadow envl_s envp_s sg u_ast

  (* EnvL *)
  and local_decls seen_imports seen_scopes scope_id =
    let envd_s = scope_decls seen_imports seen_scopes scope_id in
    let envi_s = imported_decls seen_imports seen_scopes scope_id in
    shadow envd_s envi_s sg u_ast

  (* EnvD *)
  and scope_decls seen_imports seen_scopes scope_id =
    if IntSet.mem scope_id seen_scopes then DeclSet.empty
    else (find_scope scope_id).declarations

  (* EnvI *)
  and imported_decls seen_imports seen_scopes scope_id =
    if IntSet.mem scope_id seen_scopes then DeclSet.empty else
    let scope = find_scope scope_id in
    let unseen_imports = StringSet.diff scope.imports seen_imports in
    (* Next up: get the associated set IDs for all import declarations *)
    let import_scope_ids =
      StringSet.fold (fun i acc ->
        let resolved_import_set = resolve_name_inner i seen_imports in
        (* For each resolved import declaration, add the associated scope ID to the set *)
        let scope_ids = DeclSet.fold (fun i_decl ->
          match i_decl with
            | (_decl_name, Some (scope_id)) -> IntSet.add scope_id
            | (_decl_name, None) ->
                failwith "Error in name resolution: import
                  reference resolved to non-module declaration") resolved_import_set IntSet.empty in
        (* Union with the accumulator *)
        IntSet.union acc scope_ids) scope.imports IntSet.empty in
    (* Finally, union the local environments, and we're done. *)
    let new_seen_scopes = IntSet.add scope_id seen_scopes in
    IntSet.fold (fun i_scope acc ->
      let i_decls = local_decls seen_imports new_seen_scopes i_scope in
      DeclSet.union i_decls acc
    ) import_scope_ids DeclSet.empty

  (* EnvP *)
  and parent_decls seen_imports seen_scopes scope_id =
    if IntSet.mem scope_id seen_scopes then DeclSet.empty
    else
      match (find_scope scope_id).parent_scope with
        | None -> DeclSet.empty
        | Some parent_scope_id ->
            visible_decls seen_imports (IntSet.add scope_id seen_scopes) parent_scope_id in
  resolve_name_inner ref StringSet.empty


