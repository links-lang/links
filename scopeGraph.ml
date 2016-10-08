open Utility
open Printf
open ModuleUtils

(* open Sugartypes *)
type name = Sugartypes.name
type fq_name = Sugartypes.name
type unique_name = Sugartypes.name

type scope_id = int

(* Declaration -- generally a binder. Contains name, position, and optional
 * named scope reference *)
type declaration = (name * scope_id option)

(* Name of a reference (function application, argument, etc.) *)
type reference = name

(* Name of an import (either in-module or out-of-module *)
type import  = name

module Decl = struct
  type t = declaration
  (* Deriving makes it nigh-on impossible to use Pervasives.compare. Great.
   * This is an ungodly awful hack. *)
  let compare = Pervasives.compare
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

type path = name list

(* Top-level scope: Mapping from scope IDs to scopes, and references to scope IDs.*)
type scope_graph =
  { (* Maps scope IDs to scopes *)
    scope_map : scope intmap;
    (* Maps references to the scopes which contain them *)
    reference_map : scope_id stringmap;
    (* Maps declarations to their fully-qualified paths *)
    path_map : path stringmap
  }

let new_scope_graph = {
  scope_map = IntMap.empty;
  reference_map = StringMap.empty;
  path_map = StringMap.empty
}

type resolution_result = [
  | `AmbiguousResolution of name list
  | `SuccessfulResolution of name
  | `UnsuccessfulResolution
]

let lookup_containing_scope ref sg =
  try
    StringMap.find ref sg.reference_map
  with Notfound.NotFound _ ->
    failwith (sprintf "Internal error in lookup_containing_scope: %s not found\n" ref)

let lookup_scope scope_id sg =
  try
    IntMap.find scope_id sg.scope_map
  with Notfound.NotFound _ ->
    failwith (sprintf "Internal error in lookup_scope: %d not found\n" scope_id)

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
  fun ref scope_id sg ->
    { sg with reference_map = StringMap.add ref scope_id sg.reference_map }

let new_scope parent =
  { declarations = DeclSet.empty;
    references = StringSet.empty;
    imports = StringSet.empty;
    parent_scope = parent }

let add_scope scope_id scope sg =
  { sg with scope_map = IntMap.add scope_id scope sg.scope_map }

let add_decl_path decl_name path sg =
  { sg with path_map = StringMap.add decl_name path sg.path_map }

let get_decl_path decl_name sg =
  StringMap.find decl_name sg.path_map

(* Given a declaration name and a path, creates a string of plain names.
 * For example, make_resolved_plain_name "x_1"  sg u_ast would result in
 *  A.B.C.x, assuming a mapping from x_1 |-> [A_2, B_3, C_4] in the sg's path map,
 *  and maps from x_1 |-> x, A_2 |-> A etc. in the u_ast *)
let make_resolved_plain_name decl_name sg u_ast =
  let decl_path = get_decl_path decl_name sg in
  let plain_decl_name = Uniquify.lookup_var decl_name u_ast in
  let plain_paths = List.map (fun p -> Uniquify.lookup_var p u_ast) decl_path in
  String.concat module_sep (plain_paths @ [plain_decl_name])

(* Jettisoned (for now) type stuff. This will need to be in a separate graph.
    | `Type (n, _, _) ->
        (* I suppose it depends on whether we want types to behave like function or Var bindings.
         * Let's treat them like defs (i.e. function bindings) for now *)
        {< scope = add_declaration scope (plain_decl n) >}
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
*)

let rec get_last_list_element = function
  | [] -> failwith "empty list in get_last_list_element"
  | [x] -> x
  | _ :: xs -> get_last_list_element xs

(* Superclass to handle SG mutations, references, and that sorta jazz *)
class sg_fold init_sg_ref init_scope_id init_path =
  object(self)
    inherit SugarTraversals.fold as super
    val sg = init_sg_ref

    (* Current Scope ID properties *)
    val scope_id = init_scope_id
    method get_scope_id = scope_id
    method set_scope_id sid = {< scope_id = sid >}

    (* Current path properties *)
    val path = init_path
    method get_path = path
    method set_path new_path = {< path = new_path >}

    method get_sg = !sg

    (* Scope graph mutators *)
    method create_scope parent_opt =
      let sg_inst = !sg in
      let created_scope_id = get_scope_num () in
      let created_scope = new_scope parent_opt in
      let sg_inst = add_scope created_scope_id created_scope sg_inst in
      sg := sg_inst;
      created_scope_id

    method add_ref_to_scope ref_name scope_id =
      (* Dereference and get scope *)
      let sg_inst = !sg in
      let scope = lookup_scope scope_id sg_inst in
      (* Add reference to scope, and add mapping to containing scope *)
      let scope = add_reference scope ref_name in
      let sg_inst = add_ref_scope_mapping ref_name scope_id sg_inst in
      let sg_inst = add_scope scope_id scope sg_inst in
      sg := sg_inst

    method add_decl_to_scope decl path scope_id =
      let sg_inst = !sg in
      let scope = lookup_scope scope_id sg_inst in
      let scope = add_declaration scope decl in
      let sg_inst = add_scope scope_id scope sg_inst in
      let sg_inst = add_decl_path (fst decl) path sg_inst in
      sg := sg_inst

    method add_import_to_scope import scope_id =
      let sg_inst = !sg in
      let scope = lookup_scope scope_id sg_inst in
      let scope = add_import scope import in
      let sg_inst = add_scope scope_id scope sg_inst in
      sg := sg_inst

    (* Qualified names *)
    method qualified_name = function
      | [] -> self
      | [x] ->
          self#add_ref_to_scope x scope_id;
          self
      | x :: xs ->
          self#add_ref_to_scope x scope_id;
          let anon_scope_id = self#create_scope None in
          (* CHECK: Does this also need to modify the path? *)
          self#add_import_to_scope x anon_scope_id;
          {< scope_id = anon_scope_id >}#qualified_name xs

    (* Phrases are handled the same in either binding or definition mode. *)
    method phrasenode = function
      | `Var n -> self#add_ref_to_scope n scope_id; self
      | `QualifiedVar ns -> self#qualified_name ns
      | `Switch (phr, cases, _dtopt) ->
          let _ = self#phrase phr in
          self#list (fun _ (pat, phr) ->
            let pat_scope_id = self#create_scope (Some scope_id) in
            let o = {< scope_id = pat_scope_id >} in
            let o = o#pattern pat in
            let _ = o#phrase phr in
            self) cases
      | pn -> super#phrasenode pn

    method funlit (pat_list_list, phr) =
      (* Create a new scope for the phrase, and add the bindings to it *)
      (* I assume the list list thing is for curried arguments. Lets say that
       * each pattern list gets its own scope. *)
      let o_plist_list = self#list (fun o plist ->
        let plist_scope_id = self#create_scope (Some o#get_scope_id) in
        let o_plist = self#set_scope_id plist_scope_id in
        o_plist#list (fun o -> o#pattern) plist) pat_list_list in
      let _ = o_plist_list#phrase phr in
      self

    method binder (n, _, _) =
      self#add_decl_to_scope (plain_decl n) path scope_id;
      self

    method bindingnode = function
      | `Funs _ -> assert false
      | `Fun ((fn_name, _, _), _lin, (_tyvars, fn_funlit), _loc, _dtopt) ->
           self#add_decl_to_scope (plain_decl fn_name) path scope_id;
           let _ = self#funlit fn_funlit in
           self
      | `Infix -> self
      | `Exp p -> self#phrase p
      | `Foreign ((bnd_name, _, _), _name, _dt) ->
          self#add_decl_to_scope (plain_decl bnd_name) path scope_id;
          self
      | bn -> super#bindingnode bn
  end

  (* Binding policies for inside blocks (inside functions, for example)
   * differ to TL bindings since they aren't accessible by others. Here, we
   * follow the standard "define-before-use" scoping rules. *)
  (* At the top-level (and within modules), bindings are unordered sets. *)
let rec binding_sg_fold init_sg_ref init_scope_id init_path =
  object (self)
    inherit sg_fold init_sg_ref init_scope_id init_path as super
    method phrasenode = function
      | `Block (bs, p) ->
          let block_scope_id = self#create_scope (Some scope_id) in
          let o = phrase_sg_fold sg block_scope_id path in
          let o = List.fold_left (fun o b -> (o#binding b)) o bs in
          let _ = o#phrase p in
          self
      | pn -> super#phrasenode pn

    method bindingnode = function
        | `Val (_tvs, pat, phr, _loc, _dtopt) ->
            (* Process phrase in this scope *)
            let _ = self#phrase phr in
            (* Pattern bindings are added to this scope *)
            self#pattern pat
        | `QualifiedImport ns ->
            let _ = self#qualified_name ns in
            super#add_import_to_scope (get_last_list_element ns) (self#get_scope_id);
            self
        | `Module (n, bs) ->
           (* Create new scope for module *)
           let new_scope_id = self#create_scope (Some scope_id) in
           (* Add new declaration *)
           self#add_decl_to_scope (annotated_decl n new_scope_id) path scope_id;
           (* Process inner block with new parameters *)
           let o = binding_sg_fold init_sg_ref new_scope_id (n :: self#get_path) in
           let o = List.fold_left (fun o -> o#binding) o bs in
           (* Back to our parameters *)
           self
        | bn -> super#bindingnode bn
  end

and phrase_sg_fold init_sg_ref init_scope_id init_path =
    object(self)
      inherit sg_fold init_sg_ref init_scope_id init_path as super

      method phrasenode = function
        | `Block (bs, p) ->
            let block_scope_id = self#create_scope (Some scope_id) in
            let o = phrase_sg_fold sg block_scope_id path in
            let o = List.fold_left (fun o -> o#binding) o bs in
            let _ = o#phrase p in
            self
        | pn -> super#phrasenode pn

      method bindingnode = function
        | `Val (_tvs, pat, phr, _loc, _dtopt) ->
            (* Process phrase in this scope *)
            let _ = self#phrase phr in
            (* Create new scope for pattern bindings *)
            let new_scope_id = super#create_scope (Some scope_id) in
            let new_o = super#set_scope_id new_scope_id in
            new_o#pattern pat
        | `QualifiedImport ns ->
            let _ = self#qualified_name ns in
            let new_scope_id = super#create_scope (Some self#get_scope_id) in
            let new_o = self#set_scope_id new_scope_id in
            let _ = super#add_import_to_scope (get_last_list_element ns) new_scope_id in
            new_o
        | `Module (n, bs) ->
          (* Create new scope for module *)
          let new_scope_id = self#create_scope (Some scope_id) in
          (* Add new declaration *)
          self#add_decl_to_scope (annotated_decl n new_scope_id) path scope_id;
          (* Process inner block with new parameters *)
          let o_module_inner = binding_sg_fold init_sg_ref new_scope_id (n :: self#get_path) in
          let _ = o_module_inner#list (fun o -> o#binding) bs in
          (* Back to our parameters *)
          self
        | bn -> super#bindingnode bn
    end

let construct_sg_imp prog =
    let sg = new_scope_graph in
    let init_scope_id = get_scope_num () in
    let init_scope = new_scope None in
    let sg = add_scope init_scope_id init_scope sg in
    let sg_ref = ref sg in
    let o = (phrase_sg_fold sg_ref init_scope_id [])#program prog in
    o#get_sg

let create_scope_graph = construct_sg_imp

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
    (show_decls (DeclSet.elements scope.declarations)) ^
    (show_references (StringSet.elements scope.references)) ^
    (show_imports (StringSet.elements scope.imports)) ^
    (show_parent scope.parent_scope) in
  "digraph G {\n" ^
    String.concat ""
      (List.map (fun (s_id, s) -> show_scope s s_id) (IntMap.bindings sg.scope_map))
      ^ "}"

(* Shadowing Operator *)
let shadow : DeclSet.t -> DeclSet.t -> scope_graph -> Uniquify.unique_ast -> DeclSet.t
  = fun e1 e2 sg unique_ast ->
  (* Next, make a list of pairs of (unique, non-unique) names for e1 and e2 *)
  let annotate_decl_set ds = DeclSet.fold
    (fun x ->
      AnnotatedDeclSet.add (x, Uniquify.lookup_var (fst x) unique_ast))
        ds AnnotatedDeclSet.empty in
  let combined_e1 = annotate_decl_set e1 in
  let combined_e2 = annotate_decl_set e2 in

  (* Use sets of the plain names to compute the overlapping set via intersection *)
  let get_plain_names ds =
    AnnotatedDeclSet.fold (fun ((_, _), n) s -> StringSet.add n s) ds StringSet.empty in
  let set_plain_e1 = get_plain_names combined_e1 in
  let set_plain_e2 = get_plain_names combined_e2 in
  let annotated_decl_list =
    (AnnotatedDeclSet.elements combined_e1) @ (AnnotatedDeclSet.elements combined_e2) in
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
  = fun ref_name_outer sg u_ast ->
  let find_scope scope_id = lookup_scope scope_id sg in
  (* Aaaaaand here we go *)
  let rec resolve_name_inner ref_name seen_imports =
    let containing_scope_id = lookup_containing_scope ref_name sg in
    let plain_name = Uniquify.lookup_var ref_name u_ast in
    (* printf "Resolving %s. Plain name: %s. Containing scope: %d\n" ref_name plain_name containing_scope_id; *)
    let vis_decls = visible_decls  (StringSet.add ref_name seen_imports) IntSet.empty containing_scope_id in
    (* Finally filter out irrelevant ones *)
    DeclSet.filter (fun (n, _d) -> (Uniquify.lookup_var n u_ast) = plain_name) vis_decls

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
  and scope_decls _seen_imports seen_scopes scope_id =
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
        (* Given an import, resolve it, returning a set of possible resolutions *)
        let resolved_import_set = resolve_name_inner i seen_imports in
        (* For each resolved import declaration, add the associated scope ID to the set *)
        let scope_ids = DeclSet.fold (fun i_decl ->
          match i_decl with
            | (_decl_name, Some (scope_id)) -> IntSet.add scope_id
            | (decl_name, None) ->
                let err = sprintf "Error in name resolution: import %s resolved to non-module decl %s\n" i decl_name in
                failwith err) resolved_import_set IntSet.empty in
        (* Union with the accumulator *)
        IntSet.union acc scope_ids) unseen_imports IntSet.empty in
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
  (* Top-level *)
  resolve_name_inner ref_name_outer StringSet.empty


let show_resolved_names sg unique_ast =
  let show_resolved_name ref_name =
    (* Firstly, get the resolution result, which will get us a list of
     * declarations that this reference could map to. *)
    let resolution_results = DeclSet.elements (resolve_name ref_name sg unique_ast) in
    (* Next up, define a function to print each of these names in a list. *)
    let print_single_resolution_result (decl_name, _) =
      (* Firstly, grab the plain name from the SG and UAST *)
      let plain_fqn = make_resolved_plain_name decl_name sg unique_ast in
      (* Next, format it *)
      sprintf "(%s, FQ: %s)" decl_name plain_fqn in
    (* Now, print the result. *)
    let formatted_resolution_list = List.map print_single_resolution_result resolution_results in
    sprintf "%s ----> %s\n" ref_name (print_list formatted_resolution_list) in
  let name_list = List.map fst (StringMap.bindings sg.reference_map) in
  printf "Reference list: %s\n" (print_list name_list);
  String.concat "" (List.map show_resolved_name name_list)

let make_and_print_scope_graph unique_ast =
  let prog = Uniquify.get_ast unique_ast in
  let sg = create_scope_graph prog in
  printf "====== SCOPE GRAPH ======\n";
  printf "%s\n" (show_scope_graph sg);
  printf "====== RESOLUTIONS ======\n";
  printf "%s\n" (show_resolved_names sg unique_ast)

let resolve_reference ref sg u_ast =
  let resolution_results = DeclSet.elements (resolve_name ref sg u_ast) in
  match resolution_results with
    | [] -> `UnsuccessfulResolution
    | [(x, _)] -> `SuccessfulResolution x
    | x::xs -> `AmbiguousResolution (List.map fst (x::xs))

