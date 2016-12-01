open Utility
open Sugartypes
open Printf
open ModuleUtils
open Uniquify
open ScopeGraph

type prog_map = program StringMap.t
type filename = string
(* Helper functions *)

(* Helper function: given top-level module name, maps to expected filename *)
let top_level_filename module_name =
  (String.uncapitalize module_name) ^ ".links"

let print_sorted_deps xs =
  print_list (List.map print_list xs)

(* Given a module name and unique AST, try and locate / parse the module file *)
let parse_module module_name =
  let (prog, _) = try_parse_file module_name in
  prog

let assert_no_cycles = function
  | [] -> ()
  | [x]::ys -> ()
  | (x::xs)::ys -> failwith ("Error -- cyclic dependencies: " ^ (String.concat ", " (x :: xs)))

let unique_list xs =
  StringSet.elements (StringSet.of_list xs)


let can_resolve_name name sg u_ast =
  match ScopeGraph.resolve_reference name sg u_ast with
    | `SuccessfulResolution _ -> true
    | `AmbiguousResolution _ -> true
    | `UnsuccessfulResolution -> false

(* Given a fully-qualified variable or import, tries to resolve the first segment.
 * For example, given [A_1, B_2, x_3], tries to resolve A_1. If successful, it's
 * internally resolvable and we're OK. If not, then we'll need to try and import it.
 *)
let rec can_resolve_qual_name qual_name sg u_ast =
  match qual_name with
    | [] -> failwith "INTERNAL ERROR: Empty qualified name; this should never happen"
    | x::xs ->
        (* The head will be the module we'll want to try and resolve. *)
        can_resolve_name x sg u_ast

(* Traversal to find module import references in the current file *)
let rec find_module_refs sg ty_sg u_ast init_import_candidates =
object(self)
  inherit SugarTraversals.fold as super
  (* Imports that are not resolvable in the current file *)
  val import_candidates = init_import_candidates
  method add_import_candidate x =
    {< import_candidates = StringSet.add x import_candidates >}
  method get_import_candidates = import_candidates

  method bindingnode = function
    | `QualifiedImport ns ->
        if can_resolve_qual_name ns sg u_ast then self else
          let to_add = Uniquify.lookup_var (List.hd ns) u_ast in
          self#add_import_candidate to_add
    | bn -> super#bindingnode bn
end


let find_external_refs sg ty_sg u_ast =
  let prog = Uniquify.get_ast u_ast in
  StringSet.elements ((find_module_refs sg ty_sg u_ast StringSet.empty)#program prog)#get_import_candidates

let rec add_module_bindings deps dep_map =
  match deps with
    | [] -> []
    (* Don't re-inline bindings of base module *)
    | [""]::ys -> add_module_bindings ys dep_map
    | [module_name]::ys ->
      try
        let (bindings, _) = StringMap.find module_name dep_map in
        (* TODO: Fix dummy position to be more meaningful, if necessary *)
        (`Module (module_name, bindings), Sugartypes.dummy_position) :: (add_module_bindings ys dep_map)
      with Notfound.NotFound _ ->
        failwith "Trying to find %s in dep map containing keys: %s\n" module_name (print_list (List.map fst (StringMap.bindings dep_map)));
    | _ -> failwith "Internal error: impossible pattern in add_module_bindings"


let rec add_dependencies_inner module_name module_prog visited deps dep_map =
  if StringSet.mem module_name visited then (visited, [], dep_map) else
  let visited1 = StringSet.add module_name visited in
  let dep_map1 = StringMap.add module_name module_prog dep_map in

  (* Unique AST and scope graph for plain program*)
  let u_ast = Uniquify.uniquify_ast module_prog in
  let sg = ScopeGraph.create_scope_graph (Uniquify.get_ast u_ast) in
  let ty_sg = ScopeGraph.create_type_scope_graph (Uniquify.get_ast u_ast) in
  (* With this, get import candidates *)
  let ics = find_external_refs sg ty_sg u_ast in
  (* Next, run the dependency analysis on each one to get us an adjacency list *)
  List.fold_right (
    fun name (visited_acc, deps_acc, dep_map_acc) ->
      (* Given the top-level module name, try and parse wrt the paths *)
      let filename = top_level_filename name in
      let prog = parse_module filename in
      let (visited_acc', deps_acc', dep_map_acc') =
        add_dependencies_inner name prog visited_acc deps_acc dep_map_acc in
      (visited_acc', deps_acc @ deps_acc', dep_map_acc')
  ) ics (visited1, (module_name, ics) :: deps, dep_map1)


(* Top-level function: given a module name + program, return a program with
 * all necessary files added to the binding list as top-level modules. *)
let add_dependencies module_name module_prog =
  let (bindings, phrase) = module_prog in
  (* Firstly, get the dependency graph *)
  let (_, deps, dep_binding_map) =
    add_dependencies_inner module_name module_prog (StringSet.empty) [] (StringMap.empty) in
  (* Next, do a topological sort to get the dependency graph and identify cyclic dependencies *)
  let sorted_deps = Graph.topo_sort_sccs deps in
  (* Each entry should be *precisely* one element (otherwise we have cycles) *)
  assert_no_cycles sorted_deps;
  (* Now, build up binding list where each opened dependency is mapped to a `Module containing
   * its list of inner bindings. *)
  (* FIXME: This isn't reassigning positions! What we'll want is to retain the positions, but modify
   * the position data type to keep track of the module filename we're importing from. *)
  let module_bindings = add_module_bindings sorted_deps dep_binding_map in
  let transformed_prog = (module_bindings @ bindings, phrase) in
  (* Now, finally create a new SG and unique AST for the program with all inlined modules *)
  let u_ast = Uniquify.uniquify_ast transformed_prog in
  let u_prog = Uniquify.get_ast u_ast in
  let sg = ScopeGraph.create_scope_graph u_prog in
  let ty_sg = ScopeGraph.create_type_scope_graph u_prog in

  (sg, ty_sg, u_ast)
