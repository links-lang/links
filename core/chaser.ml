open Utility
open SourceCode
open ModuleUtils
open Sugartypes

(* Helper functions *)
(* Helper function: given top-level module name, maps to expected filename *)
let top_level_filename module_name =
  (String.uncapitalize_ascii module_name) ^ ".links"

let _print_sorted_deps xs =
  print_list (List.map print_list xs)

let assert_no_cycles = function
  | [] -> ()
  | []::_ys -> ()
  | [_x]::_ys -> ()
  | (x::xs)::_ys -> raise (Errors.module_error ("Error -- cyclic dependencies: " ^ (String.concat ", " (x :: xs))))

(* Traversal to find module import references in the current file *)
let rec find_module_refs mt path ht =
object(self)
  inherit SugarTraversals.fold as super
  (* Imports that are not resolvable in the current file *)
  val import_candidates = StringSet.empty
  method add_import_candidate x =
    {< import_candidates = StringSet.add x import_candidates >}
  method add_import_candidates ic_set =
    {< import_candidates = StringSet.union ic_set import_candidates >}
  method get_import_candidates = import_candidates

  val shadow_table = ht
  method get_shadow_table = shadow_table
  method bind_shadow name fqn =
    {< shadow_table = shadow_binding name fqn shadow_table >}

  method bind_open name fqn =
    let (shadow_table, _) = shadow_open name fqn mt shadow_table shadow_table in
    {< shadow_table = shadow_table >}

  method! bindingnode = function
    | Import { path = ns; _ } ->
        (* Try to resolve the import; if not, add to ICs list *)
        let lookup_ref = List.hd ns in
        (try
           let _ = StringMap.find lookup_ref shadow_table in
           self
         with
           _ -> self#add_import_candidate lookup_ref)
    | Module { binder; members } ->
        let name = Binder.to_name binder in
        let new_path = path @ [name] in
        let fqn = lst_to_path new_path in
        let o = self#bind_shadow name fqn in
        let shadow_ht = o#get_shadow_table in
        (* Now, recursively check the module, with this one in scope *)
        let o_bindings =
          List.fold_left (fun o b -> o#binding b) (find_module_refs mt new_path shadow_ht) members in
        let ics = o_bindings#get_import_candidates in
        (o#bind_open name fqn)#add_import_candidates ics
    | bn -> super#bindingnode bn
end


let find_external_refs prog module_table =
  StringSet.elements
    ((find_module_refs module_table [] StringMap.empty)#program prog)#get_import_candidates

let rec add_module_bindings deps dep_map =
  match deps with
    | [] -> []
    (* Don't re-inline bindings of base module *)
    | [""]::ys -> add_module_bindings ys dep_map
    | [name]::ys ->
      (try
         let (members, _) = StringMap.find name dep_map in
         let binder = SourceCode.WithPos.make (Binder.make ~name ()) in
        WithPos.make (Module { binder; members }) :: (add_module_bindings ys dep_map)
      with Notfound.NotFound _ ->
        (raise (Errors.internal_error ~filename:"chaser.ml"
          ~message:(Printf.sprintf "Could not find %s in dependency map containing keys: %s\n"
          name (print_list (List.map fst (StringMap.bindings dep_map)))))));
    | _ ->
        raise (Errors.internal_error ~filename:"chaser.ml"
          ~message:"Impossible pattern in add_module_bindings")


let rec add_dependencies_inner module_name module_prog visited deps dep_map =
  if StringSet.mem module_name visited then (visited, [], dep_map) else
  let visited1 = StringSet.add module_name visited in
  let dep_map1 = StringMap.add module_name module_prog dep_map in

  (* With this, get import candidates *)
  let mt = create_module_info_map module_prog in
  let ics = find_external_refs module_prog mt in
  (* Next, run the dependency analysis on each one to get us an adjacency list *)
  List.fold_left (
    fun (visited_acc, deps_acc, dep_map_acc) name ->
      (* Given the top-level module name, try and parse wrt the paths *)
      let filename = top_level_filename name in
      let (prog, pos_ctx) = try_parse_file filename in
      let prog = (ResolvePositions.resolve_positions pos_ctx)#program prog in
      let (visited_acc', deps_acc', dep_map_acc') =
        add_dependencies_inner name prog visited_acc [] dep_map_acc in
      (visited_acc', deps_acc @ deps_acc', dep_map_acc')
  ) (visited1, (module_name, ics) :: deps, dep_map1) ics

(* Top-level function: given a module name + program, return a program with
 * all necessary files added to the binding list as top-level modules. *)
let add_dependencies module_prog =
  let (bindings, phrase) = module_prog in
  (* Firstly, get the dependency graph *)
  let (_, deps, dep_binding_map) =
    add_dependencies_inner "" module_prog (StringSet.empty) [] (StringMap.empty) in
  (* Next, do a topological sort to get the dependency graph and identify cyclic dependencies *)
  let sorted_deps = Graph.topo_sort_sccs deps in
  (* Each entry should be *precisely* one element (otherwise we have cycles) *)
  assert_no_cycles sorted_deps;
  (* Now, build up binding list where each opened dependency is mapped to a Module containing
   * its list of inner bindings. *)
  (* FIXME: This isn't reassigning positions! What we'll want is to retain the positions, but modify
   * the position data type to keep track of the module filename we're importing from. *)
  let module_bindings = add_module_bindings sorted_deps dep_binding_map in
  (module_bindings @ bindings, phrase)

let add_dependencies_sentence = function
  | Definitions defs -> Definitions (fst (add_dependencies (defs, None)))
  | s -> s
