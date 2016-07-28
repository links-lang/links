open Utility
open Sugartypes
open Printf
open ModuleUtils

type prog_map = program StringMap.t
type filename = string

let module_file_name module_name =
  match (Str.split (Str.regexp module_sep) module_name) with
    | [] -> failwith "Internal error: empty list in module_file_name"
    | (x::_xs) -> x

(* Traversal to find module import references in the current file *)
let rec find_module_refs prefix init_seen_modules init_import_candidates init_binding_stack =
object(self)
  inherit SugarTraversals.fold as super
  (* Module definitions we've seen in the file *)
  val seen_modules = init_seen_modules
  val binding_stack = init_binding_stack
  method add_seen_module x = {< seen_modules = StringSet.add x seen_modules >}
  method get_seen_modules = seen_modules

  method push_module name =
    {< binding_stack = (`OpenStatement name) :: binding_stack >}


  (* Imports that are not resolvable in the current file *)
  val import_candidates = init_import_candidates
  method add_import_candidate x =
    {< import_candidates = StringSet.add x import_candidates >}
  method get_import_candidates = import_candidates

  method bindingnode = function
    | `Module (module_name, block) ->
        let new_prefix = prefixWith module_name prefix in
        (* Add fully-qualified module to seen module list *)
        (* Add fully-qualified module to module stack *)
        let o = self#add_seen_module new_prefix in
        let o1 =
          (find_module_refs new_prefix o#get_seen_modules o#get_import_candidates
            ((`OpenStatement new_prefix)::binding_stack))#phrase block in
        {< seen_modules = o1#get_seen_modules; import_candidates = o1#get_import_candidates >}
    | `Import name ->
        let scope_res = moduleInScope seen_modules binding_stack name in
        (match scope_res with
          | Some(fully_qual) ->
              self#push_module fully_qual
          | None ->
              (* If we can't resolve it internally, assume that it
               * is an external dependency *)
              self#add_import_candidate (module_file_name name))
    | b -> super#bindingnode b

  method phrasenode = function
    | `Block (bs, p) ->
        (* Recursively process bindings / phrase *)
        let o1 =
          List.fold_left (fun o_acc binding ->
            o_acc#binding binding) self bs in
        (* Restore the previous binding stack by making a copy of this object w/ new
         * seen modules / bindings *)
        let o2 = o1#phrase p in
        {< seen_modules = o2#get_seen_modules; import_candidates = o2#get_import_candidates >}
    | p -> super#phrasenode p
end


let find_external_refs prog =
  StringSet.elements ((find_module_refs "" StringSet.empty StringSet.empty [])#program prog)#get_import_candidates

let assert_no_cycles = function
  | [] -> ()
  | [x]::ys -> ()
  | (x::xs)::ys -> failwith ("Error -- cyclic dependencies: " ^ (String.concat ", " (x :: xs)))

let print_sorted_deps xs =
  print_list (List.map print_list xs)

let rec add_module_bindings deps dep_map =
  match deps with
    | [] -> []
    (* Don't re-inline bindings of base module *)
    | [""]::ys -> add_module_bindings ys dep_map
    | [module_name]::ys ->
      let (bindings, _) = StringMap.find module_name dep_map in
      (* TODO: Fix dummy position to be more meaningful, if necessary *)
      (`Module (module_name,
        (`Block (bindings, (`RecordLit ([], None), Sugartypes.dummy_position)),
        Sugartypes.dummy_position)
        ), Sugartypes.dummy_position) :: (add_module_bindings ys dep_map)
    | _ -> failwith "Internal error: impossible pattern in add_module_bindings"

let module_filename module_name =
  (String.uncapitalize module_name) ^ ".links"

let parse_file module_name =
  let filename = module_filename module_name in
  let (prog, _) = Parse.parse_file Parse.program filename in
  prog

let print_external_deps prog =
  let external_deps = find_external_refs prog in
  printf "External dependencies:\n%s\n" (print_list external_deps)

let rec add_dependencies_inner module_name module_prog visited deps dep_map =
  if StringSet.mem module_name visited then (visited, [], dep_map) else
  let visited1 = StringSet.add module_name visited in
  let dep_map1 = StringMap.add module_name module_prog dep_map in

  (* Fistly, get import candidates *)
  let ics = find_external_refs module_prog in
  (* Next, run the dependency analysis on each one to get us an adjacency list *)
  List.fold_right (
    fun name (visited_acc, deps_acc, dep_map_acc) ->
      let prog = parse_file name in
      let (visited_acc', deps_acc', dep_map_acc') = add_dependencies_inner name prog visited_acc deps_acc dep_map_acc in
      (visited_acc', deps_acc @ deps_acc', dep_map_acc')
  ) ics (visited1, (module_name, ics) :: deps, dep_map1)


let unique_list xs =
  StringSet.elements (StringSet.of_list xs)

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
  (* printf "Sorted deps: %s\n" (print_sorted_deps sorted_deps); *)

  (* Now, build up binding list where each opened dependency is mapped to a `Module containing
   * its list of inner bindings. *)
  (* FIXME: This isn't reassigning positions! What we'll want is to retain the positions, but modify
   * the position data type to keep track of the module filename we're importing from. *)
  let module_bindings = add_module_bindings sorted_deps dep_binding_map in
  let transformed_prog = (module_bindings @ bindings, phrase) in
  (* printf "After chaser transformation: \n%s\n" (Sugartypes.Show_program.show transformed_prog); *)
  (* Finally, add this to the start of the original list of bindings *)
  transformed_prog


