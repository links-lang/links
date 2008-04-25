(*pp deriving *)
(* Facilities for determining and restructuring static call graphs *)

open Utility
open List
open Syntax


(** {1 Callgraph ordering}

    Find the sccs in a group of functions.  Whenever there's mutual
    recursion we need to type all the functions in the cycle as
    `letrec-bound'; we want to avoid doing this in all other cases to
    make everything as polymorphic as possible (and to make typing
    faster).  In such cases the bindings must be reordered so that we
    type called functions before their callers.

    The plan is as follows:
    1. Find the call graph (by analysing the rhs for free variables.)
    2. Find all the cycles (strongly-connected components) in the call graph.
    3. Collapse cycles to single nodes and perform a topological sort
       to obtain the ordering.
*)

let is_mapped_by alist x = mem_assoc x alist

(** [make_callgraph bindings] returns an alist that gives a list of
    called functions for each function in [bindings], limited to the
    definitions in bindings. *)
let make_callgraph bindings = 
  alistmap
    (fun expr -> 
       filter (is_mapped_by bindings) (StringSet.elements (freevars expr)) )
    bindings

let group_and_order_bindings_by_callgraph 
    (bindings : (string * 'a expression') list) 
    : string list list = 
  let call_graph = make_callgraph bindings in
    Graph.topo_sort_sccs call_graph

(** Find definition of a symbol in a list of definitions.
    (Perhaps this should work in the other direction, bottom-to-top?) *)
let rec defn_of symbol = function
  | Define(n, _, _, _) as expr :: _ when n = symbol -> expr
  | Module(_, Some defs, _) :: more_defs -> 
      defn_of symbol (defs@more_defs)
  | _ :: defns -> defn_of symbol defns

let find_defn_in x y = defn_of y x

(** order_exprs_by_callgraph takes a list of groupings of functions
    and returns a new, possibly finer, list of groupings of functions.
    Each of the new groupings should truly be mutually recursive and
    the groupings should be ordered in callgraph-order (but note that
    bindings are only determined within the original groupings; how
    does this work with redefined function names that are part of
    mut-rec call groups? )*)
let refine_def_groups (def_lists : 'a definition' list list) 
    : 'a definition' list list = 
  let regroup_defs defs = 
    let bindings = deflist_to_alist defs in
    let sccs = group_and_order_bindings_by_callgraph bindings in
      map (map (find_defn_in defs)) sccs 
  in
    (* Each grouping in the input will be broken down into a new list
       of groupings. We only care about the new groupings, so we
       concat_map to bring them together *)
    concat_map (function
                  | Define _ :: _ as defs -> regroup_defs defs
                  | e                     -> [e]) def_lists
      
(** Removes defs from [env] that aren't used by [expr]. *)
module DeadCode = struct
  type scc = {
    free_names : string list;
    exposed_names : string list;
    defs : definition list
  }

  (** [elim_dead_defs global_names defs root_names] will return the
      elements of [defs] that are referenced (transitively) by the
      definitions of the names in [root_names]; all names referenced
      in defs must be defined somewhere in defs, except that names in
      [global_names] are treated as external, thus don't need to be
      defined. *)
  let elim_dead_defs globals defs root_names =
    let remove_globals s = difference (StringSet.elements s) globals in
    let defs, other = partition (function Module _ | Define _ -> true
                                   | _ -> false) defs in
    (* Debug.print("definitions in elim_dead_defs (" ^ string_of_int(length defs) ^ "):\n  " ^ mapstrcat "," defname defs); *)
    let groupings = refine_def_groups [defs] in
    let scc_info = map (fun defs ->
                             {free_names = remove_globals (StringSet.union_all (List.map freevars_def defs));
                              exposed_names = Syntax.defined_names defs;
                              defs = defs})
                        groupings in
    let expr_info = { free_names = remove_globals root_names;
                      exposed_names = [];
                      defs = [] } in
    let rec close env =
        match env.free_names with
          | [] -> env
          | names ->
              let defines name {exposed_names = names} = List.mem name names in
              let defining_sccs =  
 		List.map (fun name -> 
                            try
                              List.find (defines name) scc_info
                            with NotFound _ ->
                              failwith("Internal error: missing definition in elim_dead_defs: " ^ name)) 
                  names 
              in
                close (List.fold_right
                         (fun scc env ->
                            let exposed_names = env.exposed_names @ scc.exposed_names in
                              { free_names = 
                                  difference (env.free_names @ scc.free_names)
                                    exposed_names;
                                exposed_names = exposed_names;
                                defs = scc.defs @ env.defs})
                         defining_sccs env)
    in
    try
      other @ (close expr_info).defs
    with NotFound s -> failwith("NotFound "^s^" in elim_dead_defs")

end

let elim_dead_defs = DeadCode.elim_dead_defs
