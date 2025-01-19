(** A few graph algorithms.  Pure interfaces, but impure
   implementations.  A graph is represented as node lists + adjacency
   list.
*)

open Utility

(* Utility function for hashtables *)
let hashfind_dflt table elem def =
  try Hashtbl.find table elem
  with NotFound _ -> def

(** Crazy notation for hashtable updates. Suggestions are welcomed.

   [table *->! k v] updates k to v in table
   [table *-> k |-> d] returns the value for [k] in [table], or [d]
     if [k] is not set.
   [table *+> k ++= newVal] cons'es newVal onto the list stored
     under k
*)

let ( *->!) table k v = Hashtbl.replace table k v
(* let ($) op arg = op arg*)

let ( *-> ) = hashfind_dflt
let (|->) op arg = op arg


let ( *+> ) table k v =
  (table *->! k) (v :: (table *-> k |-> []))
let ( ++= ) op arg = op arg

let hashtbl_invert table =
  let result = Hashtbl.create (Hashtbl.length table) in
    Hashtbl.iter (fun k v ->
                    result *+> v ++= k) table;
    result

let hashtbl_values table = Hashtbl.fold (fun _ v rslt -> v :: rslt) table []

(** [hashtbl_regions table] is the list of equivalence classes of keys
    in [table], where equivalence is determined through lookup in [table].
    If you think of the value under each key as its "color", this gives
    you the list of groups having the same color.
*)
let hashtbl_regions table =
  hashtbl_values(hashtbl_invert table)

let hashtbl_as_alist tbl =
  let list = ref [] in
    Hashtbl.iter (fun k v -> list := (k,v) :: !list) tbl;
    !list

(** unroll_edges: given an alist that maps an x to a list nbhd(x) of
    the same type, return a list of all the pairs (u, v) where
    v \in nbhd(u) *)
let unroll_edges l = concat_map (fun (f, callers) ->
                                 List.map (fun caller -> (f, caller))
                                   callers) l

let edge_to_str (u, v) = u ^ "->" ^ v

let reverse = List.rev

(* CLR 23.3 *)
(** Depth-first search *)
let dfs nodes edges =
  let nnodes = List.length nodes in
  let color = Hashtbl.create nnodes in
  let parent = Hashtbl.create nnodes in
  let discover = Hashtbl.create nnodes in
  let finish = Hashtbl.create nnodes in
  List.iter (fun u ->
               Hashtbl.add color u `white;
               Hashtbl.add parent u None;
            ) nodes;
  let time = ref 0 in
  let rec dfs_visit u =
    Hashtbl.replace color u `grey;
    Hashtbl.replace discover u (incr time; !time);
    List.iter (fun (_, v) -> if Hashtbl.find color v = `white
               then (Hashtbl.replace parent v (Some u);
                     dfs_visit v))
      (List.filter ((=) u -<- fst) edges);
    Hashtbl.replace color u `black ;
    Hashtbl.replace finish u (incr time; !time)
  in
    List.iter (fun u ->
         if Hashtbl.find color u = `white
         then dfs_visit u) nodes;
    (finish, discover, parent)

(* CLR 23.4 *)
let topological_sort' nodes edges =
  let f, _, _ = dfs nodes edges in
    List.sort (fun (_,y1) (_,y2) -> - (compare y1 y2)) (hashtbl_as_alist f)

let topological_sort nodes edges =
  List.map fst (topological_sort' nodes edges)


(* CLR Ex 23.1-3 *)
let transpose_edges : ('a * 'b) list -> ('b * 'a) list =
  fun list -> List.map (fun (x,y) -> (y,x)) list

let string_of_parent_tree =
  mapstrcat "\n" (function a, None -> a ^ " root"
                    | a, Some b -> a ^ " -> " ^ b)

(**Takes a tree given in "parent-pointer" form, and returns a list
   of the nodes in each tree. More or less duplicates the union-find
   algorithm? *)
let flatten_forest nodes : 'a list list=
  let table = Hashtbl.create (List.length nodes) in
  let counter = ref 0 in
  let bump_counter () = incr counter; !counter in

    ListLabels.iter nodes
      ~f:(function
           (* if it's a root, just stick it in the table *)
         | node, None ->
             Hashtbl.replace table node
                      (hashfind_dflt table node (bump_counter()));
           (* if it has a parent, set them both to the same color. *)
         | node, Some parent ->
             let parent_comp = hashfind_dflt table parent (bump_counter()) in
             let node_comp = hashfind_dflt table node 0 in
               Hashtbl.replace table node parent_comp;
               Hashtbl.replace table parent parent_comp;
               (* also update anybody that had node's old color. *)
               Hashtbl.iter (fun k v ->
                               if v = node_comp then
                                 Hashtbl.replace table k parent_comp)
                 table
      );
    hashtbl_regions table

let cmp_snd_desc (_,y1) (_,y2) = (- compare y1 y2)

(* CLR 23.5 *)
let strongly_connected_components (nodes : 'a list) (edges : ('a * 'a) list) : 'a list list =
  let f, _, _ = dfs nodes edges in
  let edges_reversed = transpose_edges edges in
  let nodes_sorted = (List.map fst (List.sort cmp_snd_desc (hashtbl_as_alist f))) in
  let _, _, p = (dfs nodes_sorted edges_reversed) in
    flatten_forest (hashtbl_as_alist p)

(** [topo_sort_sccs]: given a graph in adjacency-list
    representation, find all the sccs and topologically sort them;
    return the result as a list of sccs, the sccs represented as the
    list of their members. *)
let topo_sort_sccs (adj_list : ('a * 'a list) list) : 'a list list =
  (* [adj_list] is an alist, mapping each node to the
     list of nodes it points to, like so:
     [(u, [v; w; ...]);
      (v, [u; s; t; ...])]
  *)
  (* [scc_of sccs]: lookup (in [sccs]) the scc that [v] belongs to *)
  let scc_of sccs v = List.find (List.mem v) sccs in

  let nodes = List.map fst adj_list in
    (*  unfold adj_list: let `edges' be the list of all
        (u, v) where (u, v) is an edge in the graph *)
  let edges = unroll_edges adj_list in
  let sccs = strongly_connected_components nodes edges in
    (* Now, for each scc, find the nodes that it points to: *)
  let scc_innodes =
    map2alist (fun nodes -> concat_map_uniq (lookup_in adj_list) nodes)
      sccs in
    (* Map each such node to its scc, so that we have, for each
       scc, the list of sccs that it points to: *)
  let scc_innodes =
    alistmap (fun calls -> List.map (scc_of sccs) calls) scc_innodes in
    (* Now unroll that to get a list of pairs (U, V) where U and V are
       sccs and there is an edge from U to V *)
  let scc_edges = unroll_edges scc_innodes in
  let result = reverse (topological_sort sccs scc_edges) in
    result
