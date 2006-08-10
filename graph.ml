(* A few graph algorithms.  Pure interfaces, but impure
   implementations.  A graph is represented as node lists + adjacency
   list.  Some abstraction might be nice.
*)

open Utility



(* Utility bits for hashtables and mutable lists *)
let push elem list = 
  list := elem :: !list;
  list

let pushnew elem list = 
  if not (List.mem elem !list) then
    push elem list
  else list

let find_def table elem def = 
  try Hashtbl.find table elem
  with Not_found -> def

let set table k v = 
  Hashtbl.replace table k v;
  v

let hashtbl_as_list tbl = 
  let list = ref [] in
    Hashtbl.iter (fun k v -> list := (k,v) :: !list) tbl;
    !list 

(** unroll_edges: given an alist that maps an x to a list nbhd(x) of
    the same type, return a list of all the pairs (u, v) where 
    v \in nbhd(u) *)
let unroll_edges l = concat_map (fun (f, callers) -> 
                                 List.map (fun caller -> (f, caller)) 
                                   callers) l

(* CLR 23.3 *)
(** Depth-first search *)
let dfs nodes edges = 
  let nnodes = List.length nodes in
  let color = Hashtbl.create nnodes 
  and pi = Hashtbl.create nnodes 
  and d = Hashtbl.create nnodes 
  and f = Hashtbl.create nnodes in
    List.iter (fun u ->
		 Hashtbl.add color u `white;
		 Hashtbl.add pi u None;
              ) nodes;
    let time = ref 0 in
    let rec dfs_visit u = 
      Hashtbl.replace color u `grey;
      Hashtbl.replace d u (incr time; !time);
      List.iter (fun (_, v) -> if Hashtbl.find color v = `white 
                 then (Hashtbl.replace pi v (Some u);
                       dfs_visit v))
        (List.filter ((=) u -<- fst) edges);
      Hashtbl.replace color u `black ;
      Hashtbl.replace f u (incr time; !time)
    in
      List.iter (fun u ->
		   if Hashtbl.find color u = `white
		   then dfs_visit u) nodes;
      (f, d, pi)


(* CLR 23.4 *)
let topological_sort nodes edges = 
  let f, _, _ = dfs nodes edges in
    List.sort (fun (_,y1) (_,y2) -> - (compare y1 y2)) (hashtbl_as_list f)

(* CLR Ex 23.1-3 *)
let transpose_edges : ('a * 'b) list -> ('b * 'a) list = 
  fun list -> List.map (fun (x,y) -> (y,x)) list

let iter_over list f = List.iter f list

(* flatten_forest
   Takes a tree given in "parent-pointer" form, and returns a list
   of the nodes in each tree. *)
let flatten_forest nodes = (* Probably not the best way *)
  let table = Hashtbl.create (List.length nodes) in
    (* For each of the nodes... *)
    iter_over nodes
      (function
           (* if it's a root, just stick it in the table *)
         | node, None -> (if not (Hashtbl.mem table node)
                          then Hashtbl.add table node (ref [node]))
             (* if it has a parent, set the content for both
                to be a list of all things that both are connected to. *)
         | node, Some partner -> 
             let partner_comp = find_def table partner (ref [partner]) in
             let node_comp = find_def table node (ref [node]) in
             let comp = unduplicate (=) (!partner_comp @ !node_comp) in
               partner_comp := comp;
               node_comp := comp;
               ignore(set table partner partner_comp);
               ignore(set table node partner_comp))
    ;
    unduplicate (=) (List.map (snd ->- (!)) (hashtbl_as_list table))
  
let cmp_snd_desc (_,y1) (_,y2) = (- compare y1 y2)

(* CLR 23.5 *)
let strongly_connected_components (nodes : 'a list) (edges : ('a * 'a) list) = 
  let f, _, _ = dfs nodes edges in 
  let edges_reversed = transpose_edges edges in
  let nodes_sorted = (List.map fst (List.sort cmp_snd_desc (hashtbl_as_list f))) in
  let _, _, p = (dfs nodes_sorted edges_reversed) in
    flatten_forest (hashtbl_as_list p)
