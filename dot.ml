open Utility

module type KindSig =
sig
  type kind
  val color_of_kind : kind -> string
  val shape_of_kind : kind -> string
end

module Make(Kind : KindSig) = struct
  type id = int
  type label = string

  type leaf = id * label * Kind.kind
  type node = id * label * Kind.kind * tree list
  and tree = Node of node | Leaf of leaf

  let id = ref 0
  let next_id () =
    id := !id + 1;
    !id

  let mk_node label kind subtrees = Node (next_id (), label, kind, subtrees)
  let mk_leaf label kind = Leaf (next_id (), label, kind)

  let rec apply_tree visit tree =
    match tree with
      | (Leaf _) as l ->
	  visit l
      | (Node (_, _, _, subtrees)) as n ->
	  visit n;
	  List.iter (apply_tree visit) subtrees

  let output_header outc =
    output_string outc "digraph {\n";
    output_string outc "node [shape=box, style=filled, height=0.1, width=0.2, fontsize=10];\n";
    output_string outc "edge [fontsize=9];\n";
    output_string outc "bgcolor=\"#FFFFFF00\";\n";
    output_string outc "color=\"#FFFFFF00\";\n"

  let output_footer outc =
    output_string outc "}\n"

  let output_node outc node =
    let (id, label, kind) =
      match node with 
	| Leaf (id, label, kind) -> (id, label, kind)
	| Node (id, label, kind, _) -> (id, label, kind)
    in
    let color = Kind.color_of_kind kind in
    let shape = Kind.shape_of_kind kind in
    let s = Printf.sprintf "%d [label=\"%s\", color=%s %s];\n" id label color shape in
      output_string outc s

  let output_edge outc node =
    match node with
      | Leaf _ -> ()
      | Node (id, _, _, children) ->
	  let write_edge id1 child_node =
	    let child_id = 
	      match child_node with
		| Leaf (id, _, _) -> id
		| Node (id, _, _, _) -> id
	    in
	      output_string outc (Printf.sprintf "%d -> %d;\n" id1 child_id)
	  in
	    List.iter (write_edge id) children
	      
  let output_dot tree fname =
    let outc = open_out fname in
      output_header outc;
      apply_tree (output_node outc) tree;
      apply_tree (output_edge outc) tree;
      output_footer outc;
      close_out outc
end
