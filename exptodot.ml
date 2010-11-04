open Utility

type id = int
type label = string
type kind = 
    Record 
  | For 
  | If 
  | Case 
  | Variant
  | List 
  | Apply 
  | Lambda 
  | Primitive 
  | Constant
  | Var
  | Table
  | Box
  | Wrong

type leaf = id * label * kind
type node = id * label * kind * tree list
and tree = Node of node | Leaf of leaf

let string_of_typ = function
  | `Atom -> "::Atom"
  | `List -> "::List"

let ns s t = s ^ (string_of_typ t)

let id = ref 0
let next_id () =
  id := !id + 1;
  !id

let rec tree_of_exp = function
  | `For ((l, os, body), typ) ->
      let label = ns "For" typ in
      let subtrees = (tree_of_exp l) :: ((List.map tree_of_exp os) @ [tree_of_exp body]) in
	Node (next_id (), label, For, subtrees)
  | `Lambda ((vars, body), typ) ->
      let label = (ns "Lambda" typ) ^ "\\n" ^ (mapstrcat " " string_of_int vars) in
	Node (next_id (), label, Lambda, [tree_of_exp body])
  | `If ((c, t, e), typ) ->
      let label = ns "If" typ in
      let id = next_id () in
      let subtrees = [tree_of_exp c; tree_of_exp t] in
      let subtrees = subtrees @ (opt_app (fun e -> [tree_of_exp e]) [] e) in
	Node (id, label, If, subtrees)
  | `Table ((_db, name, _keys, _row), typ) ->
      let label = (ns "Table" typ) ^ "\\n" ^ name in
      let id = next_id () in
	Leaf (id, label, If)
  | `Singleton (x, typ) ->
      let label = ns "Singleton" typ in
      let id = next_id () in
      let subtree = tree_of_exp x in
	Node (id, label, List, [subtree])
  | `Append (xs, typ) ->
      let label = ns "Append" typ in
      let subtrees = List.map tree_of_exp xs in
	Node (next_id (), label, List, subtrees)
  | `Record (map, typ) ->
      let label = ns "Record" typ in
      let f k v (names, values) = (k :: names, v :: values) in
      let (names, values) = Utility.StringMap.fold f map ([], []) in
      let label = mapstrcat "\\n" identity (label :: names) in
      let subtrees = List.map tree_of_exp values in
	Node (next_id (), label, Record, subtrees)
  | `Project ((record, field), typ) ->
      let label = (ns "Project" typ) ^ "\\n" ^ field in
	Node (next_id (), label, Record, [tree_of_exp record])
  | `Extend ((r, extend_fields), typ) ->
      let label = ns "Extend" typ in
      let f k v (names, values) = (k :: names, v :: values) in
      let (names, values) = Utility.StringMap.fold f extend_fields ([], []) in
      let label = mapstrcat "\\n" identity (label :: names) in
      let subtrees = List.map tree_of_exp ((opt_app (fun r -> [r]) [] r) @ values) in
	Node (next_id (), label, Record, subtrees)
  | `Erase ((r, names), typ) ->
      let label = ns "Erase" typ in
      let label = mapstrcat "\\n" identity (label :: (StringSet.elements names)) in
	Node (next_id (), label, Record, [tree_of_exp r])
  | `Variant ((tag, value), typ) ->
      let label = (ns "Variant" typ) ^ "\\n" ^ "tag " ^ tag in
	Node (next_id (), label, Variant, [tree_of_exp value])
  | `Apply ((f, args), typ) ->
      let label = ns "Apply" typ in
      let subtrees = (tree_of_exp f) :: (List.map tree_of_exp args) in
	Node (next_id (), label, Apply, subtrees)
  | `Primitive op ->
      Leaf (next_id (), ("Primitive\\n" ^ op), Primitive)
  | `Var (x, typ) ->
      let label = (ns "Var" typ) ^ "\\n" ^ (string_of_int x) in
	Leaf (next_id (), label, Var)
  | `Constant (c, typ) ->
      let label = (ns "Constant" typ) ^ "\\n" ^ (Constant.string_of_constant c) in
	Leaf (next_id (), label, Constant)
  | `Box (e, typ) ->
      let label = ns "Box" typ in
	Node (next_id (), label, Box, [tree_of_exp e])
  | `Unbox (e, typ) ->
      let label = ns "Unbox" typ in
	Node (next_id (), label, Box, [tree_of_exp e])
  | `Case ((v, cases, default), typ) ->
      let label = ns "Case" typ in
      let f k v (names, values) = (k :: names, v :: values) in
      let (tags, cases) = Utility.StringMap.fold f cases ([], []) in
      let names = List.map2 (fun tag (x, _body) -> Printf.sprintf "%s -> %d" tag x) tags cases in
      let label = mapstrcat "\\n" identity (label :: names) in
      let label = label ^ (opt_app (fun (x, _) -> "\\ndefault -> " ^ (string_of_int x)) "" default) in
      let subtrees = (tree_of_exp v) :: (List.map (tree_of_exp -<- snd) cases) in
      let subtrees = subtrees @ (opt_app (fun x -> [tree_of_exp (snd x)]) [] default) in
	Node (next_id (), label, Case, subtrees)
  | `Wrong typ ->
      let label = ns "Wrong" typ in
	Leaf (next_id (), label, Wrong)
  | `XML _ -> failwith "Not implemented"

let rec apply_tree visit tree =
  match tree with
    | (Leaf _) as l ->
	visit l
    | (Node (_, _, _, subtrees)) as n ->
	visit n;
	List.iter (apply_tree visit) subtrees

let color_of_kind = function
  | Record -> "\"#00AA00\""
  | For -> "\"#00FF00\""
  | If -> "\"#FF3333\""
  | Case -> "\"#990000\""
  | Variant -> "\"lightblue\""
  | List -> "\"magenta\""
  | Apply -> "\"cyan\""
  | Lambda -> "\"red\""
  | Primitive -> "\"#00DDDD\""
  | Constant -> "\"#C0C0C0\""
  | Var -> "\"#909090\""
  | Table -> "\"#C0C0C0\""
  | Box -> "\"yellow\""
  | Wrong -> "\"red\""

let shape_of_kind = function
  | Var -> ", shape=ellipse"
  | _ -> ""

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
  let color = color_of_kind kind in
  let shape = shape_of_kind kind in
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
	
let output_dot e fname =
  let outc = open_out fname in
  let tree = tree_of_exp e in
    output_header outc;
    apply_tree (output_node outc) tree;
    apply_tree (output_edge outc) tree;
    output_footer outc;
    close_out outc
	
      
  
	
	
	
	
	
					 
      
	
	
      
      
