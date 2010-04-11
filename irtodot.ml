open Utility

type id = int
type label = string
type kind = Binding | Value | TailComp | Special | Comp | Variable
type leaf = id * label * kind
type node = id * label * kind * tree list
and tree = Node of node | Leaf of leaf

(* Var.var Env.String.t -> string Env.Int.t*)
(* maps vars to names for prelude values *)
let prelude_venv = ref Env.Int.empty

let lookup_var var env =
  match Value.lookup var env with
    | Some v -> Some v
    | None -> Some (Lib.primitive_stub (Lib.primitive_name var))

let string_of_binder binder =
  let (var, (typ, name, _)) = binder in
  let typestring = Types.string_of_datatype typ in
    Printf.sprintf "%s : %s = %d" name typestring var 

let concat_stringlist strings =
  let rec loop l s =
    match l with
      | x :: b2 :: tl ->
	  let s = s ^ x ^ "\\n" in
	    loop (b2 :: tl) s
      | x :: [] ->
	  s ^ x
      | [] -> 
	  s
  in
    loop strings ""

(* convert RecFunction def list to a string *)
let def_label_of_defs defs =
  mapstrcat 
    "\\n"
    (fun (var, (vars, _)) ->
       (string_of_int var) ^ ": " ^ (mapstrcat " " string_of_int vars))
    defs

let rec nodes_of_bindings id bindings env recnodes =
  let f binding (id, nodes) =
    let (next_id, node) = tree_of_binding id binding env recnodes in
      (next_id, node :: nodes)
  in
    List.fold_right f bindings (id, [])

and nodes_of_computations id bindings env recnodes =
  let f binding (id, nodes) =
    let (next_id, node) = tree_of_computation id binding env recnodes in
      (next_id, node :: nodes)
  in
    List.fold_right f bindings (id, [])

and trees_of_list treefun id l env recnodes =
  let f e (id, trees) =
    let (next_id, tree) = treefun id e env recnodes in
      (next_id, tree :: trees)
  in
    List.fold_right f l (id, [])

and tree_of_binding id binding env recnodes =
  match binding with
    | `Let (binder, (_, tail_comp)) ->
	let label = "let\\n" ^ (string_of_binder binder) in
	let (next_id, subtree) = tree_of_tailcomp (id + 1) tail_comp env recnodes in
	  (next_id, Node (id, label, Binding, [subtree]))
    | `Fun (binder, (_, args, body), _) ->
	let label = "fun\\n" ^ (string_of_binder binder) in
	let bstring_list = List.map string_of_binder args in
	let label = label ^ "\\nargs" in
	let label = label ^ "\\n" ^ (concat_stringlist bstring_list) in
	let (next_id, subtree) = tree_of_computation (id + 1) body env recnodes in
	  (next_id, Node (id, label, Binding, [subtree]))
    | `Rec _ ->
	(* FIXME: handle subtree *)
	let label = "rec" in
	  ((id + 1), Leaf (id, label, Binding))
    | `Alien (binder, language) ->
	let binderstring = string_of_binder binder in
	let label = Printf.sprintf "alien\\n%s\\nlang=%s" binderstring language in
	  ((id + 1), Leaf (id, label, Binding))
    | `Module (name, bindings_option) ->
	let label = "module\\n" ^ name in
	  (match bindings_option with
	     | Some bindings ->
		 let (next_id, subtrees) = nodes_of_bindings (id + 1) bindings env recnodes in
		   (next_id, Node (id, label, Binding, subtrees))
	     | None -> 
		 (id + 1, Leaf (id, label, Binding))
	  )

and tree_of_value id value env recnodes =
  match value with
    | `Constant c ->
	let label = "constant " ^ (Constant.string_of_constant c) in
	  ((id + 1), Leaf (id, label, Value))
    | `Variable var ->
	(try 
	  let value = val_of (lookup_var var env) in 
	    (match value with
	       | `RecFunction (defs, _locals, f, _loc) when (not (IntSet.mem var recnodes)) ->
		   (* try to look up predefined functions in the lib *)
		   let name = 
		     (try
			Lib.primitive_name f
		      with _ -> 
			(try 
			   Env.Int.lookup !prelude_venv f
			 with _ -> ""))
		   in
		   let def_label = def_label_of_defs defs in
		   let comps = List.map (fun (_, (_, c)) -> c) defs in
		   let (next_id, subtrees) = nodes_of_computations (id + 1) comps env (IntSet.add var recnodes) in
		   let funsig = Value.string_of_value value in
		   let value_string = name ^ " " ^ funsig in
		   let label = "var " ^ (string_of_int var) in
		   let label = label ^ "\\n" ^ value_string in
		   let label = label ^ "\\n" ^ def_label in
		     if Env.Int.has !prelude_venv f then
		       ((id + 1), Leaf (id, label, Variable))
		     else
		       (next_id, Node (id, label, Variable, subtrees))
	       | _ ->
		   let value_string = Value.string_of_value value in
		   let label = "var " ^ (string_of_int var) in
		   let label = label ^ "\\n" ^ value_string in
		     ((id + 1), Leaf (id, label, Variable)))
	with NotFound _ -> 
	  let label = "var " ^ (string_of_int var) ^ "\\nnotfound" in
	    ((id + 1), Leaf (id, label, Variable)))
    | `Extend (fields, r) ->
	(* fields: value name_map = map string -> value *)
	let f k v (names, values) = (k :: names, v :: values) in
	let (names, values) = Utility.StringMap.fold f fields ([], []) in
	let label = "extend\\n" in
	let label = label ^ concat_stringlist names in
	let (next_id, trees) = trees_of_list tree_of_value (id + 1) values env recnodes in
	  (match r with
	    | Some value ->
		let (next_id, r_tree) = tree_of_value next_id value env recnodes in
		  (next_id, Node (id, label, Value, trees @ [r_tree]))
	    | None ->
		(next_id, Node (id, label, Value, trees)))
    | `Project (name, value) ->
	let label = "project " ^ name in
	let (next_id, subtree) = tree_of_value (id + 1) value env recnodes in
	  (next_id, Node (id, label, Value, [subtree]))
    | `Erase (nameset, value) ->
	let names = Utility.StringSet.fold
	  (fun name names -> names ^ " " ^ name) 
	  nameset
	  ""
	in
	let label = "erase\\n" ^ names in
	let (next_id, subtree) = tree_of_value (id + 1) value env recnodes in
	  (next_id, Node (id, label, Value, [subtree]))
    | `Inject (name, value, datatype) ->
	let typestring = Types.string_of_datatype datatype in
	let label = Printf.sprintf "inject\\n%s\\n%s" name typestring in
	let (next_id, subtree) = tree_of_value (id + 1) value env recnodes in
	  (next_id, Node (id, label, Value, [subtree]))
    | `TAbs (_, value) ->
	(* TODO: add tyvars to label *)
	let label = "tabs" in
	let (next_id, subtree) = tree_of_value (id + 1) value env recnodes in
	  (next_id, Node (id, label, Value, [subtree]))
    | `TApp (value, _) ->
	tree_of_value id value env recnodes
    | `XmlNode _ ->
	(* TODO: handle subtree and label *)
	let label = "xmlnode\\(subtree)" in
	  ((id + 1), Leaf (id, label, Value))
    | `ApplyPure (f, args) ->
	let label = "applypure" in
	let (next_id, f_tree) = tree_of_value (id + 1) f env recnodes in
	let (next_id, args_trees) = trees_of_list tree_of_value next_id args env recnodes in
	  (next_id, Node (id, label, Value, f_tree :: args_trees))
    | `Coerce (value, typ) ->
	let typestring = Types.string_of_datatype typ in
	let label =  "coerce\\n" ^ typestring in
	let (next_id, subtree) = tree_of_value (id + 1) value env recnodes in
	  (next_id, Node (id, label, Value, [subtree]))

and tree_of_tailcomp id tailcomp env recnodes =
  match tailcomp with
    | `Return value ->
	let label = "return" in
	let (next_id, subtree) = tree_of_value (id + 1) value env recnodes in
	  (next_id, Node (id, label, TailComp, [subtree]))
    | `Apply (f, args) ->
	let label = "apply" in
	let (next_id, f_tree) = tree_of_value (id + 1) f env recnodes in
	let (next_id, args_trees) = trees_of_list tree_of_value next_id args env recnodes in
	  (next_id, Node (id, label, TailComp, f_tree :: args_trees))
    | `Special t ->
	let label = "special" in
	let (next_id, subtree) = tree_of_special (id + 1) t env recnodes in
	  (next_id, Node (id, label, TailComp, [subtree]))
    | `Case (v, cases, _default) -> 
	let label = "case\\n" in
	let (next_id, v_subtree) = tree_of_value (id + 1) v env recnodes in
	  let case_labels =
	    StringMap.fold
	      (fun name (binder, _comp) label ->
		 label ^ name ^ " -> " ^ (string_of_binder binder) ^ "\\n")
	      cases
	      ""
	  in
	  let comps = StringMap.to_list (fun _ (_, comp) -> comp) cases in
	  let (next_id, case_subtrees) = nodes_of_computations next_id comps env recnodes in
	    (next_id, Node (id, (label ^ case_labels), TailComp, v_subtree :: case_subtrees))
    | `If (c, t, e) ->
	let label = "if" in
	let (next_id, c_tree) = tree_of_value (id + 1) c env recnodes in
	let (next_id, t_tree) = tree_of_computation next_id t env recnodes in
	let (next_id, e_tree) = tree_of_computation next_id e env recnodes in
	  (next_id, Node (id, label, TailComp, [c_tree; t_tree; e_tree]))

and tree_of_special id special env recnodes =
  match special with
    | `Wrong typ -> 
	let typestring = Types.string_of_datatype typ in
	let label = "wrong\\n" ^ typestring in
	  ((id + 1), Leaf (id, label, Special))
    | `Database value ->
	let label = "database" in
	let (next_id, subtree) = tree_of_value (id + 1) value env recnodes in
	  (next_id, Node (id, label, Special, [subtree]))
    | `Table (db, name, _) ->
	(* TODO: add types to label *)
	let label = "table" in
	let (next_id, db_tree) = tree_of_value (id + 1) db env recnodes in
	let (next_id, name_tree) = tree_of_value next_id name env recnodes in
	  (next_id, Node (id, label, Special, [db_tree; name_tree]))
    | `Query (range, e, _t) ->
	let (next_id, range_trees) =
	  match range with
	    | None -> ((id + 1), [])
	    | Some (v1, v2) ->
		let (next_id, v1_tree) = tree_of_value (id + 1) v1 env recnodes in
		let (next_id, v2_tree) = tree_of_value next_id v2 env recnodes in
		  (next_id, [v1_tree; v2_tree])
	in
	let (next_id, e_tree) = tree_of_computation next_id e env recnodes in
	let typestring = Types.string_of_datatype _t in
	let label = "query\\n" ^ typestring in
	  (next_id, Node (id, label, Special, range_trees @ [e_tree]))
    | `Update ((xb, source), where, body) ->
	let label = "update" in
	let label = label ^ "\\n" ^ string_of_binder xb in
	let (next_id, source_tree) = tree_of_value (id + 1) source env recnodes in
	  (match where with
	    | None ->
		let (next_id, body_tree) = tree_of_computation next_id body env recnodes in
		  (next_id, Node (id, label, Special, [source_tree; body_tree]))
	    | Some where_comp ->
		let (next_id, where_tree) = tree_of_computation next_id where_comp env recnodes in
		let (next_id, body_tree) = tree_of_computation next_id body env recnodes in
		  (next_id, Node (id, label, Special, [source_tree; where_tree; body_tree])))
    | `Delete ((xb, source), where) ->
	let label = "delete" in
	let label = label ^ "\\n" ^ string_of_binder xb in
	let (next_id, source_tree) = tree_of_value (id + 1) source env recnodes in
	  (match where with
	    | None ->
		(next_id, Node (id, label, Special, [source_tree]))
	    | Some where_comp ->
		let (next_id, where_tree) = tree_of_computation next_id where_comp env recnodes in
		  (next_id, Node (id, label, Special, [source_tree; where_tree])))
    | `CallCC (value) ->
	let label = "callcc" in
	let (next_id, subtree) = tree_of_value (id + 1) value env recnodes in
	  (next_id, Node (id, label, Special, [subtree]))

and tree_of_computation id (bindings, tailcomp) env recnodes =
  let (next_id, binding_trees) = nodes_of_bindings (id + 1) bindings env recnodes in
  let (next_id, tailcomp_tree) = tree_of_tailcomp next_id tailcomp env recnodes in
  let trees = binding_trees @ [tailcomp_tree] in
    (next_id, Node (id, "computation", Comp, trees))

let rec apply_tree visit tree =
  match tree with
    | (Leaf _) as l ->
	visit l
    | (Node (_, _, _, subtrees)) as n ->
	visit n;
	List.iter (apply_tree visit) subtrees

let color_of_kind = function
  | Binding -> "red"
  | Value -> "green"
  | TailComp -> "lightblue"
  | Comp -> "yellow"
  | Special -> "magenta"
  | Variable -> "cyan"

let shape_of_kind = function
  | Variable -> ", shape=ellipse"
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


let output_dot program env fname =
(*
  let (envmap, _) = env in
  let _ = 
    print_endline "\nenv\n";
    IntMap.fold
      (fun var (value, _) () ->
	 Printf.printf "env %d -> %s\n" var (Value.string_of_value value);
	 flush stdout)
      envmap
      ()
  in
*)
  let () = prelude_venv := 
	Env.String.fold
	  (fun name var venv ->
	     Env.Int.bind venv (var, name))
	  (Utility.val_of !Lib.prelude_nenv)
	  !prelude_venv
  in
  let outc = open_out fname in
  let (_, tree) = tree_of_computation 0 program env IntSet.empty in
    output_header outc;
    apply_tree (output_node outc) tree;
    apply_tree (output_edge outc) tree;
    output_footer outc;
    close_out outc
