open Utility

module QrKind =
struct
  type kind = 
      Record 
    | For 
    | If 
    | Case 
    | Inject
    | List 
    | Apply 
    | Lambda 
    | Primitive 
    | Constant
    | Var
    | Table
    | Wrong
    | Let

  let color_of_kind = function
    | Record -> "\"#00AA00\""
    | For -> "\"#00FF00\""
    | If -> "\"#FF3333\""
    | Case -> "\"#990000\""
    | Inject -> "\"lightblue\""
    | List -> "\"magenta\""
    | Apply -> "\"cyan\""
    | Lambda -> "\"red\""
    | Primitive -> "\"#00DDDD\""
    | Constant -> "\"#C0C0C0\""
    | Var -> "\"#909090\""
    | Table -> "\"#C0C0C0\""
    | Wrong -> "\"red\""
    | Let -> "\"blue\""

  let shape_of_kind = function
    | Var -> ", shape=ellipse"
    | _ -> ""

end

module QrDot = Dot.Make(QrKind)

let rec tree_of_qr : Qr.qr -> QrDot.tree = function
  | `Lambda (vars, body) ->
      let label = "Lambda" ^ "\\n" ^ (mapstrcat " " string_of_int vars) in
	QrDot.mk_node label QrKind.Lambda [tree_of_qr body]
  | `If (c, t, e) ->
      let label = "If" in
      let subtrees = [tree_of_qr c; tree_of_qr t] in
      let subtrees = subtrees @ (opt_app (fun e -> [tree_of_qr e]) [] e) in
	QrDot.mk_node label QrKind.If subtrees
  | `Table (_db, name, _keys, _row) ->
      let label = "Table" ^ "\\n" ^ name in
	QrDot.mk_leaf label QrKind.Table
  | `Singleton x ->
      let label = "Singleton" in
	QrDot.mk_node label QrKind.List [tree_of_qr x]
  | `Concat xs ->
      let label = "Concat" in
      let subtrees = List.map tree_of_qr xs in
	QrDot.mk_node label QrKind.List subtrees
(* Dead code according to Qr.qr type
  | `Record map ->
      let label = "Record" in
      let f k v (names, values) = (k :: names, v :: values) in
      let (names, values) = Utility.StringMap.fold f map ([], []) in
      let label = mapstrcat "\\n" identity (label :: names) in
      let subtrees = List.map tree_of_qr values in
	QrDot.mk_node label QrKind.Record subtrees
*)
  | `Project (field, record) ->
      let label = "Project" ^ "\\n" ^ field in
	QrDot.mk_node label QrKind.Record [tree_of_qr record]
  | `Extend (extend_fields, r) ->
      let label = "Extend" in
      let f k v (names, values) = (k :: names, v :: values) in
      let (names, values) = Utility.StringMap.fold f extend_fields ([], []) in
      let label = mapstrcat "\\n" identity (label :: names) in
      let subtrees = List.map tree_of_qr ((opt_app (fun r -> [r]) [] r) @ values) in
	QrDot.mk_node label QrKind.Record subtrees
  | `Erase (names, r) ->
      let label = "Erase" in
      let label = mapstrcat "\\n" identity (label :: (StringSet.elements names)) in
	QrDot.mk_node label QrKind.Record [tree_of_qr r]
  | `Inject (tag, value) ->
      let label = "Inject" ^ "\\n" ^ "tag " ^ tag in
	QrDot.mk_node label QrKind.Inject [tree_of_qr value]
  | `Apply (f, args) ->
      let label = "Apply" in
      let subtrees = (tree_of_qr f) :: (List.map tree_of_qr args) in
	QrDot.mk_node label QrKind.Apply subtrees
  | `Primitive op ->
      let label = ("Primitive\\n" ^ op) in
	QrDot.mk_leaf label QrKind.Primitive
  | `Variable x ->
      let label = "Var" ^ "\\n" ^ (string_of_int x) in
	QrDot.mk_leaf label QrKind.Var
  | `Constant c ->
      let label = "Constant" ^ "\\n" ^ (Constant.string_of_constant c) in
	QrDot.mk_leaf label QrKind.Constant
  | `Case (v, cases, default) ->
      let label = "Case" in
      let f k v (names, values) = (k :: names, v :: values) in
      let (tags, cases) = Utility.StringMap.fold f cases ([], []) in
      let names = List.map2 (fun tag (x, _body) -> Printf.sprintf "%s -> %d" tag x) tags cases in
      let label = mapstrcat "\\n" identity (label :: names) in
      let label = label ^ (opt_app (fun (x, _) -> "\\ndefault -> " ^ (string_of_int x)) "" default) in
      let subtrees = (tree_of_qr v) :: (List.map (tree_of_qr -<- snd) cases) in
      let subtrees = subtrees @ (opt_app (fun x -> [tree_of_qr (snd x)]) [] default) in
	QrDot.mk_node label QrKind.Case subtrees
  | `Wrong ->
      let label = "Wrong" in
	QrDot.mk_leaf label QrKind.Wrong
  | `Let (bs, tc) ->
      let label = "Let" in
      let label = label ^ "\\n[" ^ (mapstrcat ", " (fst ->- string_of_int) bs) ^ "]" in
      let trees = (List.map (snd ->- tree_of_qr) bs) @ [tree_of_qr tc] in
	QrDot.mk_node label QrKind.Let trees

let output_dot exp fname = QrDot.output_dot (tree_of_qr exp) fname
