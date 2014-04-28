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
    | Box
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
    | Box -> "\"yellow\""
    | Wrong -> "\"red\""
    | Let -> "\"blue\""

  let shape_of_kind = function
    | Var -> ", shape=ellipse"
    | _ -> ""

end

module QrDot = Dot.Make(QrKind)

let string_of_typ = function
  | `Atom -> "::Atom"
  | `List -> "::List"

let ns s t = s ^ (string_of_typ t)

let rec tree_of_qr : Qr.ImpType.tqr -> QrDot.tree = function
  | `Lambda ((vars, body), typ) ->
      let label = (ns "Lambda" typ) ^ "\\n" ^ (mapstrcat " " string_of_int vars) in
	QrDot.mk_node label QrKind.Lambda [tree_of_qr body]
  | `If ((c, t, e), typ) ->
      let label = ns "If" typ in
      let subtrees = [tree_of_qr c; tree_of_qr t] in
      let subtrees = subtrees @ (opt_app (fun e -> [tree_of_qr e]) [] e) in
	QrDot.mk_node label QrKind.If subtrees
  | `Table ((_db, name, _keys, _row), typ) ->
      let label = (ns "Table" typ) ^ "\\n" ^ name in
	QrDot.mk_leaf label QrKind.Table
  | `Singleton (x, typ) ->
      let label = ns "Singleton" typ in
	QrDot.mk_node label QrKind.List [tree_of_qr x]
  | `Concat (xs, typ) ->
      let label = ns "Concat" typ in
      let subtrees = List.map tree_of_qr xs in
	QrDot.mk_node label QrKind.List subtrees
(*  | `Record (map, typ) ->
      let label = ns "Record" typ in
      let f k v (names, values) = (k :: names, v :: values) in
      let (names, values) = Utility.StringMap.fold f map ([], []) in
      let label = mapstrcat "\\n" identity (label :: names) in
      let subtrees = List.map tree_of_qr values in
	QrDot.mk_node label QrKind.Record subtrees
*)
  | `Project ((field, record), typ) ->
      let label = (ns "Project" typ) ^ "\\n" ^ field in
	QrDot.mk_node label QrKind.Record [tree_of_qr record]
  | `Extend (extend_fields, r, typ) ->
      let label = ns "Extend" typ in
      let f k v (names, values) = (k :: names, v :: values) in
      let (names, values) = Utility.StringMap.fold f extend_fields ([], []) in
      let label = mapstrcat "\\n" identity (label :: names) in
      let subtrees = List.map tree_of_qr ((opt_app (fun r -> [r]) [] r) @ values) in
	QrDot.mk_node label QrKind.Record subtrees
  | `Erase ((names, r), typ) ->
      let label = ns "Erase" typ in
      let label = mapstrcat "\\n" identity (label :: (StringSet.elements names)) in
	QrDot.mk_node label QrKind.Record [tree_of_qr r]
  | `Inject ((tag, value), typ) ->
      let label = (ns "Inject" typ) ^ "\\n" ^ "tag " ^ tag in
	QrDot.mk_node label QrKind.Inject [tree_of_qr value]
  | `Apply ((f, args), typ) ->
      let label = ns "Apply" typ in
      let subtrees = (tree_of_qr f) :: (List.map tree_of_qr args) in
	QrDot.mk_node label QrKind.Apply subtrees
  | `Primitive op ->
      let label = ("Primitive\\n" ^ op) in
	QrDot.mk_leaf label QrKind.Primitive
  | `Variable (x, typ) ->
      let label = (ns "Var" typ) ^ "\\n" ^ (string_of_int x) in
	QrDot.mk_leaf label QrKind.Var
  | `Constant (c, typ) ->
      let label = (ns "Constant" typ) ^ "\\n" ^ (Constant.string_of_constant c) in
	QrDot.mk_leaf label QrKind.Constant
  | `Box (e, typ) ->
      let label = ns "Box" typ in
	QrDot.mk_node label QrKind.Box [tree_of_qr e]
  | `Unbox (e, typ) ->
      let label = ns "Unbox" typ in
	QrDot.mk_node label QrKind.Box [tree_of_qr e]
  | `Case ((v, cases, default), typ) ->
      let label = ns "Case" typ in
      let f k v (names, values) = (k :: names, v :: values) in
      let (tags, cases) = Utility.StringMap.fold f cases ([], []) in
      let names = List.map2 (fun tag (x, _body) -> Printf.sprintf "%s -> %d" tag x) tags cases in
      let label = mapstrcat "\\n" identity (label :: names) in
      let label = label ^ (opt_app (fun (x, _) -> "\\ndefault -> " ^ (string_of_int x)) "" default) in
      let subtrees = (tree_of_qr v) :: (List.map (tree_of_qr -<- snd) cases) in
      let subtrees = subtrees @ (opt_app (fun x -> [tree_of_qr (snd x)]) [] default) in
	QrDot.mk_node label QrKind.Case subtrees
  | `Wrong typ ->
      let label = ns "Wrong" typ in
	QrDot.mk_leaf label QrKind.Wrong
  | `Let (bs, tc, t) ->
      let label = ns "Let" t in
      let label = label ^ "\\n[" ^ (mapstrcat ", " (fst ->- string_of_int) bs) ^ "]" in
      let trees = (List.map (snd ->- tree_of_qr) bs) @ [tree_of_qr tc] in
	QrDot.mk_node label QrKind.Let trees

let output_dot exp fname = QrDot.output_dot (tree_of_qr exp) fname
