open Utility

module ExpKind =
struct
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

end

module ExpDot = Dot.Make(ExpKind)

let string_of_typ = function
  | `Atom -> "::Atom"
  | `List -> "::List"

let ns s t = s ^ (string_of_typ t)

let rec tree_of_exp = function
  | `For ((l, os, body), typ) ->
      let label = ns "For" typ in
      let subtrees = (tree_of_exp l) :: ((List.map tree_of_exp os) @ [tree_of_exp body]) in
	ExpDot.mk_node label ExpKind.For subtrees
  | `Lambda ((vars, body), typ) ->
      let label = (ns "Lambda" typ) ^ "\\n" ^ (mapstrcat " " string_of_int vars) in
	ExpDot.mk_node label ExpKind.Lambda [tree_of_exp body]
  | `If ((c, t, e), typ) ->
      let label = ns "If" typ in
      let subtrees = [tree_of_exp c; tree_of_exp t] in
      let subtrees = subtrees @ (opt_app (fun e -> [tree_of_exp e]) [] e) in
	ExpDot.mk_node label ExpKind.If subtrees
  | `Table ((_db, name, _keys, _row), typ) ->
      let label = (ns "Table" typ) ^ "\\n" ^ name in
	ExpDot.mk_leaf label ExpKind.Table
  | `Singleton (x, typ) ->
      let label = ns "Singleton" typ in
	ExpDot.mk_node label ExpKind.List [tree_of_exp x]
  | `Append (xs, typ) ->
      let label = ns "Append" typ in
      let subtrees = List.map tree_of_exp xs in
	ExpDot.mk_node label ExpKind.List subtrees
  | `Record (map, typ) ->
      let label = ns "Record" typ in
      let f k v (names, values) = (k :: names, v :: values) in
      let (names, values) = Utility.StringMap.fold f map ([], []) in
      let label = mapstrcat "\\n" identity (label :: names) in
      let subtrees = List.map tree_of_exp values in
	ExpDot.mk_node label ExpKind.Record subtrees
  | `Project ((record, field), typ) ->
      let label = (ns "Project" typ) ^ "\\n" ^ field in
	ExpDot.mk_node label ExpKind.Record [tree_of_exp record]
  | `Extend ((r, extend_fields), typ) ->
      let label = ns "Extend" typ in
      let f k v (names, values) = (k :: names, v :: values) in
      let (names, values) = Utility.StringMap.fold f extend_fields ([], []) in
      let label = mapstrcat "\\n" identity (label :: names) in
      let subtrees = List.map tree_of_exp ((opt_app (fun r -> [r]) [] r) @ values) in
	ExpDot.mk_node label ExpKind.Record subtrees
  | `Erase ((r, names), typ) ->
      let label = ns "Erase" typ in
      let label = mapstrcat "\\n" identity (label :: (StringSet.elements names)) in
	ExpDot.mk_node label ExpKind.Record [tree_of_exp r]
  | `Variant ((tag, value), typ) ->
      let label = (ns "Variant" typ) ^ "\\n" ^ "tag " ^ tag in
	ExpDot.mk_node label ExpKind.Variant [tree_of_exp value]
  | `Apply ((f, args), typ) ->
      let label = ns "Apply" typ in
      let subtrees = (tree_of_exp f) :: (List.map tree_of_exp args) in
	ExpDot.mk_node label ExpKind.Apply subtrees
  | `Primitive op ->
      let label = ("Primitive\\n" ^ op) in
	ExpDot.mk_leaf label ExpKind.Primitive
  | `Var (x, typ) ->
      let label = (ns "Var" typ) ^ "\\n" ^ (string_of_int x) in
	ExpDot.mk_leaf label ExpKind.Var
  | `Constant (c, typ) ->
      let label = (ns "Constant" typ) ^ "\\n" ^ (Constant.string_of_constant c) in
	ExpDot.mk_leaf label ExpKind.Constant
  | `Box (e, typ) ->
      let label = ns "Box" typ in
	ExpDot.mk_node label ExpKind.Box [tree_of_exp e]
  | `Unbox (e, typ) ->
      let label = ns "Unbox" typ in
	ExpDot.mk_node label ExpKind.Box [tree_of_exp e]
  | `Case ((v, cases, default), typ) ->
      let label = ns "Case" typ in
      let f k v (names, values) = (k :: names, v :: values) in
      let (tags, cases) = Utility.StringMap.fold f cases ([], []) in
      let names = List.map2 (fun tag (x, _body) -> Printf.sprintf "%s -> %d" tag x) tags cases in
      let label = mapstrcat "\\n" identity (label :: names) in
      let label = label ^ (opt_app (fun (x, _) -> "\\ndefault -> " ^ (string_of_int x)) "" default) in
      let subtrees = (tree_of_exp v) :: (List.map (tree_of_exp -<- snd) cases) in
      let subtrees = subtrees @ (opt_app (fun x -> [tree_of_exp (snd x)]) [] default) in
	ExpDot.mk_node label ExpKind.Case subtrees
  | `Wrong typ ->
      let label = ns "Wrong" typ in
	ExpDot.mk_leaf label ExpKind.Wrong
  | `XML _ -> failwith "Not implemented"
      
let output_dot exp fname = ExpDot.output_dot (tree_of_exp exp) fname
