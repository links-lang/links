open CommonTypes

open Utility
open Proc

(* Error functions *)
let runtime_error msg = raise (Errors.runtime_error msg)

let internal_error message =
  Errors.internal_error ~filename:"lib.ml" ~message

let runtime_type_error error =
  internal_error
    ("Runtime type error: " ^ error ^ ".\n" ^
     "This should not happen if the type system / checker is correct. " ^
     "Please file a bug report.")

(* Alias environment *)
module AliasEnv = Env.String

(* This is done in two stages because the datatype for regexes refers
   to the String alias *)
let alias_env : Types.tycon_environment = DefaultAliases.alias_env

let alias_env : Types.tycon_environment =
  AliasEnv.bind "Repeat" (`Alias ([], (DesugarDatatypes.read ~aliases:alias_env Linksregex.Repeat.datatype))) alias_env

let alias_env : Types.tycon_environment =
  AliasEnv.bind "Regex" (`Alias ([], (DesugarDatatypes.read ~aliases:alias_env Linksregex.Regex.datatype))) alias_env

let datatype = DesugarDatatypes.read ~aliases:alias_env

type primitive =
[ Value.t
| `PFun of RequestData.request_data -> Value.t list -> Value.t ]

type pure = PURE | IMPURE

type located_primitive = [ `Client | `Server of primitive | primitive ]

let mk_binop_fn impl unbox_fn constr = function
    | [x; y] -> constr (impl (unbox_fn x) (unbox_fn y))
    | _ -> raise (internal_error "arity error in integer operation")

let int_op impl pure : located_primitive * Types.datatype * pure =
  (`PFun (fun _ -> mk_binop_fn impl Value.unbox_int (fun x -> `Int x))),
  datatype "(Int, Int) -> Int",
  pure

let float_op impl pure : located_primitive * Types.datatype * pure =
  (`PFun (fun _ -> mk_binop_fn impl Value.unbox_float (fun x -> `Float x))),
  datatype "(Float, Float) -> Float",
  pure

let string_op impl pure : located_primitive * Types.datatype * pure =
  (`PFun (fun _ -> mk_binop_fn impl Value.unbox_string (fun x -> `String x))),
  datatype "(String, String) -> String",
  pure

let conversion_op' ~unbox ~conv ~(box :'a->Value.t): Value.t list -> Value.t = function
    | [x] -> box (conv (unbox x))
    | _ -> assert false

let conversion_op ~from ~unbox ~conv ~(box :'a->Value.t) ~into pure : located_primitive * Types.datatype * pure =
  let open Types in
  ((`PFun (fun _ x -> conversion_op' ~unbox:unbox ~conv:conv ~box:box x) : located_primitive),
   (let q, r = fresh_row_quantifier (lin_any, res_any) in
      (ForAll ([q], Function (make_tuple_type [from], r, into)) : datatype)),
   pure)

let string_to_xml : Value.t -> Value.t = function
  | `String s -> `List [`XML (Value.Text s)]
  | _ -> raise (runtime_type_error "non-string value passed to xml conversion routine")

(* The following functions expect 1 argument. Assert false otherwise. *)
let float_fn fn pure =
  (`PFun (fun _ args ->
      match args with
        | [c] -> (Value.box_float (fn (Value.unbox_float c)))
        | _ -> assert false),
   datatype "(Float) -> Float",
  pure)

(* Functions which also take the request data as an argument --
 * for example those which set cookies, change the headers, etc. *)
let p1D fn =
  `PFun (fun req_data args ->
      match args with
        | ([a]) -> fn a req_data
        | _ -> assert false)

let p2D fn =
  `PFun (fun req_data args ->
      match args with
        | [a; b] -> fn a b req_data
        | _ -> assert false)

let p3D fn =
  `PFun (fun req_data args ->
      match args with
        | [a;b;c] -> fn a b c req_data
        | _ -> assert false)

let p1 fn = p1D (fun x _ -> fn x)
let p2 fn = p2D (fun x y _ -> fn x y)
let p3 fn = p3D (fun x y z _ -> fn x y z)

let rec equal l r =
  match l, r with
    | `Bool l  , `Bool r   -> l = r
    | `Int l   , `Int r    -> l = r
    | `Float l , `Float r  -> l = r
    | `Char l  , `Char r   -> l = r
    | `String l, `String r -> l = r
    | `Record lfields, `Record rfields ->
        let rec one_equal_all = (fun alls (ref_label, ref_result) ->
                                   match alls with
                                     | [] -> false
                                     | (label, result) :: _ when label = ref_label -> equal result ref_result
                                     | _ :: alls -> one_equal_all alls (ref_label, ref_result)) in
          List.for_all (one_equal_all rfields) lfields && List.for_all (one_equal_all lfields) rfields
    | `Variant (llabel, lvalue), `Variant (rlabel, rvalue) -> llabel = rlabel && equal lvalue rvalue
    | `List (l), `List (r) -> equal_lists l r
    | l, r ->
        runtime_error
          (Printf.sprintf "Comparing %s with %s which either does not make sense or isn't implemented."
            (Value.string_of_value l) (Value.string_of_value r))
and equal_lists l r =
  match l,r with
    | [], [] -> true
    | (l::ls), (r::rs) -> equal l r && equal_lists ls rs
    | _,_ -> false


let rec less l r =
  match l, r with
    | `Bool l, `Bool r   -> l < r
    | `Int l, `Int r     -> l < r
    | `Float l, `Float r -> l < r
    | `Char l, `Char r -> l < r
    | `String l, `String r -> l < r
      (* Compare fields in lexicographic order of labels *)
    | `Record lf, `Record rf ->
        let order = List.sort (fun x y -> compare (fst x) (fst y)) in
        let lv, rv = List.map snd (order lf), List.map snd (order rf) in
        let rec compare_list = function
          | [] -> false
          | (l,r)::_ when less l r -> true
          | (l,r)::_ when less r l -> false
          | _::rest                -> compare_list rest in
          compare_list (List.combine lv rv)
    | `List (l), `List (r) -> less_lists (l,r)
    | l, r ->  runtime_error ("Cannot yet compare "^ Value.string_of_value l ^" with "^ Value.string_of_value r)
and less_lists = function
  | _, [] -> false
  | [], (_::_) -> true
  | (l::_), (r::_) when less l r -> true
  | (l::_), (r::_) when less r l -> false
  | (_::l), (_::r) -> less_lists (l, r)

let less_or_equal l r = less l r || equal l r

let add_attribute : Value.t * Value.t -> Value.t -> Value.t =
  let rec filter = fun name -> function
    | [] -> []
    | Value.Attr (s, _) :: nodes when s = name -> filter name nodes
    | Value.NsAttr (ns, s, _) :: nodes when ns ^ ":" ^ s = name -> filter name nodes
    | node :: nodes -> node :: filter name nodes
  in
  fun (name,value) ->
    let name = Value.unbox_string name
    and value = Value.unbox_string value
    in let new_attr = match String.split_on_char ':' name with
          | [ n ] -> Value.Attr(n, value)
          | [ ns; n ] -> Value.NsAttr(ns, n, value)
          | _ -> runtime_error ("Attribute-name con only contain one colon for namespacing. Multiple found: " ^ name)
    in function
    | `XML (Value.Node (tag, children))       -> `XML (Value.Node (tag, new_attr :: filter name children))
    | `XML (Value.NsNode (ns, tag, children)) -> `XML (Value.NsNode (ns, tag, new_attr :: filter name children))
    | r -> raise (runtime_type_error ("cannot add attribute to " ^ Value.string_of_value r))

let add_attributes : (Value.t * Value.t) list -> Value.t -> Value.t =
  List.fold_right add_attribute

let env : (string * (located_primitive * Types.datatype * pure)) list = [
  "+", int_op (+) PURE;
  "-", int_op (-) PURE;
  "*", int_op ( * ) PURE;
  "/", int_op (/) IMPURE;
  "^", int_op pow PURE;
  "mod", int_op (mod) IMPURE;
  "+.", float_op (+.) PURE;
  "-.", float_op (-.) PURE;
  "*.", float_op ( *.) PURE;
  "/.", float_op (/.) PURE;
  "^.", float_op ( ** ) PURE;
  "^^", string_op ( ^ ) PURE;

  (* Comparisons *)
  "==",
  (p2 (fun v1 v2 -> Value.box_bool (equal v1 v2)),
   datatype "(a,a) -> Bool",
   PURE);

  "<>",
  (p2 (fun v1 v2 -> Value.box_bool (not (equal v1 v2))),
   datatype "(a,a) -> Bool",
   PURE);

  "<",
  (p2 (fun v1 v2 -> Value.box_bool (less v1 v2)),
   datatype "(a,a) -> Bool",
   PURE);

  ">",
  (p2 (fun v1 v2 -> Value.box_bool (less v2 v1)),
   datatype "(a,a) -> Bool",
   PURE);


  "<=",
  (p2 (fun v1 v2 -> Value.box_bool (less_or_equal v1 v2)),
   datatype "(a,a) -> Bool",
   PURE);

  ">=",
  (p2 (fun v1 v2 -> Value.box_bool (less_or_equal v2 v1)),
   datatype "(a,a) -> Bool",
   PURE);

  (* Conversions (any missing?) *)
  "intToString",   conversion_op ~from:(Types.Primitive Primitive.Int)
                                 ~unbox:Value.unbox_int
                                 ~conv:string_of_int
                                 ~box:Value.box_string
                                 ~into:Types.string_type
                                 PURE;
  "stringToInt",   conversion_op ~from:Types.string_type
                                 ~unbox:Value.unbox_string
                                 ~conv:int_of_string
                                 ~box:Value.box_int
                                 ~into:(Types.Primitive Primitive.Int)
                                 IMPURE;
  "intToFloat",    conversion_op ~from:(Types.Primitive Primitive.Int)
                                 ~unbox:Value.unbox_int
                                 ~conv:float_of_int
                                 ~box:Value.box_float
                                 ~into:(Types.Primitive Primitive.Float)
                                 PURE;
  "floatToInt",    conversion_op ~from:(Types.Primitive Primitive.Float)
                                 ~unbox:Value.unbox_float
                                 ~conv:int_of_float
                                 ~box:Value.box_int
                                 ~into:(Types.Primitive Primitive.Int)
                                 PURE;
  "floatToString", conversion_op ~from:(Types.Primitive Primitive.Float)
                                 ~unbox:Value.unbox_float
                                 ~conv:string_of_float'
                                 ~box:Value.box_string
                                 ~into:Types.string_type
                                 PURE;
  "stringToFloat", conversion_op ~from:Types.string_type
                                 ~unbox:Value.unbox_string
                                 ~conv:float_of_string
                                 ~box:Value.box_float
                                 ~into:(Types.Primitive Primitive.Float)
                                 IMPURE;

  "stringToXml",
  ((p1 string_to_xml),
   datatype "(String) -> Xml",
  PURE);

  "intToXml",
  (`PFun (fun _ ->
    string_to_xml -<- (conversion_op' ~unbox:Value.unbox_int ~conv:(string_of_int) ~box:Value.box_string)),
   datatype "(Int) -> Xml",
  PURE);

  "floatToXml",
  (`PFun (fun _ ->
    string_to_xml -<- (conversion_op' ~unbox:Value.unbox_float ~conv:(string_of_float') ~box:Value.box_string)),
   datatype "(Float) -> Xml",
   PURE);

  "sysexit",
  (p1 (fun ret -> exit (Value.unbox_int ret)),
   datatype "(Int) ~> a",
   IMPURE);

  "show",
  (p1 (fun v -> Value.box_string (Value.string_of_value v)),
   datatype "(a) ~> String",
   IMPURE);

  "exit",
  (`Continuation Value.Continuation.empty,
  (* Return type must be free so that it unifies with things that
     might be used alternatively. E.g.:
     if (test) exit(1) else 42 *)
   datatype "(a) ~> b",
  IMPURE);

  (* Adds a list of attributes (represented as pairs of strings) to
     each of the root nodes of an XML forest. *)
  "addAttributes",
  (p2 (fun xml attrs -> match xml, attrs with
         | `List xmlitems, `List attrs ->
             let attrs = List.map (fun p -> Value.unbox_pair p) attrs in
               `List (List.map (add_attributes attrs) xmlitems)
         | _ -> raise (runtime_type_error "addAttributes takes an XML forest and a list of attributes")),
   datatype "(Xml, [(String, String)]) -> Xml",
   PURE);

  "Send",
  (p2 (fun _pid _msg ->
         assert(false)), (* Now handled in evalir.ml *)
   datatype "forall a::Type(Any, Any), e::Row(Unl, Any), f::Row.(Process ({hear:a|e}), a) ~f~> ()",
   IMPURE);

  "self",
  (`PFun (fun _ _ -> `Pid (`ServerPid (Proc.get_current_pid()))),
   datatype "() {hear{a}|e}~> Process ({ hear{a} })",
   IMPURE);

  "here",
  (`PFun (fun _ _ -> `SpawnLocation (`ServerSpawnLoc)),
    datatype "() ~> Location",
    IMPURE
  );

  "there",
  (`PFun (fun req_data _ ->
    let client_id = RequestData.get_client_id req_data in
    `SpawnLocation (`ClientSpawnLoc client_id)),
    datatype "() ~> Location",
    IMPURE
  );

  "haveMail",
  (`PFun(fun _ ->
           runtime_error "The haveMail function is not implemented on the server yet"),
   datatype "() {:_|_}~> Bool",
   IMPURE);

  "recv",
  (* This function is not used, as its application is a special case
     in the interpreter. But we need it here (for now) to assign it a
     type. Ultimately we should probably not special-case it, but
     rather provide a way to implement this primitive from here.
     (Ultimately, it should perhaps be a true primitive (an AST node),
     because it uses a different evaluation mechanism from functions.
     -- jdy) *)
    (`PFun (fun (_) -> assert false),
     datatype "() {:a|_}~> a",
  IMPURE);

  "spawn",
  (`PFun (fun _ -> assert false),
    begin
    if Settings.get Basicsettings.Sessions.exceptions_enabled then
      datatype "(() { SessionFail:[||] |e}~@ _) ~> Process ({ |e })"
    else
      datatype "(() ~e~@ _) ~> Process ({ |e })"
    end,
  IMPURE);

  "spawnAt",
  (`PFun (fun _ -> assert false),
    begin
    if Settings.get Basicsettings.Sessions.exceptions_enabled then
      datatype "(Location, () {SessionFail:[||] |e}~@ _) ~> Process ({ |e })"
    else
      datatype "(Location, () ~e~@ _) ~> Process ({ |e })"
    end,
   IMPURE);

  "spawnClient",
  (`PFun (fun _ -> assert false),
    begin
    if Settings.get Basicsettings.Sessions.exceptions_enabled then
      datatype "(() { SessionFail:[||] |e}~@ _) ~> Process ({ |e })"
    else
      datatype "(() ~e~@ _) ~> Process ({ |e })"
    end,
   IMPURE);

  "spawnAngel",
  (`PFun (fun _ -> assert false),
    begin
    if Settings.get Basicsettings.Sessions.exceptions_enabled then
      datatype "(() { SessionFail:[||] |e}~@ _) ~> Process ({ |e })"
    else
      datatype "(() ~e~@ _) ~> Process ({ |e })"
    end,
   IMPURE);

  "spawnAngelAt",
  (`PFun (fun _ -> assert false),
    begin
    if Settings.get Basicsettings.Sessions.exceptions_enabled then
      datatype "(Location, () { SessionFail:[||] |e}~@ _) ~> Process ({ |e })"
    else
      datatype "(Location, () ~e~@ _) ~> Process ({ |e })"
    end,
   IMPURE);

  "spawnWait",
  (`PFun (fun _ -> assert false),
   datatype "(() { |e}~> a) ~> a",
   IMPURE);

  "spawnWait'",
  (`PFun (fun _ -> assert false),
   datatype "() ~> a",
   IMPURE);

   (* If we add more effects then spawn and spawnWait shouldn't
     necessarily mask them, so we might want to change their types to
     something like this:

     spawn : (() {wild{p},hear{q}:a|e}-> _) {hear{_}:_|e}~> Process({wild{p},hear{q}:a|e})
     spawnWait : (() {wild{_},hear{_}:_|e}-> a) {hear{_}:_|e}~> a

     We might even split spawnWait into spawn and wait:

     spawn : (() {wild{p},hear{q}:b|e}-> a) {hear{_}:_|e}~> Process(a, {wild{p},hear{q}:b|e})
     wait : Process (a, {wild{_},hear{_}:_|e}) {hear{_}:_}~> a
  *)

  (* Sessions *)

  "send",
  (`PFun (fun _ -> assert false),
   datatype "forall a::Type(Any, Any), s::Type(Any, Session), e::Row.(a, !a.s) ~e~> s",
   IMPURE);

  "receive",
  (`PFun (fun _ -> assert false),
   datatype "forall a::Type(Any, Any), s::Type(Any, Session), e::Row. (?a.s) ~e~> (a, s)",
   IMPURE);

  "link",
  (`PFun (fun _ -> assert false),
   datatype "forall s::Type(Any, Session), e::Row(Unl, Any).(s, ~s) ~e~> ()",
   IMPURE);

  (* access points *)
  "new",
  (`PFun (fun _ -> assert false),
   datatype "forall s::Type(Any, Session), e::Row.() ~e~> AP(s)",
   IMPURE);

  "newAP",
  (`PFun (fun _ -> assert false),
   datatype "forall s::Type(Any, Session), e::Row. (Location) ~e~> AP(s)",
   IMPURE);

  "newClientAP",
  (`PFun (fun _ -> assert false),
   datatype "forall s::Type(Any, Session), e::Row.() ~e~> AP(s)",
   IMPURE);

  "newServerAP",
  (`PFun (fun _ -> assert false),
   datatype "forall s::Type(Any, Session), e::Row.() ~e~> AP(s)",
   IMPURE);

  "accept",
  (`PFun (fun _ -> assert false),
   datatype "forall s::Type(Any, Session), e::Row.(AP(s)) ~e~> s",
   IMPURE);

  "request",
  (`PFun (fun _ -> assert false),
   datatype "forall s::Type(Any, Session), e::Row.(AP(s)) ~e~> ~s",
   IMPURE);

  "cancel",
  (`PFun (fun _ -> assert false),
   datatype "forall s::Type(Any, Session), e::Row.(s) ~e~> ()",
   IMPURE);

  "close",
  (`PFun (fun _ -> assert false),
   datatype "(End) ~> ()",
   IMPURE);

  (* Lists and collections *)
  "Nil",
  (`List [],
   datatype "[a]",
   PURE);

  "Cons",
  (p2 (fun x xs ->
         Value.box_list (x :: (Value.unbox_list xs))),
   datatype "(a, [a]) -> [a]",
   PURE);

  "Concat",
  (p2 (fun xs ys ->
         Value.box_list (Value.unbox_list xs @ Value.unbox_list ys)),
   datatype "([a], [a]) -> [a]",
   PURE);

  "hd",
  (p1 (fun lst ->
        match (Value.unbox_list lst) with
          | [] -> runtime_error "hd() of empty list"
          | x :: _ -> x
      ),
   datatype "([a]) ~> a",
  IMPURE);

  "tl",
  (p1 (fun lst ->
         match (Value.unbox_list lst) with
            | [] -> runtime_error "tl() of empty list"
            | _x :: xs -> Value.box_list xs
      ),
   datatype "([a]) ~> [a]",
  IMPURE);

  (* HACK: tame versions of head and tail for use in pattern matching *)
  "$$hd",
  (p1 (fun lst ->
        match (Value.unbox_list lst) with
          | [] -> runtime_error "hd() of empty list"
          | x :: _ -> x
      ),
   datatype "([a]) -> a",
  IMPURE);

  "$$tl",
  (p1 (fun lst ->
         match (Value.unbox_list lst) with
            | [] -> runtime_error "tl() of empty list"
            | _x :: xs -> Value.box_list xs
      ),
   datatype "([a]) -> [a]",
  IMPURE);

  "length",
  (p1 (Value.unbox_list ->- List.length ->- Value.box_int),
   datatype "([a]) -> Int",
  PURE);

  "take",
  (p2 (fun n l ->
         Value.box_list (Utility.take (Value.unbox_int n) (Value.unbox_list l))),
   datatype "(Int, [a]) ~> [a]",
  PURE);

  "drop",
  (p2 (fun n l ->
         Value.box_list (Utility.drop (Value.unbox_int n) (Value.unbox_list l))),
   datatype "(Int, [a]) ~> [a]",
  PURE);

  "max",
  (p1 (let max2 x y = if less x y then y else x in
         function
           | `List [] -> `Variant ("None", `Record [])
           | `List (x::xs) -> `Variant ("Some", List.fold_left max2 x xs)
           | _ -> raise (runtime_type_error "Internal error: non-list passed to max")),
   datatype "([a]) ~> [|Some:a | None:()|]",
  PURE);

  "min",
  (p1 (let min2 x y = if less x y then x else y in
         function
           | `List [] -> `Variant ("None", `Record [])
           | `List (x::xs) -> `Variant ("Some", List.fold_left min2 x xs)
           | _ -> raise (runtime_type_error "Internal error: non-list passed to min")),
   datatype "([a]) ~> [|Some:a | None:()|]",
  PURE);

  (* XML *)
  "itemChildNodes",
  (p1 (function
         | `XML (Value.Node (_, children)) ->
           let children = List.filter Value.is_node children in
           `List (List.map (fun x -> `XML x) children)
         | `XML (Value.NsNode (_, _, children)) ->
           let children = List.filter Value.is_node children in
           `List (List.map (fun x -> `XML x) children)
         | `XML (_) -> `List []
         | _ -> raise (runtime_type_error "non-XML given to itemChildNodes")),
   datatype "(XmlItem) -> Xml",
  IMPURE);

  "objectType",
  (`Client, datatype "(a) ~> String",
  IMPURE);

  "attribute",
  (p2 (fun elem attr ->
    let attr = Value.unbox_string attr
    in let find_attr = fun cs -> (try match List.find (function
      | Value.Attr (k, _) when k = attr -> true
      | Value.NsAttr (ns, k, _) when ns ^ ":" ^ k = attr -> true
      | _ -> false
    ) cs with
      | Value.Attr (_, v) -> `Variant ("Just", Value.box_string v)
      | Value.NsAttr (_, _, v) -> `Variant ("Just", Value.box_string v)
      | _ -> raise (runtime_type_error "Incorrect arguments to `attribute' function")
    with NotFound _ -> `Variant ("Nothing", `Record []))
    in match elem with
      | `List [ `XML (Value.Node (_, children)) ] -> find_attr children
      | `List [ `XML (Value.NsNode (_, _, children)) ] -> find_attr children
      | _ -> raise (runtime_type_error  "Non-element node given to attribute function")),
  datatype "(Xml, String) -> [| Just: String | Nothing |]",
  PURE);

  "debug",
  (p1 (fun message -> Debug.print (Value.unbox_string message);
                      `Record []),
   datatype "(String) ~> ()",
  IMPURE);

  "debugObj",
  (`Client, datatype "(a) ~> ()",
  IMPURE);

  "dump",
  (`Client, datatype "(a) ~> ()",
  IMPURE);

  "textContent",
  (`Client, datatype "(DomNode) ~> String",
  IMPURE);

  "print",
  (p1 (fun msg -> print_string (Value.unbox_string msg); flush stdout; `Record []),
   datatype "(String) ~> ()",
  IMPURE);

  "javascript",
  (`Bool false, datatype "Bool",
  PURE);

  "not",
  (p1 (Value.unbox_bool ->- not ->- Value.box_bool),
   datatype "(Bool) -> Bool",
  PURE);

  "negate",
  (p1 (Value.unbox_int ->- (~-) ->- Value.box_int), datatype "(Int) -> Int",
  PURE);

  "negatef",
  (p1 (fun f -> Value.box_float (-. (Value.unbox_float f))), datatype "(Float) -> Float",
  PURE);

  "error",
  (p1 (Value.unbox_string ->- runtime_error), datatype "(String) ~> a",
  IMPURE);

  (* HACK *)
  (*   [DEACTIVATED] *)
  (*   "callForeign", *)
  (*    (`Client, datatype "((a) -> b) -> (a) -> b"); *)

  (* DOM API *)

  "isElementNode",
  (`Client, datatype "(DomNode) ~> Bool",
  PURE);


  (* [DEACTIVATED] *)
  (*   "domOp", *)
  (*   (p1 (fun message -> failwith("`domOp' is only available on the client."); *)
  (*          `Record []), *)
  (*    datatype "(a) -> ()"); *)

  "insertBefore",
  (`Client, datatype "(Xml, DomNode) ~> ()",
  IMPURE);

  "appendChildren",
  (`Client, datatype "(Xml, DomNode) ~> ()",
  IMPURE);

  "replaceNode",
  (`Client, datatype "(Xml, DomNode) ~> ()",
  IMPURE);

  "replaceDocument",
  (`Client, datatype "(Xml) ~> ()",
  IMPURE);

  "domInsertBeforeRef",
  (`Client, datatype "(DomNode, DomNode) ~> ()",
  IMPURE);

  "domAppendChildRef",
  (`Client, datatype "(DomNode, DomNode) ~> ()",
  IMPURE);

  "removeNode",
  (`Client, datatype "(DomNode) ~> ()",
  IMPURE);

  "cloneNode",
  (`Client, datatype "(DomNode, Bool) ~> (DomNode)",
  IMPURE);

  "domReplaceChildren",
  (`Client, datatype "(Xml, DomNode) ~> ()",
  IMPURE);

  "swapNodes",
  (`Client, datatype "(DomNode, DomNode) ~> ()",
  IMPURE);

  "getDocumentNode",
  (`Client, datatype "() ~> DomNode",
  IMPURE);

  "getNodeById",
  (`Client, datatype "(String) ~> DomNode",
  IMPURE);

  "getValue",
  (`Client, datatype "(DomNode) ~> Xml",
  IMPURE);

  "isNull",
  (`Client, datatype "(DomNode) ~> Bool",
  PURE);

  (* Create XML element from string, attributes and children *)
  "makeXml",
  (p3 (fun name attrList children -> match (attrList, children) with
    | (`List attrs, `List cs) -> `XML (Value.Node (Value.unbox_string(name), List.map (function
        | (`XML x) -> x
        | _ -> raise (runtime_type_error "non-XML in makeXml")
      ) cs @ (List.map (function
          | `Record [ ("1", key); ("2", value) ] -> Value.Attr (Value.unbox_string(key), Value.unbox_string(value))
        | _ -> raise (runtime_type_error "non-attr in makeXml")
      ) attrs))
    )
    | _ -> raise (runtime_type_error "non-XML in makeXml")),
  datatype "(String, [(String, String)], Xml) -> XmlItem",
  IMPURE);

  (* XML <-> variant conversion *)

  "xmlToVariant",
  (p1 (fun v ->
        match v with
          | `List xs ->
              `List (List.map
                (function
                   | (`XML x) -> Value.value_of_xmlitem x
                   | _ -> raise (runtime_type_error "non-XML passed to xmlToVariant")) xs)
          | _ -> raise (runtime_type_error "non-XML passed to xmlToVariant")),
  datatype "(Xml) ~> mu n.[ [|Text:String | Attr:(String, String) | Node:(String, n) | NsAttr: (String, String, String) | NsNode: (String, String, n) |] ]",
  IMPURE);

  "xmlItemToVariant",
  (p1 (fun v ->
      match v with
        | (`XML x) -> Value.value_of_xmlitem x
        | _ -> raise (runtime_type_error "non-XML passed to xmlItemToVariant")),
    datatype "(XmlItem) ~> mu n. [|Text:String | Attr:(String, String) | Node:(String, [ n ]) | NsAttr: (String, String, String) | NsNode: (String, String, [ n ]) |]",
    IMPURE);

  "variantToXml",
  (p1 (fun v -> `List (List.map (fun i -> `XML (i)) (Value.xml_of_variants v))),
   datatype "(mu n.[ [|Text:String | Attr:(String, String) | Node:(String, n) | NsAttr: (String, String, String) | NsNode: (String, String, n) |] ]) ~> Xml",
   IMPURE);

  "variantToXmlItem",
  (p1 (fun v -> `XML (Value.xmlitem_of_variant v)),
    datatype "(mu n. [| Text: String | Attr: (String, String) | Node: (String, [ n ]) | NsAttr: (String, String, String) | NsNode: (String, String, [ n ]) |]) ~> XmlItem",
    IMPURE);

  (* Section: Accessors for XML *)

  "getTagName",
  (p1 (fun v ->
         match v with
           | `List [ `XML (Value.Node (name, _)) ]      -> Value.box_string name
           | `List [ `XML (Value.NsNode (_, name, _)) ] -> Value.box_string name
           | _ -> raise (runtime_type_error "non-element passed to getTagName")),
  datatype "(Xml) ~> String",
  IMPURE);

  "getNamespace",
  (p1 (fun v -> match v with
    | `List [ `XML (Value.NsNode (ns, _, _)) ] -> Value.box_string ns
    | `List [ `XML (Value.Node (_, _)) ]       -> Value.box_string ""
    | _ -> raise (runtime_type_error "non-element passed to getNamespace")),
  datatype "(Xml) ~> String",
  IMPURE);


  "itemTextContent",
  (p1 (function
         | `XML (Value.Text str) -> Value.box_string str
         | _ -> raise (runtime_type_error "non-text node given to textContent")),
   datatype "(XmlItem) ~> String",
  IMPURE);

  "getAttributes",
  (p1 (fun v ->
    let attr_to_record = function
      | (Value.Attr (name, value)) -> `Record [("1", Value.box_string name); ("2", Value.box_string value)]
      | (Value.NsAttr (ns, name, value)) -> `Record [ ("1", Value.box_string (ns ^ ":" ^ name)); ("2", Value.box_string value) ]
      | _ -> assert false
    in match v with
      | `List [ `XML (Value.Node (_, children)) ]      -> `List (List.map attr_to_record (List.filter Value.is_attr children))
      | `List [ `XML (Value.NsNode (_, _, children)) ] -> `List (List.map attr_to_record (List.filter Value.is_attr children))
      | _ -> raise (runtime_type_error "non-element given to getAttributes")),
  datatype "(Xml) ~> [(String,String)]",
  IMPURE);

  "hasAttribute",
  (`Client, datatype "(Xml, String) ~> Bool",
  PURE);

  "getAttribute",
  (`Client, datatype "(Xml, String) ~> String",
  IMPURE);

  "not",
  (p1 (Value.unbox_bool ->- not ->- Value.box_bool),
   datatype "(Bool) -> Bool",
  PURE);


  (* Section: Accessors for DomNodes *)

  "domGetNodeValueFromRef",
  (`Client, datatype "(DomNode) ~> String",
  IMPURE);

  "domGetTagNameFromRef",
  (`Client, datatype "(DomNode) ~> String",
  IMPURE);

  "domGetPropertyFromRef",
  (`Client, datatype "(DomNode, String) ~> String",
  IMPURE);

  "domSetPropertyFromRef",
  (`Client, datatype "(DomNode, String, String) ~> ()",
  IMPURE);

  "domHasAttribute",
  (`Client, datatype "(DomNode, String) ~> Bool",
  IMPURE);

  "domRemoveAttributeFromRef",
  (`Client, datatype "(DomNode, String) ~> ()",
  IMPURE);

  "domGetAttributeFromRef",
  (`Client, datatype "(DomNode, String) ~> String",
  IMPURE);

  "domSetAttributeFromRef",
  (`Client, datatype "(DomNode, String, String) ~> ()",
  IMPURE);

  "domGetStyleAttrFromRef",
  (`Client, datatype "(DomNode, String) ~> String",
  IMPURE);

  "domGetChildrenFromRef",
  (`Client, datatype "(DomNode) ~> [ DomNode ]",
  IMPURE);

  "domSetStyleAttrFromRef",
  (`Client, datatype "(DomNode, String, String) ~> ()",
  IMPURE);

  (* Section:  Navigation for DomNodes *)
  "parentNode",
  (`Client, datatype "(DomNode) ~> DomNode",
  IMPURE);

  "firstChild",
  (`Client, datatype "(DomNode) ~> DomNode",
  IMPURE);

  "nextSibling",
  (`Client, datatype "(DomNode) ~> DomNode",
  IMPURE);

  (* Section: DOM Event API *)
  "getTarget",
  (`Client, datatype "(Event) ~> DomNode",
  PURE);

  "getTargetValue",
  (`Client, datatype "(Event) ~> String",
  PURE);

  "getTargetElement",
  (`Client, datatype "(Event) ~> DomNode",
  PURE);

  (* event handlers *)
  (* what effect annotation should the inner arrow have? *)
  "registerEventHandlers",
  (`PFun (fun _ -> assert false),
  datatype "([(String, (Event) { |e}~> ())]) ~> String",
  IMPURE);

  (* getPageX : (Event) -> Int *)
  "getPageX",
  (`Client, datatype "(Event) ~> Int",
  PURE);

  (* getPageY : (Event) -> Int *)
  "getPageY",
  (`Client, datatype "(Event) ~> Int",
  PURE);

  (* getFromElement : (Event) -> DomNode *)
  "getFromElement",
  (`Client, datatype "(Event) ~> DomNode",
  PURE);

  (* getToElement : (Event) -> DomNode *)
  "getToElement",
  (`Client, datatype "(Event) ~> DomNode",
  PURE);

  (* getTime : (Event) -> Int *)
  "getTime",
  (`Client, datatype "(Event) ~> Int",
  PURE);

  (* getCharCode : (Event) -> Int *)
  "getCharCode",
  (`Client, datatype "(Event) ~> Int",
  PURE);

  "getInputValue",
  (`Client, datatype "(String) ~> String",
  PURE);

  "getRadioGroupValue",
  (`Client, datatype "([String]) ~> String",
  PURE);

  "event",
  (`Client, datatype "Event",
  PURE);


  (* domSetAnchor : String -> () *)
  "domSetAnchor",
  (`Client, datatype "(String) ~> ()",
  IMPURE);

  "domGetAnchor",
  (`Client, datatype "() ~> String",
  IMPURE);

 (* Cookies *)
  "setCookie",
  (p2D (fun cookieName cookieVal req_data ->
         let cookieName = Value.unbox_string cookieName in
         let cookieVal = Value.unbox_string cookieVal in
         let resp_headers = RequestData.get_http_response_headers req_data in
         RequestData.set_http_response_headers req_data
             (("Set-Cookie", cookieName ^ "=" ^ cookieVal) :: resp_headers);
           `Record []
             (* Note: perhaps this should affect cookies returned by
                getcookie during the current request. *)),
   datatype "(String, String) ~> ()",
  IMPURE);


  (* WARNING:

     getCookie returns "" to indicate either that the cookie is not
     present or that the header is ill-formed (in debug mode a warning
     will also be sent to stderr if the header is ill-formed).

     Ideally, perhaps, a malformed header from the client should
     be ignored at this level (let the HTTP agents handle it).

     An absent cookie should probably be indicated by a None value in
     the Maybe(String) type.
  *)
  "getCookie",
  (p1D (fun name req_data ->
         let name = Value.unbox_string name in
         let cookies = RequestData.get_cookies req_data in
         let value =
           if List.mem_assoc name cookies then
             List.assoc name cookies
           else
             ""
         in
           Value.box_string value),
   datatype "(String) ~> String",
  IMPURE);

  (* getCommandOutput disabled for now; possible security risk. *)
  (*
    "getCommandOutput",
    (p1 ((Value.unbox_string ->- Utility.process_output ->- Value.box_string) :> result -> primitive),
    datatype "(String) -> String");
  *)

  "redirect",
  (p1D (fun url req_data ->
         let url = Value.unbox_string url in
           (* This is all quite hackish, just testing an idea. --ez *)
           let resp_headers = RequestData.get_http_response_headers req_data in
           RequestData.set_http_response_headers req_data (("Location", url) :: resp_headers);
           RequestData.set_http_response_code req_data 302;
           `Record []
      ), datatype "(String) ~> ()",
  IMPURE);
  (* Should this function really return?
     I think not --ez*)

  "sleep",
  (p1 (fun _ ->
         (* FIXME: This isn't right : it freezes all threads *)
         (*Unix.sleep (int_of_num (Value.unbox_int duration));
         `Record []*)
      runtime_error "The sleep function is not implemented on the server yet"
      ),
   datatype "(Int) ~> ()",
  IMPURE);

  "clientTime",
  (`Client,
   datatype "() ~> Int",
   IMPURE);

  "clientTimeMilliseconds",
  (`Client,
   datatype "() ~> Int",
   IMPURE);

  "serverTime",
  (`Server
     (`PFun (fun _ _ ->
               Value.box_int(int_of_float(Unix.time())))),
   datatype "() ~> Int",
   IMPURE);

  "serverTimeMilliseconds",
  (`Server
     (`PFun (fun _ _ ->
               Value.box_int(time_milliseconds()))),
   datatype "() ~> Int",
   IMPURE);

  "dateToInt",
  (p1 (fun r ->
         match r with
           | `Record r ->
               let lookup s =
                 Value.unbox_int (List.assoc s r) in
               let tm = {
                 Unix.tm_sec = lookup "seconds";
                Unix.tm_min = lookup "minutes";
                Unix.tm_hour = lookup "hours";
                Unix.tm_mday = lookup "day";
                Unix.tm_mon = lookup "month";
                Unix.tm_year = (lookup "year" - 1900);
                Unix.tm_wday = 0; (* ignored *)
                Unix.tm_yday =  0; (* ignored *)
                Unix.tm_isdst = false} in

               let t, _ = Unix.mktime tm in
                 Value.box_int (int_of_float t)
           | _ -> assert false),
   datatype "((year:Int, month:Int, day:Int, hours:Int, minutes:Int, seconds:Int)) ~> Int",
   IMPURE);

  "intToDate",
  (p1 (fun t ->
         let tm = Unix.localtime(float_of_int (Value.unbox_int t)) in
           `Record [
             "year", Value.box_int (tm.Unix.tm_year + 1900);
             "month", Value.box_int tm.Unix.tm_mon;
             "day", Value.box_int tm.Unix.tm_mday;
             "hours", Value.box_int tm.Unix.tm_hour;
             "minutes", Value.box_int tm.Unix.tm_min;
             "seconds", Value.box_int tm.Unix.tm_sec;
           ]),
  datatype "(Int) ~> (year:Int, month:Int, day:Int, hours:Int, minutes:Int, seconds:Int)",
  IMPURE);

  (* Database functions *)
  "AsList",
  (p1 (fun _ -> raise (internal_error "Unoptimized table access!!!")),
   datatype "(TableHandle(r, w, n)) {}-> [r]",
  IMPURE);

  "getDatabaseConfig",
  (`PFun
     (fun _ _ ->
    let args = from_option "" (Settings.get Database.connection_info) in
    match Settings.get DatabaseDriver.driver with
    | None ->
       raise (Errors.settings_error
                "Default database driver not defined. Set `database_driver`.")
    | Some driver ->
       `Record(["driver", Value.box_string driver;
                "args", Value.box_string args])),
   datatype "() ~> (driver:String, args:String)",
  IMPURE);

  "ord",
  (p1 (fun c -> Value.box_int (Char.code (Value.unbox_char c))),
   datatype "(Char) -> Int",
  PURE);

  "chr",
  (p1 (fun n -> (Value.box_char (Char.chr (Value.unbox_int n)))),
   datatype "(Int) -> Char",
  PURE);

  (* some trig functions *)
  "floor",   float_fn floor PURE;
  "ceiling", float_fn ceil PURE;
  "cos",     float_fn cos PURE;
  "sin",     float_fn sin PURE;
  "tan",     float_fn tan PURE;
  "log",     float_fn log PURE;
  "log10",   float_fn log10 PURE;
  "exp",     float_fn exp PURE;
  "sqrt",    float_fn sqrt PURE;

  ("environment",
   (`PFun (fun req_data _ ->
             let cgi_params = RequestData.get_cgi_parameters req_data in
             let makestrpair (x1, x2) = `Record [("1", Value.box_string x1); ("2", Value.box_string x2)] in
             let is_internal s = Str.string_match (Str.regexp "^_") s 0 in
               `List (List.map makestrpair (List.filter (not -<- is_internal -<- fst) cgi_params))),
    datatype "() ~> [(String,String)]",
    IMPURE));

  (* regular expression matching *)
  ("tilde",
   (p2 (fun s r ->
          let regex = Regex.compile_ocaml (Linksregex.Regex.ofLinks r)
          and string = Value.unbox_string s in
            Value.box_bool (Str.string_match regex string 0)),
    datatype "(String, Regex) -> Bool",
    PURE));

  (* regular expression matching with grouped matched results as a list *)
  ("ltilde",
    (`Server (p2 (fun s r ->
        let (re, ngroups) = Linksregex.Regex.ofLinksNGroups r
        and string = Value.unbox_string s in
    let regex = Regex.compile_ocaml re in
    match (Str.string_match regex string 0) with
     false -> `List []
    | _ ->
    (let rec accumMatches l : int -> Value.t = function
           0 -> `List ((Value.box_string (Str.matched_group 0 string))::l)
    |  i ->
    (try
    let m = Str.matched_group i string in
        accumMatches ((Value.box_string m)::l) (i - 1)
    with
       NotFound _ -> accumMatches ((`String "")::l) (i - 1)) in
       accumMatches [] ngroups))),
     datatype "(String, Regex) ~> [String]",
   PURE));

  (* regular expression substitutions --- don't yet support global substitutions *)
  ("stilde",
   (`Server (p2 (fun s r ->
        let open Regex in
        match Linksregex.Regex.ofLinks r with
    | Replace (l, t) ->
       let (regex, tmpl) = Regex.compile_ocaml l, t in
           let string = Value.unbox_string s in
           Value.box_string (Utility.decode_escapes (Str.replace_first regex tmpl string))
        | Any | StartAnchor | EndAnchor | Simply _ | Seq _ | Quote _ | Group _
          | Range _ | Alternate _ | Repeat _ -> assert false)),
    datatype "(String, Regex) ~> String",
    PURE));

(* FIXME: should these functions return a Maybe Char/Maybe String? *)
  (* String utilities *)
  ("charAt",
   (p2 (fun s i ->
      let int = Value.unbox_int in
        try
              Value.box_char ((Value.unbox_string s).[int i])
        with
        Invalid_argument _ -> runtime_error "charAt: invalid index"),
    datatype ("(String, Int) ~> Char"),
    IMPURE));

  ("strsub",
   (p3 (fun s start len ->
      let int = Value.unbox_int in
        try
          Value.box_string (String.sub (Value.unbox_string s) (int start) (int len))
        with
        Invalid_argument _ -> runtime_error "strsub: invalid arguments"),
    datatype "(String, Int, Int) ~> String",
    IMPURE));

  ("strlen",
   (p1 (fun s -> match s with
          | `String s -> `Int (String.length s)
          |  _ -> raise (runtime_type_error "strlen got wrong arguments")),
    datatype ("(String) ~> Int "),
    PURE));

  ("strescape",
   (p1 (function
          | `String s -> `String (String.escaped s)
          | _ -> raise (runtime_type_error "strescape got wrong arguments")),
   datatype ("(String) ~> String "),
   IMPURE));

  ("strunescape",
   (p1 (function
          | `String s -> `String (Scanf.unescaped s)
          | _ -> raise (runtime_type_error "Internal error: strunescape got wrong arguments")),
   datatype ("(String) ~> String "),
   IMPURE));

   ("strContains",
    (p2 (fun s c -> Value.box_bool (String.contains (Value.unbox_string s) (Value.unbox_char c))),
    datatype ("(String, Char) ~> Bool"),
    PURE)
  );

  ("implode",
   (p1 (fun l ->
           let chars = List.map Value.unbox_char (Value.unbox_list l) in
           let len = List.length chars in
           let s = Bytes.create len in
           let rec aux i l =
             match l with
               | [] -> ()
               | c :: cs -> Bytes.set s i c; aux (i + 1) cs
           in
             aux 0 chars;
             Value.box_string (Bytes.to_string s)),
    datatype ("([Char]) ~> String"),
    PURE));

  ("explode",
   (p1 (fun s -> match s with
               | `String s ->
               let rec aux i l =
             if i < 0 then
               l
             else
               aux (i - 1) (s.[i] :: l)
               in
               let chars = aux ((String.length s) - 1) [] in
             Value.box_list (List.map Value.box_char chars)
               | _  -> raise (runtime_type_error "Internal error: non-String in implode")),
    datatype ("(String) ~> [Char]"),
    PURE));

  ("unsafePickleCont",
   (*
     HACK:

     really we want this to be pickleCont, and to give it the type:

     (() -> Page) ~> String

     but the Page type is defined in the prelude, so pickleCont is also defined
     in the prelude and is just a wrapper for this function.
   *)
   (`Server (p1 (Serialisation.MarshalSerialiser.Value.save ->- Value.box_string)),
    datatype "(() { |e}-> a) ~> String",
    IMPURE));

  (* HACK *)
  ("unsafe_cast",
   (`Server (p1 (fun v -> v)),
    datatype "(a) ~> b",
    PURE));

  (* xml parser *)
  "parseXml",
  (`Server (p1 (fun v ->
                  `List [`XML(ParseXml.parse_xml (Value.unbox_string v))])),
   datatype "(String) -> Xml",
   IMPURE);

  (* non-deterministic random number generator *)
  "random",
  (`PFun (fun _ _ -> (Value.box_float (Random.float 1.0))),
   datatype "() -> Float",
   IMPURE);

    (* LINKS GAME LIBRARY *)

    (* GENERAL JAVASCRIPT / EVENTS *)

    "jsSetInterval",
    (`Client, datatype "(() ~e~> (), Int) ~e~> ()", IMPURE);

    "jsRequestAnimationFrame",
    (`Client, datatype "(() ~e~> ()) ~e~> ()", IMPURE);

    "jsSave",
    (`Client, datatype "(a) ~> ()", IMPURE);

    "jsRestore",
    (`Client, datatype "(a) ~> ()", IMPURE);

    "jsSetOnKeyDown",
    (`Client, datatype "(DomNode, (Event) ~e~> ()) ~e~> ()", IMPURE);

    "jsSetOnEvent",
    (`Client, datatype "(DomNode, String, (Event) ~e~> (), Bool) ~e~> ()", IMPURE);

    "jsSetWindowEvent",
    (`Client, datatype "(String, (Event) ~e~> (), Bool) ~e~> ()", IMPURE);

    "jsSetOnLoad",
    (`Client, datatype "((Event) ~e~> ()) ~e~> ()", IMPURE);

    (* GLOBAL STATE MANIPULATION *)

    "jsSaveGlobalObject",
    (`Client, datatype "(String, a) ~> ()", IMPURE);

    "jsLoadGlobalObject",
    (`Client, datatype "(String) ~> a", IMPURE);

    (* CANVAS SPECIFIC *)

    "jsGetContext2D",
    (`Client, datatype "(DomNode) ~> a", IMPURE); (* the a here should be something like Context2D *)

    "jsFillText",
    (`Client, datatype "(a, String, Float, Float) ~> ()", IMPURE);

    "jsCanvasFont",
    (`Client, datatype "(a, String) ~> ()", IMPURE);

    "jsDrawImage",
    (`Client, datatype "(a, DomNode, Float, Float) ~> ()", IMPURE);

    "jsFillRect",
    (`Client, datatype "(a, Float, Float, Float, Float) ~> ()", IMPURE);

    "jsFillCircle",
    (`Client, datatype "(a, Float, Float, Float) ~> ()", IMPURE);

    "jsBeginPath",
    (`Client, datatype "(a) ~> ()", IMPURE);

    "jsClosePath",
    (`Client, datatype "(a) ~> ()", IMPURE);

    "jsFill",
    (`Client, datatype "(a) ~> ()", IMPURE);

    "jsArc",
    (`Client, datatype "(a, Float, Float, Float, Float, Float, Bool) ~> ()", IMPURE);

    "jsMoveTo",
    (`Client, datatype "(a, Float, Float) ~> ()", IMPURE);

    "jsLineTo",
    (`Client, datatype "(a, Float, Float) ~> ()", IMPURE);

    "jsLineWidth",
    (`Client, datatype "(a, Float) ~> ()", IMPURE);

    "jsScale",
    (`Client, datatype "(a, Float, Float) ~> ()", IMPURE);

    "jsTranslate",
    (`Client, datatype "(a, Float, Float) ~> ()", IMPURE);

    "jsStrokeStyle",
    (`Client, datatype "(a, a) ~> ()", IMPURE);

    "jsStroke",
    (`Client, datatype "(a) ~> ()", IMPURE);

    "jsSetFillColor",
    (`Client, datatype "(a, String) ~> ()", IMPURE);

    "jsClearRect",
    (`Client, datatype "(a, Float, Float, Float, Float) ~> ()", IMPURE);

    "jsCanvasWidth",
    (`Client, datatype "(a) ~> Float", IMPURE);

    "jsCanvasHeight",
    (`Client, datatype "(a) ~> Float", IMPURE);

    "jsSaveCanvas",
    (`Client, datatype "(DomNode, DomNode, String) ~> ()", IMPURE);

    (* END OF LINKS GAME LIBRARY *)

    (* FOR DEBUGGING *)

    "debugGetStats",
    (`Client, datatype "(String) ~> a", IMPURE);

    "debugChromiumGC",
    (`Client, datatype "() ~> ()", IMPURE);

    (* END OF DEBUGGING FUNCTIONS *)


    (* EQUALITY *)

    "stringEq",
    (`Client, datatype "(String, String) -> Bool", PURE);

    "intEq",
    (`Client, datatype "(Int, Int) -> Bool", PURE);

    "floatEq",
    (`Client, datatype "(Float, Float) -> Bool", PURE);

    "floatNotEq",
    (`Client, datatype "(Float, Float) -> Bool", PURE);

    "objectEq",
    (`Client, datatype "(a, a) -> Bool", PURE);


    (* END OF EQUALITY FUNCTIONS *)

        "gensym",
        (let idx = ref 0 in
         `PFun (fun _ _ -> let i = !idx in idx := i+1; (Value.box_int i)),
         datatype "() -> Int",
         IMPURE);

    "connectSocket",
    (`Server (p2 (fun serverv portv ->
                  try
                    let server = Value.unbox_string serverv in
                    let port = Value.unbox_int portv in
                    let server_addr =
                      try  Unix.inet_addr_of_string server
                      with Failure _ ->
                        (Unix.gethostbyname server).Unix.h_addr_list.(0) in
                    let sockaddr = Unix.ADDR_INET(server_addr, port) in
                    let domain = Unix.domain_of_sockaddr sockaddr in
                    let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
                    Unix.connect sock sockaddr;
                    Unix.set_nonblock sock;
                    `Variant ("Just", Value.box_socket (Unix.in_channel_of_descr sock, Unix.out_channel_of_descr sock))
                  with _ -> `Variant ("Nothing", `Record []))),
     datatype "(String, Int) ~> [|Nothing|Just:Socket|]",
     IMPURE);
    "writeToSocket",
    (`Server (p2 (fun messagev socketv ->
                  let message = Value.unbox_string messagev in
                  let (_, outc) = Value.unbox_socket socketv in
                  output_string outc message;
                  flush outc;
                  `Record [])),
     datatype "(String, Socket) ~> ()",
     IMPURE);
    "readFromSocket",
    (`Server (p1 (fun socketv ->
                  let (inc, _) = Value.unbox_socket socketv in
                  try
                    let r = input_line inc in
                    `Variant ("Just", Value.box_string r)
                  with
                    Sys_blocked_io ->
                    `Variant ("Nothing", `Record []))),
     datatype "(Socket) ~> [|Nothing|Just:String|]",
     IMPURE);
    "closeSocket",
    (`Server (p1 (fun socketv ->
                  let (inc, _) = Value.unbox_socket socketv in
                  Unix.shutdown (Unix.descr_of_in_channel inc) Unix.SHUTDOWN_SEND;
                  `Record [])),
     datatype "(Socket) ~> ()",
     IMPURE);
    "addStaticRoute",
    (`PFun (fun _ -> assert false),
     datatype "(String, String, [(String, String)]) ~> ()",
     IMPURE);
    "unsafeAddRoute",
    (`PFun (fun _ -> assert false),
     (* The `hear' effects on the second argument (request handler)
        and third argument (error handler) should have different
        presence variables as they are executed in different
        contexts. *)
     datatype "(String, (String, Location) {hear{_}}~> a, (String, String, Location) {hear{_}}~> a) ~> ()",
     IMPURE);
    "servePages",
    (`PFun (fun _ -> assert false),
     datatype "() ~> ()",
     IMPURE);
    "serveWebsockets",
    (`PFun (fun _ -> assert false),
    datatype "() ~> ()",
    IMPURE);

    (* Crypt API *)

    "crypt",
    (`Server (p1 (Value.box_string -<- Bcrypt.string_of_hash -<- (fun s -> Bcrypt.hash ?count:None ?seed:None s) -<- Value.unbox_string)),
    datatype "(String) ~> String",
    PURE);

    "verify",
    (`Server (p2 (fun str -> Value.box_bool -<- (Bcrypt.verify (Value.unbox_string str)) -<- Bcrypt.hash_of_string -<- Value.unbox_string)),
    datatype "(String, String) ~> Bool",
    PURE);

    (* CLI *)
    "getArgs",
    (`Server
       (`PFun (fun _ _ ->
            Value.(box_list (List.map box_string (Settings.get_rest_arguments ()))))),
     datatype "() ~> [String]",
     IMPURE)
]

let impl : located_primitive -> primitive option = function
  | `Client -> None
  | `Server p
  | (#primitive as p) -> Some p

let nenv =
  List.fold_left
    (fun nenv (n, _) -> Env.String.bind n (Var.fresh_raw_var ()) nenv)
    Env.String.empty
    env

let venv =
  Env.String.fold
    (fun name var venv ->
       Env.Int.bind var name venv)
    nenv
    Env.Int.empty

let value_env : primitive option Env.Int.t =
  List.fold_right
    (fun (name, (p, _, _)) env ->
       Env.Int.bind (Env.String.find name nenv) (impl p) env)
    env
    Env.Int.empty

let maxvar =
  Env.String.fold
    (fun _name var x -> max var x)
    nenv 0

let minvar =
  Env.String.fold
    (fun _name var x -> min var x)
    nenv maxvar

let value_array : primitive option array =
  let array = Array.make (maxvar+1) None in
  List.iter (fun (name, (p, _, _)) ->
    Array.set array (Env.String.find name nenv) (impl p)) env;
  array

let is_primitive_var var =
  minvar <= var && var <= maxvar

let type_env : Types.environment =
  List.fold_right (fun (n, (_,t,_)) env -> Env.String.bind n t env) env Env.String.empty

let typing_env = {Types.var_env = type_env;
                  Types.rec_vars = StringSet.empty;
                  tycon_env = alias_env;
                  Types.effect_row = Types.closed_wild_row;
                  Types.desugared = false }

let primitive_names = StringSet.elements (Env.String.domain type_env)

let primitive_vars = Env.String.fold (fun _name var vars -> IntSet.add var vars) nenv IntSet.empty

let primitive_name n = Env.Int.find n venv

let primitive_location (name:string) =
  match fst3 (List.assoc name env) with
    | `Client    -> Location.Client
    | `Server _  -> Location.Server
    | #primitive -> Location.Unknown

let rec function_arity =
  let open Types in
  function
    | Function (Record row, _, _) ->
        let (l, _, _) = TypeUtils.extract_row_parts row in
        (Some (StringMap.size l))
    | ForAll (_, t) -> function_arity t
    | _ -> None

let primitive_arity (name : string) =
  let _, t, _ = List.assoc name env in
    function_arity t

(*let primitive_by_code var = Env.Int.lookup value_env var*)
(* use array instead? seems faster for primop-intensive code *)
let primitive_by_code var = Array.get value_array var



let primitive_stub (name : string) : Value.t =
  match Env.String.find_opt name nenv with
    | Some var ->
        begin
          match primitive_by_code var with
            | Some (#Value.t as r) -> r
            | Some _ -> `PrimitiveFunction (name,Some var)
            | None -> `ClientFunction name
        end
    | None -> assert false

(* jcheney: added to avoid Env.String.lookup *)
let primitive_stub_by_code (var : Var.var) : Value.t =
  let name = Env.Int.find var venv in
  match primitive_by_code var with
  | Some (#Value.t as r) -> r
  | Some _ -> `PrimitiveFunction (name,Some var)
  | None -> `ClientFunction name


(* jcheney: added to expose lookup by var *)
let apply_pfun_by_code var args req_data =
  match primitive_by_code var with
  | Some #Value.t ->
      raise (runtime_type_error ("Attempt to apply primitive non-function "
           ^ "(#" ^string_of_int var^ ")."))
  | Some (`PFun p) -> p req_data args
  | None -> assert false


let apply_pfun name args req_data =
  let var =
    Env.String.find name nenv
  in
  apply_pfun_by_code var args req_data

let is_primitive name = List.mem_assoc name env

let is_pure_primitive name =
  if List.mem_assoc name env then
    match List.assoc name env with
      | (_, _, PURE) -> true
      | _ -> false
  else
    false

(** Construct IR for application of the primitive [name] to the
    arguments [args]. *)
let prim_appln name args = Ir.Apply( Ir.Variable(Env.String.find name nenv),
                                  args)

let cohttp_server_response headers body req_data =
  let open Lwt in
  (* Debug.print (Printf.sprintf "Attempting to return:\n%s\n" body); *)
  let resp_headers = RequestData.get_http_response_headers req_data in
  let resp_code = RequestData.get_http_response_code req_data in
  let h = Cohttp.Header.add_list (Cohttp.Header.init ()) (headers @ resp_headers) in
  Cohttp_lwt_unix.Server.respond_string
    ?headers:(Some h)
    ~status:(Cohttp.Code.status_of_code resp_code)
    ~body:body
    () >>= fun resp -> Lwt.return (`Response resp)

(** Output the headers and content to stdout *)
let print_http_response headers body req_data =
  let resp_headers = RequestData.get_http_response_headers req_data in
  let resp_code = RequestData.get_http_response_code req_data in
  let headers = headers @ resp_headers  @
    if (resp_code <> 200) then
      [("Status", string_of_int resp_code)] else []
  in
    for_each headers
      (fun (name, value) -> print_endline(name ^ ": " ^ value));
    print_endline "";
    print_string body
