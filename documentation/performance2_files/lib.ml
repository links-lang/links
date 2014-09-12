open Sys
open Num
open List

open Notfound

open Value
open Types
open Utility
open Proc

(* Alias environment *)
module AliasEnv = Env.String

(* This is done in two stages because the datatype for regexes refers
   to the String alias *)
let alias_env : Types.tycon_environment = DefaultAliases.alias_env

let alias_env : Types.tycon_environment =
  AliasEnv.bind alias_env
    ("Regex", `Alias ([], (DesugarDatatypes.read ~aliases:alias_env Linksregex.Regex.datatype)))

let datatype = DesugarDatatypes.read ~aliases:alias_env

let cgi_parameters = ref []
(** http_response_headers: this is state for the webif interface. I hope we can
    find a better way for library functions to communicate with the web
    interface. *)
let http_response_headers = ref []
let http_response_code = ref 200

(*
  assumption:
    the only kind of lists that are allowed to be inserted into databases
    are strings
*)
let value_as_string db =
  function
    | `String s -> "\'" ^ db # escape_string s ^ "\'"
    | v -> string_of_value v

let cond_from_field db (k, v) =
  "("^ k ^" = "^ value_as_string db v ^")"

let single_match db =
  function
    | `Record fields -> "("^ (String.concat " AND " (map (cond_from_field db) fields)) ^")"
    | r -> failwith ("Internal error: forming query from non-row (single_match): "^string_of_value r)

let row_columns = function
  | `List ((`Record fields)::_) -> map fst fields
  | r -> failwith ("Internal error: forming query from non-row (row_columns): "^string_of_value r)
and row_values db = function
  | `List records ->
        (List.map (function
                     | `Record fields -> map (value_as_string db -<- snd) fields
                     | _ -> failwith "Internal error: forming query from non-row") records)
  | r -> failwith ("Internal error: forming query from non-row (row_values): "^string_of_value r)
(* and delete_condition db = function *)
(*   | `List(rows) -> "("^ (String.concat " OR " (map (single_match db) rows)) ^")" *)
(*   | r -> failwith ("Internal error: forming query from non-row (delete_condition): "^string_of_value r) *)
(* and updates db : Value.t -> string = function *)
(*   | `Record fields ->  *)
(*       let field (k, v) = (k ^" = "^ value_as_string db v) in *)
(*         (String.concat ", " (map field fields)) *)
(*   | r -> failwith ("Internal error: forming query from non-row: "^string_of_value r)  *)

type primitive =
[ Value.t
| `PFun of Value.t list -> Value.t ]

type pure = PURE | IMPURE

type located_primitive = [ `Client | `Server of primitive | primitive ]

let int_op impl pure : located_primitive * Types.datatype * pure =
  (`PFun (fun [x;y] -> `Int (impl (unbox_int x) (unbox_int y)))),
  datatype "(Int, Int) -> Int",
  pure

let float_op impl pure : located_primitive * Types.datatype * pure =
  `PFun (fun [x; y] -> `Float (impl (unbox_float x) (unbox_float y))),
  datatype "(Float, Float) -> Float",
  pure

let string_op impl pure : located_primitive * Types.datatype * pure =
  (`PFun (fun [x; y] -> `String (impl (unbox_string x) (unbox_string y)))),
  datatype "(String, String) -> String",
  pure

let conversion_op' ~unbox ~conv ~(box :'a->Value.t): Value.t list -> Value.t =
  fun [x] -> (box (conv (unbox x)))

let make_type_variable = Types.make_type_variable

let conversion_op ~from ~unbox ~conv ~(box :'a->Value.t) ~into pure : located_primitive * Types.datatype * pure =
  ((`PFun (conversion_op' ~unbox:unbox ~conv:conv ~box:box) : located_primitive),
   (let q, r = Types.fresh_row_quantifier (`Any, `Any) in
      (`ForAll (Types.box_quantifiers [q], `Function (make_tuple_type [from], r, into)) : Types.datatype)),
   pure)

let string_to_xml : Value.t -> Value.t = function
  | `String s -> `List [`XML (Text s)]
  | _ -> failwith "internal error: non-string value passed to xml conversion routine"

let char_test_op fn pure =
  (`PFun (fun [c] -> (`Bool (fn (unbox_char c)))),
   datatype "(Char) ~> Bool",
  pure)

let char_conversion fn pure =
  (`PFun (fun [c] ->  (box_char (fn (unbox_char c)))),
   datatype "(Char) -> Char",
  pure)

let float_fn fn pure =
  (`PFun (fun [c] ->  (box_float (fn (unbox_float c)))),
   datatype "(Float) -> Float",
  pure)

let p1 fn =
  `PFun (fun ([a]) -> fn a)
and p2 fn =
  `PFun (fun [a;b] -> fn a b)
and p3 fn =
  `PFun (fun [a;b;c] -> fn a b c)

let client_only_1 fn =
  p1 (fun _ -> failwith (Printf.sprintf "%s is not implemented on the server" fn))
let client_only_2 fn =
  p2 (fun _ _ -> failwith (Printf.sprintf "%s is not implemented on the server" fn))

let rec equal l r =
  match l, r with
    | `Bool l  , `Bool r   -> l = r
    | `Int l   , `Int r    -> eq_num l r
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
    | l, r ->  failwith ("Comparing "^ string_of_value l ^" with "^ string_of_value r ^" either doesn't make sense or isn't implemented")
and equal_lists l r =
  match l,r with
    | [], [] -> true
    | (l::ls), (r::rs) -> equal l r && equal_lists ls rs
    | _,_ -> false


let rec less l r =
  match l, r with
    | `Bool l, `Bool r   -> l < r
    | `Int l, `Int r     -> lt_num l r
    | `Float l, `Float r -> l < r
    | `Char l, `Char r -> l < r
    | `String l, `String r -> l < r
      (* Compare fields in lexicographic order of labels *)
    | `Record lf, `Record rf ->
        let order = sort (fun x y -> compare (fst x) (fst y)) in
        let lv, rv = map snd (order lf), map snd (order rf) in
        let rec compare_list = function
          | [] -> false
          | (l,r)::_ when less l r -> true
          | (l,r)::_ when less r l -> false
          | _::rest                -> compare_list rest in
          compare_list (combine lv rv)
    | `List (l), `List (r) -> less_lists (l,r)
    | l, r ->  failwith ("Cannot yet compare "^ string_of_value l ^" with "^ string_of_value r)
and less_lists = function
  | _, [] -> false
  | [], (_::_) -> true
  | (l::_), (r::_) when less l r -> true
  | (l::_), (r::_) when less r l -> false
  | (_::l), (_::r) -> less_lists (l, r)

let less_or_equal l r = less l r || equal l r

let add_attribute : Value.t * Value.t -> Value.t -> Value.t =
  fun (name,value) -> function
    | `XML (Node (tag, children)) ->
        let name = unbox_string name
        and value = unbox_string value in
        let rec filter = function
          | [] -> []
          | Attr (s, _) :: nodes when s=name -> filter nodes
          | node :: nodes -> node :: filter nodes
        in
          `XML (Node (tag, Attr (name, value) :: filter children))
    | r -> failwith ("cannot add attribute to " ^ string_of_value r)

let add_attributes : (Value.t * Value.t) list -> Value.t -> Value.t =
  List.fold_right add_attribute

let prelude_tyenv = ref None (* :-( *)
let prelude_nenv = ref None (* :-( *)

let env : (string * (located_primitive * Types.datatype * pure)) list = [
  "+", int_op (+/) PURE;
  "-", int_op (-/) PURE;
  "*", int_op ( */) PURE;
  "/", int_op (fun x y -> integer_num (x // y)) IMPURE;
  "^", int_op ( **/ ) PURE;
  "mod", int_op mod_num IMPURE;
  "+.", float_op (+.) PURE;
  "-.", float_op (-.) PURE;
  "*.", float_op ( *.) PURE;
  "/.", float_op (/.) PURE;
  "^.", float_op ( ** ) PURE;
  "^^", string_op ( ^ ) PURE;

  (** Comparisons *)
  "==",
  (p2 (fun v1 v2 -> box_bool (equal v1 v2)),
   datatype "(a,a) -> Bool",
   PURE);

  "<>",
  (p2 (fun v1 v2 -> box_bool (not (equal v1 v2))),
   datatype "(a,a) -> Bool",
   PURE);

  "<",
  (p2 (fun v1 v2 -> box_bool (less v1 v2)),
   datatype "(a,a) -> Bool",
   PURE);

  ">",
  (p2 (fun v1 v2 -> box_bool (less v2 v1)),
   datatype "(a,a) -> Bool",
   PURE);


  "<=",
  (p2 (fun v1 v2 -> box_bool (less_or_equal v1 v2)),
   datatype "(a,a) -> Bool",
   PURE);

  ">=",
  (p2 (fun v1 v2 -> box_bool (less_or_equal v2 v1)),
   datatype "(a,a) -> Bool",
   PURE);

  (** Conversions (any missing?) **)
  "intToString",   conversion_op ~from:(`Primitive `Int) ~unbox:unbox_int ~conv:string_of_num ~box:box_string ~into:Types.string_type PURE;
  "stringToInt",   conversion_op ~from:Types.string_type ~unbox:unbox_string ~conv:num_of_string ~box:box_int ~into:(`Primitive `Int) IMPURE;
  "intToFloat",    conversion_op ~from:(`Primitive `Int) ~unbox:unbox_int ~conv:float_of_num ~box:box_float ~into:(`Primitive `Float) PURE;
  "floatToInt",    conversion_op ~from:(`Primitive `Float) ~unbox:unbox_float ~conv:(num_of_int -<- int_of_float) ~box:box_int ~into:(`Primitive `Int) PURE;
  "floatToString", conversion_op ~from:(`Primitive `Float) ~unbox:unbox_float ~conv:string_of_float ~box:box_string ~into:Types.string_type PURE;
  "stringToFloat", conversion_op ~from:Types.string_type ~unbox:unbox_string ~conv:float_of_string ~box:box_float ~into:(`Primitive `Float) IMPURE;

  "stringToXml",
  ((p1 string_to_xml),
   datatype "(String) -> Xml",
  PURE);

  "intToXml",
  (`PFun (string_to_xml -<-
            (conversion_op' ~unbox:unbox_int ~conv:(string_of_num) ~box:box_string)),
   datatype "(Int) -> Xml",
  PURE);

  "floatToXml",
  (`PFun (string_to_xml -<-
            (conversion_op' ~unbox:unbox_float ~conv:(string_of_float) ~box:box_string)),
   datatype "(Float) -> Xml",
  PURE);

  "exit",
  (`Continuation Value.toplevel_cont,
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
             let attrs = List.map (fun p -> unbox_pair p) attrs in
               `List (List.map (add_attributes attrs) xmlitems)
         | _ -> failwith "Internal error: addAttributes takes an XML forest and a list of attributes"),
   datatype "(Xml, [(String, String)]) -> Xml",
   PURE);

  "send",
  (p2 (fun pid msg ->
         assert(false)), (* Now handled in evalir.ml *)
   datatype "(Process ({hear:a|_}), a) ~> ()",
   IMPURE);

  "self",
  (`PFun (fun _ -> `Int (num_of_int(Proc.get_current_pid()))),
   datatype "() ~e~> Process ({ |e })",
   IMPURE);

  "haveMail",
  (`PFun(fun _ ->
           failwith "The haveMail function is not implemented on the server yet"),
   datatype "() {:_|_}~> Bool",
   IMPURE);

  "recv",
  (* This function is not used, as its application is a special case
     in the interpreter. But we need it here (for now) to asign it a
     type. Ultimately we should probably not special-case it, but
     rather provide a way to implement this primitive from here.
     (Ultimately, it should perhaps be a true primitive (an AST node),
     because it uses a different evaluation mechanism from functions.
     -- jdy) *)
    (`PFun (fun ([]) -> assert false),
     datatype "() {:a|_}~> a",
  IMPURE);

  "spawn",
  (* This should also be a primitive, as described in the ICFP paper. *)
  (p1 (fun f ->
         if Settings.get_value Basicsettings.web_mode then
           failwith("Can't spawn at the server in web mode.");
         let var = Var.dummy_var in
         let cont = (`Local, var, Value.empty_env IntMap.empty,
                     ([], `Apply (`Variable var, []))) in
         let new_pid = Proc.create_process (cont::Value.toplevel_cont, f) in
           (`Int (num_of_int new_pid))),
   datatype "(() ~e~@ _) ~> Process ({ |e })",
   IMPURE);

  "spawnWait",
  (`Client,
   datatype "(() ~> a) ~> a",
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

  (** Sessions *)

  "give",
  (`PFun (fun _ -> assert false),
   (* datatype "(a::Any, !a::Any.s) ~> s::Session", *)
   datatype "forall a::Type(Any, Any), s::Type(Any, Session).(a, !a.s) ~> s",
   IMPURE);

  "grab",
  (`PFun (fun _ -> assert false),
   (* datatype "(?a::Any.s) ~> (a::Any, s::Session)", *)
   datatype "forall a::Type(Any, Any), s::Type(Any, Session). (?a.s) ~> (a, s)",
   IMPURE);

  (* access points *)
  "new",
  (`PFun (fun _ -> assert false),
   datatype "forall s::Type(Any, Session).() ~> AP(s)",
   IMPURE);

  "accept",
  (`PFun (fun _ -> assert false),
   datatype "forall s::Type(Any, Session).(AP(s)) ~> s",
   IMPURE);

  "request",
  (`PFun (fun _ -> assert false),
   datatype "forall s::Type(Any, Session).(AP(s)) ~> ~s",
   IMPURE);

  (** Lists and collections **)
  "Nil",
  (`List [],
   datatype "[a]",
   PURE);

  "Cons",
  (p2 (fun x xs ->
         box_list (x :: (unbox_list xs))),
   datatype "(a, [a]) -> [a]",
   PURE);

  "Concat",
  (p2 (fun xs ys ->
         box_list (unbox_list xs @ unbox_list ys)),
   datatype "([a], [a]) -> [a]",
   PURE);

  "hd",
  (p1 (fun list ->
         try
           (List.hd(unbox_list list))
         with
             Failure "hd" -> failwith "hd() of empty list"
      ),
   datatype "([a]) ~> a",
  IMPURE);

  "tl",
  (p1 (fun list ->
         try
           box_list(List.tl(unbox_list list))
         with
             Failure "tl" -> failwith "tl() of empty list"
      ),
   datatype "([a]) ~> [a]",
  IMPURE);

  "length",
  (p1 (unbox_list ->- List.length ->- num_of_int ->- box_int),
   datatype "([a]) -> Int",
  PURE);

  "take",
  (p2 (fun n l ->
         box_list (Utility.take (int_of_num (unbox_int n)) (unbox_list l))),
   datatype "(Int, [a]) ~> [a]",
  PURE);

  "drop",
  (p2 (fun n l ->
         box_list (Utility.drop (int_of_num (unbox_int n)) (unbox_list l))),
   datatype "(Int, [a]) ~> [a]",
  PURE);

  "max",
  (p1 (let max2 x y = if less x y then y else x in
         function
           | `List [] -> `Variant ("None", `Record [])
           | `List (x::xs) -> `Variant ("Some", List.fold_left max2 x xs)
           | _ -> failwith "Internal error: non-list passed to max"),
   datatype "([a]) ~> [|Some:a | None:()|]",
  PURE);

  "min",
  (p1 (let min2 x y = if less x y then x else y in
         function
           | `List [] -> `Variant ("None", `Record [])
           | `List (x::xs) -> `Variant ("Some", List.fold_left min2 x xs)
           | _ -> failwith "Internal error: non-list passed to min"),
   datatype "([a]) ~> [|Some:a | None:()|]",
  PURE);

  (** XML **)
  "childNodes",
  (p1 (function
         | `List [`XML (Node (_, children))] ->
             let children = filter (function (Node _) -> true | _ -> false) children in
               `List (map (fun x -> `XML x) children)
         | _ -> failwith "non-XML given to childNodes"),
   datatype "(Xml) -> Xml",
  IMPURE);

  "objectType",
  (`Client, datatype "(a) ~> String",
  IMPURE);

  "attribute",
  (p2 (let none = `Variant ("None", `Record []) in
         fun elem attr ->
             match elem with
               | `List ((`XML (Node (_, children)))::_) ->
                   let attr = unbox_string attr in
                   let attr_match = (function
                                       | Attr (k, _) when k = attr -> true
                                       | _ -> false) in
                     (try match List.find attr_match children with
                        | Attr (_, v) -> `Variant ("Some", box_string v)
                        | _ -> failwith "Internal error in `attribute'"
                      with NotFound _ -> none)
               | _ -> none),
   datatype "(Xml,String) -> [|Some:String | None:()|]",
  PURE);

  "alertDialog",
  (`Client, datatype "(String) ~> ()",
   IMPURE);

  "debug",
  (p1 (fun message -> Debug.print (unbox_string message);
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
  (p1 (fun msg -> print_endline (unbox_string msg); flush stdout; `Record []),
   datatype "(String) ~> ()",
  IMPURE);

  "javascript",
  (`Bool false, datatype "Bool",
  PURE);

  "not",
  (p1 (unbox_bool ->- not ->- box_bool),
   datatype "(Bool) -> Bool",
  PURE);

  "negate",
  (p1 (unbox_int ->- minus_num ->- box_int), datatype "(Int) -> Int",
  PURE);

  "negatef",
  (p1 (fun f -> box_float (-. (unbox_float f))), datatype "(Float) -> Float",
  PURE);

  "error",
  (p1 (unbox_string ->- failwith), datatype "(String) ~> a",
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

  "replaceChildren",
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

  (* Section: Accessors for XML *)
  "xmlToVariant",
  (`Server (p1 (fun v ->
                  match v with
                    | `List xs ->
                        `List (List.map (function
                                           | (`XML x) -> Value.value_of_xmlitem x
                                           | _ -> failwith "non-XML passed to xmlToVariant") xs)
                    | _ -> failwith "non-XML passed to xmlToVariant")),
   datatype "(Xml) ~> mu n.[ [|Text:String | Attr:(String, String) | Node:(String, n) |] ]",
   IMPURE);

  "getTagName",
  (p1 (fun v ->
         match v with
           | `List [`XML(Node(name, _))] ->
               box_string name
           | _ -> failwith "non-element passed to getTagName"),
  datatype "(Xml) ~> String",
  IMPURE);

  "getTextContent",
  (`Client, datatype "(Xml) ~> String",
  IMPURE);

  "getAttributes",
  (p1 (fun v ->
         match v with
           | `List [`XML(Node(_, children))] ->
               `List (map
                        (fun (Attr (name, value)) ->
                                `Record [("1", box_string name); ("2", box_string value)])
                        (filter (function (Attr _) -> true | _ -> false) children))
           | _ -> failwith "non-element given to getAttributes"),
   datatype "(Xml) ~> [(String,String)]",
   IMPURE);

  "hasAttribute",
  (`Client, datatype "(Xml, String) ~> Bool",
  PURE);

  "getAttribute",
  (`Client, datatype "(Xml, String) ~> String",
  IMPURE);

  (* Section: Navigation for XML *)
  "getChildNodes",
  (p1 (fun v ->
         match v with
           | `List [`XML(Node(_, children))] ->
               `List (map (fun x -> `XML(x)) (filter (function (Attr _) -> false | _ -> true) children))
           | _ -> failwith "non-element given to getChildNodes"),
   datatype "(Xml) ~> Xml",
  IMPURE);

  "not",
  (p1 (unbox_bool ->- not ->- box_bool),
   datatype "(Bool) -> Bool",
  PURE);


  (* Section: Accessors for DomNodes *)
  "domGetNodeValueFromRef",
  (`Client, datatype "(DomNode) ~> String",
  IMPURE);

  "domGetTagNameFromRef",
  (`Client, datatype "(DomNode) ~> String",
  IMPURE);

  "domHasAttribute",
  (`Client, datatype "(DomNode, String) ~> Bool",
  IMPURE);

  "domGetAttributeFromRef",
  (`Client, datatype "(DomNode, String) ~> String",
  IMPURE);

  "domSetAttributeFromRef",
  (`Client, datatype "(DomNode, String, String) ~> String",
  IMPURE);

  "domGetStyleAttrFromRef",
  (`Client, datatype "(DomNode, String) ~> String",
  IMPURE);

  "domSetStyleAttrFromRef",
  (`Client, datatype "(DomNode, String, String) ~> String",
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

  "registerEventHandlers",
  (`Client, datatype "([(String,(Event) -> ())]) ~> String",
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

  "event",
  (`Client, datatype "Event",
  PURE);

  (* Yahoo UI library functions we don't implement: *)
  (* # stopEvent : ??? *)
  (* # stopPropagation : ??? *)
  (* # preventDefault : ??? *)

  (* Cookies *)
  "setCookie",
  (p2 (fun cookieName cookieVal ->
         let cookieName = unbox_string cookieName in
         let cookieVal = unbox_string cookieVal in
           http_response_headers :=
             ("Set-Cookie", cookieName ^ "=" ^ cookieVal) :: !http_response_headers;
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
  (p1 (fun name ->
         let name = unbox_string name in
         let value =
           match getenv "HTTP_COOKIE" with
             | Some header ->
                 let cookies = Str.split (Str.regexp "[ \t]*;[ \t]*") header in
                 let cookies =
                   concat_map
                     (fun str ->
                        match Str.split (Str.regexp "[ \t]*=[ \t]*") str with
                          | [nm; vl] -> [nm, vl]
                          | _ -> Debug.print ("Warning: ill-formed cookie: "^str); [])
                     cookies
                 in
                   if List.mem_assoc name cookies then
                     List.assoc name cookies
                   else
                     ""
             | None -> ""
         in
           box_string value),
   datatype "(String) ~> String",
  IMPURE);

  (* getCommandOutput disabled for now; possible security risk. *)
  (*
    "getCommandOutput",
    (p1 ((unbox_string ->- Utility.process_output ->- box_string) :> result -> primitive),
    datatype "(String) -> String");
  *)

  "redirect",
  (p1 (fun url ->
         let url = unbox_string url in
           (* This is all quite hackish, just testing an idea. --ez *)
           http_response_headers := ("Location", url) :: !http_response_headers;
           http_response_code := 302;
           `Record []
      ), datatype "(String) ~> ()",
  IMPURE);
  (* Should this function really return?
     I think not --ez*)

  (** reifyK: I choose an obscure name, for an obscure function, until
      a better one can be thought up. It just turns a continuation into its
      string representation *)
  "reifyK",
  (p1 (function
           `Continuation k ->
             let s = marshal_continuation k in
               box_string s
         | _ -> failwith "argument to reifyK was not a continuation"
      ),
   datatype "((a) -> b) ~> String",
  IMPURE);
  (* arg type should actually be limited
     to continuations, but we don't have
     any way of specifying that in the
     type system. *)

  "sleep",
  (p1 (fun _ ->
         (* FIXME: This isn't right : it freezes all threads *)
         (*Unix.sleep (int_of_num (unbox_int duration));
         `Record []*)
         failwith "The sleep function is not implemented on the server yet"
      ),
   datatype "(Int) ~> ()",
  IMPURE);

  "clientTime",
  (`Client,
   datatype "() ~> Int",
   IMPURE);

  "serverTime",
  (`Server
     (`PFun (fun _ ->
               box_int(num_of_float(Unix.time())))),
   datatype "() ~> Int",
   IMPURE);

  "dateToInt",
  (p1 (fun r ->
         match r with
           | `Record r ->
               let lookup s =
                 int_of_num (unbox_int (List.assoc s r)) in
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
                 box_int (num_of_float t)
           | _ -> assert false),
   datatype "((year:Int, month:Int, day:Int, hours:Int, minutes:Int, seconds:Int)) ~> Int",
   IMPURE);

  "intToDate",
  (p1 (fun t ->
         let tm = Unix.localtime(float_of_num (unbox_int t)) in
         let box_int = box_int -<- num_of_int in
           `Record [
             "year", box_int (tm.Unix.tm_year + 1900);
             "month", box_int tm.Unix.tm_mon;
             "day", box_int tm.Unix.tm_mday;
             "hours", box_int tm.Unix.tm_hour;
             "minutes", box_int tm.Unix.tm_min;
             "seconds", box_int tm.Unix.tm_sec;
           ]),
  datatype "(Int) ~> (year:Int, month:Int, day:Int, hours:Int, minutes:Int, seconds:Int)",
  IMPURE);

  (** Database functions **)
  "AsList",
  (p1 (fun _ -> failwith "Unoptimized table access!!!"),
   datatype "(TableHandle(r, w, n)) -> [r]",
  IMPURE);

  "InsertRows",
  (`Server
     (p2 (fun table rows ->
            match table, rows with
              | `Table _, `List [] -> `Record []
              | `Table ((db, params), table_name, _), _ ->
                  let field_names = row_columns rows in
                  let vss = row_values db rows in
                    prerr_endline("RUNNING INSERT QUERY:\n" ^ (db#make_insert_query(table_name, field_names, vss)));
                    (Database.execute_insert (table_name, field_names, vss) db)
              | _ -> failwith "Internal error: insert row into non-database")),
   datatype "(TableHandle(r, w, n), [s]) ~> ()",
  IMPURE);

  (* FIXME:

     Choose a semantics for InsertReturning.

     Currently it is well-defined if exactly one row is inserted, but
     is not necessarily well-defined otherwise.

     Perhaps the easiest course of action is to restrict it to the
     case of inserting a single row.
  *)
  "InsertReturning",
  (`Server
     (p3 (fun table rows returning ->
            match table, rows, returning with
              | `Table _, `List [], _ ->
                  failwith "InsertReturning: undefined for empty list of rows"
              | `Table ((db, params), table_name, _), _, _ ->
                  let field_names = row_columns rows in
                  let vss = row_values db rows in

                  let returning = unbox_string returning in
                    prerr_endline("RUNNING INSERT ... RETURNING QUERY:\n" ^
                                    String.concat "\n"
                                    (db#make_insert_returning_query(table_name, field_names, vss, returning)));
                    (Database.execute_insert_returning (table_name, field_names, vss, returning) db)
              | _ -> failwith "Internal error: insert row into non-database")),
   datatype "(TableHandle(r, w, n), [s], String) ~> Int",
  IMPURE);

(*   "UpdateRows",  *)
(*   (`Server *)
(*      (p2 (fun table rows -> *)
(*             match table, rows with *)
(*               | (_:Value.t), (`List []:Value.t) -> `Record [] *)
(*               | `Table ((db, params), table_name, _), `List rows -> *)
(*                   List.iter (fun row -> *)
(*                                let query_string = *)
(*                                  "update " ^ table_name *)
(*                                  ^ " set " ^ updates db (snd (unbox_pair row)) *)
(*                                  ^ " where " ^ single_match db (fst (unbox_pair row)) *)
(*                                in *)
(*                                  prerr_endline("RUNNING UPDATE QUERY:\n" ^ query_string); *)
(*                                  ignore (Database.execute_command query_string db)) *)
(*                     rows; *)
(*                   `Record [])), *)
(*    datatype "(TableHandle(r, w, n), [(r, w)]) ~> ()", *)
(*   IMPURE); *)

(*   "DeleteRows",  *)
(*   (`Server *)
(*      (p2 (fun table rows -> *)
(*             match table, rows with *)
(*               | `Table _, `List [] -> `Record [] *)
(*               | `Table ((db, params), table_name, _), _  -> *)
(*                   let condition = delete_condition db rows in *)
(*                   let query_string = "delete from " ^ table_name ^ " where " ^ condition *)
(*                   in *)
(*                     prerr_endline("RUNNING DELETE QUERY:\n" ^ query_string); *)
(*                     (Database.execute_command query_string db) *)
(*               | _ -> failwith "Internal error: delete row from non-database")), *)
(*    datatype "(TableHandle(r, w, n), [r]) ~> ()", *)
(*   IMPURE); *)

  "getDatabaseConfig",
  (`PFun
     (fun _ ->
	let driver = Settings.get_value Basicsettings.database_driver
	and args = Settings.get_value Basicsettings.database_args in
	  if driver = "" then
	    failwith "Internal error: default database driver not defined"
	  else
	    `Record(["driver", box_string driver;
		     "args", box_string args])),
   datatype "() ~> (driver:String, args:String)",
  IMPURE);

  (** some char functions **)
  "isAlpha",  char_test_op Char.isAlpha PURE;
  "isAlnum",  char_test_op Char.isAlnum PURE;
  "isLower",  char_test_op Char.isLower PURE;
  "isUpper",  char_test_op Char.isUpper PURE;
  "isDigit",  char_test_op Char.isDigit PURE;
  "isXDigit", char_test_op Char.isXDigit PURE;
  "isBlank",  char_test_op Char.isBlank PURE;
  (* isCntrl, isGraph, isPrint, isPunct, isSpace *)

  "toUpper", char_conversion Char.uppercase PURE;
  "toLower", char_conversion Char.lowercase PURE;

  "ord",
  (p1 (fun c -> box_int (num_of_int (Char.code (unbox_char c)))),
   datatype "(Char) -> Int",
  PURE);

  "chr",
  (p1 (fun n -> (box_char (Char.chr (int_of_num (unbox_int n))))),
   datatype "(Int) -> Char",
  PURE);

  (* some trig functions *)
  "floor",   float_fn floor PURE;
  "ceiling", float_fn ceil PURE;
  "cos",     float_fn cos PURE;
  "sin",     float_fn sin PURE;
  "tan",     float_fn tan PURE;
  "log",     float_fn log PURE;
  "sqrt",    float_fn sqrt PURE;

  ("environment",
   (`PFun (fun _ ->
             let makestrpair (x1, x2) = `Record [("1", box_string x1); ("2", box_string x2)] in
             let is_internal s = Str.string_match (Str.regexp "^_") s 0 in
               `List (List.map makestrpair (List.filter (not -<- is_internal -<- fst) !cgi_parameters))),
    datatype "() ~> [(String,String)]",
    IMPURE));

  (* regular expression matching *)
  ("tilde",
   (p2 (fun s r ->
          let regex = Regex.compile_ocaml (Linksregex.Regex.ofLinks r)
          and string = unbox_string s in
            box_bool (Str.string_match regex string 0)),
    datatype "(String, Regex) -> Bool",
    PURE));

  (* regular expression matching with grouped matched results as a list *)
  ("ltilde",
    (`Server (p2 (fun s r ->
        let (re, ngroups) = Linksregex.Regex.ofLinksNGroups r
        and string = unbox_string s in
	let regex = Regex.compile_ocaml re in
	match (Str.string_match regex string 0) with
	 false -> `List []
	| _ ->
	(let rec accumMatches l : int -> Value.t = function
           0 -> `List ((box_string (Str.matched_group 0 string))::l)
	|  i ->
	(try
	let m = Str.matched_group i string in
        accumMatches ((box_string m)::l) (i - 1)
	with
	   NotFound _ -> accumMatches ((`String "")::l) (i - 1)) in
	   accumMatches [] ngroups))),
     datatype "(String, Regex) ~> [String]",
   PURE));

  (* regular expression substitutions --- don't yet support global substitutions *)
  ("stilde",
   (`Server (p2 (fun s r ->
	let Regex.Replace (l, t) = Linksregex.Regex.ofLinks r in
	let (regex, tmpl) = Regex.compile_ocaml l, t in
        let string = unbox_string s in
        box_string (Utility.decode_escapes (Str.replace_first regex tmpl string)))),
    datatype "(String, Regex) ~> String",
    PURE));

(* FIXME: should these functions return a Maybe Char/Maybe String? *)
  (* String utilities *)
  ("charAt",
   (p2 (fun s i ->
	  let int = Num.int_of_num -<- unbox_int in
	    try
              box_char ((unbox_string s).[int i])
	    with
		Invalid_argument _ -> failwith "charAt: invalid index"),
    datatype ("(String, Int) ~> Char"),
    IMPURE));

  ("strsub",
   (p3 (fun s start len ->
	  let int = Num.int_of_num -<- unbox_int in
	    try
	      box_string (String.sub (unbox_string s) (int start) (int len))
	    with
		Invalid_argument _ -> failwith "strsub: invalid arguments"),
    datatype "(String, Int, Int) ~> String",
    IMPURE));

  ("strlen",
   (p1 (fun s -> match s with
          | `String s -> `Int (Num.num_of_int (String.length s))
	  |  _ -> failwith "Internal error: strlen got wrong arguments"),
    datatype ("(String) ~> Int "),
    PURE));

  ("implode",
   (p1 (fun l ->
		   let chars = List.map unbox_char (unbox_list l) in
		   let len = List.length chars in
		   let s = String.create len in
		   let rec aux i l =
		     match l with
		       | [] -> ()
		       | c :: cs -> s.[i] <- c; aux (i + 1) cs
		   in
		     aux 0 chars;
		     box_string s),
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
			 box_list (List.map box_char chars)
	           | _  -> failwith "Internal error: non-String in implode"),
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
   (`Server (p1 (marshal_value ->- box_string)),
    datatype "(() -> a) ~> String",
    IMPURE));

  (* Serialize values to DB *)
  ("pickle_value",
   (`Server (p1 (fun v -> (box_string (marshal_value v)))),
    datatype "(a) ~> String",
    IMPURE));

  ("unpickle_value",
   (`Server (p1 (fun v -> assert false (*broken_unmarshal_value (unbox_string v)*))),
    datatype "(String) ~> a",
  IMPURE));

  (* HACK *)
  ("unsafe_cast",
   (`Server (p1 (fun v -> v)),
    datatype "(a) ~> b",
    PURE));

  (** xml parser *)
  "parseXml",
  (`Server (p1 (fun v ->
                  `List [`XML(ParseXml.parse_xml (unbox_string v))])),
   datatype "(String) -> Xml",
   IMPURE);

  (** non-deterministic random number generator *)
  "random",
  (`PFun (fun _ -> (box_float (Random.float 1.0))),
   datatype "() -> Float",
   IMPURE);

	"setCookie",
  (p2 (fun cookieName cookieVal ->
         let cookieName = unbox_string cookieName in
         let cookieVal = unbox_string cookieVal in
           http_response_headers :=
             ("Set-Cookie", cookieName ^ "=" ^ cookieVal) :: !http_response_headers;
           `Record []
             (* Note: perhaps this should affect cookies returned by
                getcookie during the current request. *)),
   datatype "(String, String) ~> ()",
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
	(`Client, datatype "(String, (Event) ~e~> ()) ~e~> ()", IMPURE);

	"jsSetOnEvent",
	(`Client, datatype "(String, String, (Event) ~e~> (), Bool) ~e~> ()", IMPURE);

	"jsSetOnLoad",
	(`Client, datatype "((Event) ~e~> ()) ~e~> ()", IMPURE);

	(* GLOBAL STATE MANIPULATION *)

	"jsSaveGlobalObject",
	(`Client, datatype "(String, a) ~> ()", IMPURE);

	(* maybe that could be pure *)
	"jsLoadGlobalObject",
	(`Client, datatype "(String) ~> a", IMPURE);

	(* CANVAS SPECIFIC *)

	"jsGetContext2D",
	(`Client, datatype "(DomNode) ~> a", IMPURE); (* the a here should be something like Context2D *)

	"jsFillText",
	(`Client, datatype "(a, String, Float, Float) ~> ()", IMPURE);

	"jsDrawImage",
	(`Client, datatype "(a, DomNode, Float, Float) ~> ()", IMPURE);

	"jsFillRect",
	(`Client, datatype "(a, Float, Float, Float, Float) ~> ()", IMPURE);

	"jsFillCircle",
	(`Client, datatype "(a, Float, Float, Float) ~> ()", IMPURE);

	"jsBeginPath",
	(`Client, datatype "(a) ~> ()", IMPURE);

	"jsMoveTo",
	(`Client, datatype "(a, Float, Float) ~> ()", IMPURE);

	"jsLineTo",
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


	(* END OF DEBUGGING FUNCTIONS *)


    "dumpTypes",
  (`Server (p1 (fun code ->
                  try
                    let ts = DumpTypes.program (val_of (!prelude_tyenv)) (unbox_string code) in

                    let line ({Lexing.pos_lnum=l}, _, _) = l in
                    let start ({Lexing.pos_bol=b; Lexing.pos_cnum=c}, _, _) = c-b in
                    let finish (_, {Lexing.pos_bol=b; Lexing.pos_cnum=c}, _) = c-b in

                    let box_int = num_of_int ->- box_int in

                    let resolve (name, t, pos) =
                      (* HACK: we need to be more principled about foralls  *)
                      let t =
                        match Types.concrete_type t with
                          | `ForAll (_, t) -> t
                          | _ -> t
                      in
                        `Record [("name", box_string name);
                                 ("t", box_string (Types.string_of_datatype t));
                                 ("pos", `Record [("line", box_int (line pos));
                                                  ("start", box_int (start pos));
                                                  ("finish", box_int (finish pos))])]
                    in
                      `Variant ("Success", box_list (List.map resolve ts))
                  with e ->
                    `Variant ("Failure", box_string(Errors.format_exception e ^ "\n"))
               )),
            datatype "(String) ~> [|Success:[(name:String, t:String, pos:(line:Int, start:Int, finish:Int))] | Failure:String|]",
            IMPURE)
]

(* HACK

   these functions are recursive, so type inference has no way of
   knowing that they are in fact tame
*)
let patch_prelude_funs tyenv =
  {tyenv with
     var_env =
      List.fold_right
        (fun (name, t) env -> Env.String.bind env (name, t))
        [("map", datatype "((a) -b-> c, [a]) -b-> [c]");
         ("concatMap", datatype "((a) -b-> [c], [a]) -b-> [c]");
         ("sortByBase", datatype "((a) -b-> (|_::Base), [a]) -b-> [a]");
         ("filter", datatype "((a) -b-> Bool, [a]) -b-> [a]")]
        tyenv.Types.var_env}

let impl : located_primitive -> primitive option = function
  | `Client -> None
  | `Server p
  | (#primitive as p) -> Some p

let nenv =
  List.fold_left
    (fun nenv (n, _) -> Env.String.bind nenv (n, Var.fresh_raw_var ()))
    Env.String.empty
    env

let venv =
  Env.String.fold
    (fun name var venv ->
       Env.Int.bind venv (var, name))
    nenv
    Env.Int.empty

let value_env : primitive option Env.Int.t =
  List.fold_right
    (fun (name, (p, _, _)) env ->
       Env.Int.bind env (Env.String.lookup nenv name, impl p))
    env
    Env.Int.empty

let maxvar =
  Env.String.fold
    (fun name var x -> max var x)
    nenv 0

let minvar =
  Env.String.fold
    (fun name var x -> min var x)
    nenv maxvar

let value_array : primitive option array =
  let array = Array.create (maxvar+1) None in
  List.iter (fun (name, (p, _, _)) ->
    Array.set array (Env.String.lookup nenv name) (impl p)) env;
  array

let is_primitive_var var =
  minvar <= var && var <= maxvar

let type_env : Types.environment =
  List.fold_right (fun (n, (_,t,_)) env -> Env.String.bind env (n, t)) env Env.String.empty

let typing_env = {Types.var_env = type_env; tycon_env = alias_env; Types.effect_row = Types.make_empty_open_row (`Any, `Any)}

let primitive_names = StringSet.elements (Env.String.domain type_env)

let primitive_vars = Env.String.fold (fun name var vars -> IntSet.add var vars) nenv IntSet.empty

let primitive_name = Env.Int.lookup venv

let primitive_location (name:string) =
  match fst3 (List.assoc name env) with
    | `Client ->  `Client
    | `Server _ -> `Server
    | #primitive -> `Unknown

let rec function_arity =
  function
    | `Function(`Record (l, _, _), _, _) ->
        (Some (StringMap.size l))
    | `ForAll (_, t) -> function_arity t
    | _ -> None

let primitive_arity (name : string) =
  let _, t, _ = assoc name env in
    function_arity t

(*let primitive_by_code var = Env.Int.lookup value_env var*)
(* use array instead? seems faster for primop-intensive code *)
let primitive_by_code var = Array.get value_array var



let primitive_stub (name : string) : Value.t =
  match Env.String.find nenv name with
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
  let name = Env.Int.lookup venv var in
  match primitive_by_code var with
  | Some (#Value.t as r) -> r
  | Some _ -> `PrimitiveFunction (name,Some var)
  | None -> `ClientFunction name


(* jcheney: added to expose lookup by var *)
let apply_pfun_by_code var args =
  match primitive_by_code var with
  | Some (#Value.t as r) ->
      failwith("Attempt to apply primitive non-function "
	       ^ "(#" ^string_of_int var^ ").")
  | Some (`PFun p) -> p args
  | None -> assert false


let apply_pfun name args =
  match Env.String.find nenv name with
    | Some var -> apply_pfun_by_code var args
    | None -> assert false



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
let prim_appln name args = `Apply(`Variable(Env.String.lookup nenv name),
                                  args)

(** Output the headers and content to stdout *)
let print_http_response headers body =
  let headers = headers @ !http_response_headers @
    if (!http_response_code <> 200) then
      [("Status", string_of_int !http_response_code)] else []
  in
    for_each headers
      (fun (name, value) -> print_endline(name ^ ": " ^ value));
    print_endline "";
    print_string body
