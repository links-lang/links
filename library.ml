open Sys
open Num
open List

open Result
open Types
open Utility

(* Data structures/utilities for proc mgmt *)

type pid = int

type proc_state = Result.continuation * Result.result 

let suspended_processes = (Queue.create () : (proc_state * pid) Queue.t)
and blocked_processes = (Hashtbl.create 10000 : (pid, (proc_state * pid)) Hashtbl.t)
and messages = (Hashtbl.create 10000  : (int, Result.result Queue.t) Hashtbl.t)
and current_pid = (ref 0 :  pid ref)
and main_process_pid = 0

let cgi_parameters = ref []

(** http_response_headers: this is state for the webif interface. I hope we can
    find a better way for library functions to communicate with the web
    interface. *)
let http_response_headers = ref []
let http_response_code = ref 200

(* default database settings *)
let database_driver = Settings.add_string("database_driver", "", `User)
let database_args = Settings.add_string("database_args", "", `User)

let debug_process_status () =
  prerr_endline("processes : " ^ 
                  string_of_int (Queue.length suspended_processes));
  prerr_endline ("blocked processes : " ^ 
                   string_of_int (Hashtbl.length blocked_processes))

let _ =   Hashtbl.add messages 0 (Queue.create ())

let fresh_pid =
  let current_pid = (ref 0 : pid ref) in
    fun () -> 
      begin
	incr current_pid;
	!current_pid
      end

(*
  assumption:
    the only kind of lists that are allowed to be inserted into databases
    are strings
*)
let value_as_string db = function
  | `List ((`Char _)::_) as c  -> "\'" ^ db # escape_string (charlist_as_string c) ^ "\'"
  | `List ([])  -> "\'\'"
  | (a) -> string_of_result a

let cond_from_field db (k, v) =
  "("^ k ^" = "^ value_as_string db v ^")"

let single_match db = 
  function
    | `Record fields -> "("^ (String.concat " AND " (map (cond_from_field db) fields)) ^")"
    | r -> failwith ("Internal error: forming query from non-row (single_match): "^string_of_result r)

let row_columns = function
  | `List ((`Record fields)::_) -> map fst fields
  | r -> failwith ("Internal error: forming query from non-row (row_columns): "^string_of_result r)
and row_values db = function
  | `List records ->
        (List.map (function
                     | `Record fields -> map (value_as_string db -<- snd) fields
                     | _ -> failwith "Internal error: forming query from non-row") records)
  | r -> failwith ("Internal error: forming query from non-row (row_values): "^string_of_result r)
and delete_condition db = function
  | `List(rows) -> "("^ (String.concat " OR " (map (single_match db) rows)) ^")"
  | r -> failwith ("Internal error: forming query from non-row (delete_condition): "^string_of_result r)
and updates db : Result.result -> string = function
  | `Record fields -> 
      let field (k, v) = (k ^" = "^ value_as_string db v) in
        (String.concat ", " (map field fields))
  | r -> failwith ("Internal error: forming query from non-row: "^string_of_result r) 

type primitive_value = [
  result
| `PFun of result list -> result
]

type primitive =  [
  Result.result
| `PFun of result list -> Result.result ]

type pure = PURE | IMPURE

type located_primitive = [ `Client | `Server of primitive | primitive ]

let datatype = DesugarDatatypes.read

let int_op impl pure : located_primitive * Types.datatype * pure = 
  (`PFun (fun [x;y] -> `Int (impl (unbox_int x) (unbox_int y)))),
  datatype "(Int, Int) -> Int",
  pure

let float_op impl pure : located_primitive * Types.datatype * pure = 
  `PFun (fun [x; y] -> `Float (impl (unbox_float x) (unbox_float y))),
  datatype "(Float, Float) -> Float",
  pure
    
let conversion_op' ~unbox ~conv ~(box :'a->result): result list -> result =
  fun [x] -> (box (conv (unbox x)))

let make_type_variable = Types.make_type_variable

let conversion_op ~from ~unbox ~conv ~(box :'a->result) ~into pure : located_primitive * Types.datatype * pure =
  ((`PFun (conversion_op' ~unbox:unbox ~conv:conv ~box:box) : located_primitive),
   (let a = Types.fresh_raw_variable () in
     (`ForAll ([`RigidTypeVar a], `Function (make_tuple_type [from], make_type_variable a, into)) : Types.datatype)),
   pure)

let string_to_xml : result -> result = function 
  | `List _ as c -> `List [`XML (Text (charlist_as_string c))]
  | _ -> failwith "internal error: non-string value passed to xml conversion routine"

let char_test_op fn pure = 
  (`PFun (fun [c] -> (`Bool (fn (unbox_char c)))),
   datatype "(Char) -> Bool",
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
    | `RecFunction _, `RecFunction _ -> 
        Pickle_result.pickleS l = Pickle_result.pickleS r
    | `Record lfields, `Record rfields -> 
        let rec one_equal_all = (fun alls (ref_label, ref_result) ->
                                   match alls with
                                     | [] -> false
                                     | (label, result) :: _ when label = ref_label -> equal result ref_result
                                     | _ :: alls -> one_equal_all alls (ref_label, ref_result)) in
          List.for_all (one_equal_all rfields) lfields && List.for_all (one_equal_all lfields) rfields
    | `Variant (llabel, lvalue), `Variant (rlabel, rvalue) -> llabel = rlabel && equal lvalue rvalue
    | `List (l), `List (r) -> length l = length r &&
            fold_left2 (fun result x y -> result && equal x y) true l r
    | `NativeString s1, `NativeString s2 -> s1 = s2
    | l, r ->  failwith ("Comparing "^ string_of_result l ^" with "^ string_of_result r ^" either doesn't make sense or isn't implemented")

let rec less l r =
  match l, r with
    | `Bool l, `Bool r   -> l < r
    | `Int l, `Int r     -> lt_num l r
    | `Float l, `Float r -> l < r
    | `Char l, `Char r -> l < r
    | `RecFunction _ , `RecFunction _
        -> Pickle_result.pickleS l < Pickle_result.pickleS r
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
    | l, r ->  failwith ("Cannot yet compare "^ string_of_result l ^" with "^ string_of_result r)
and less_lists = function
  | _, [] -> false
  | [], (_::_) -> true
  | (l::_), (r::_) when less l r -> true
  | (l::_), (r::_) when less r l -> false
  | (_::l), (_::r) -> less_lists (l, r)

let less_or_equal l r = less l r || equal l r

let add_attribute : result * result -> result -> result = 
  fun (name,value) -> function
    | `XML (Node (tag, children)) ->
        let name = unbox_string name
        and value = unbox_string value in
        let rec filter = function
          | [] -> []
          | (Attr (s, _) as node) :: nodes when s=name -> filter nodes
          | node :: nodes -> node :: filter nodes
        in
          `XML (Node (tag, Attr (name, value) :: filter children)) 
    | r -> failwith ("cannot add attribute to " ^ Result.string_of_result r)

let add_attributes : (result * result) list -> result -> result =
  List.fold_right add_attribute

let prelude_env = ref None

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
  (`Continuation Result.toplevel_cont,
  (* Return type must be free so that it unifies with things that
     might be used alternatively. E.g.: 
     if (test) exit(1) else 42 *)
   datatype "(a) -> b",
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
         let pid = int_of_num (unbox_int pid) in
           (try 
              Queue.push msg (Hashtbl.find messages pid)
            with NotFound _ -> 
              (* Is this really an internal error? Maybe target has finished? *)
              failwith ("Internal error while sending message: no mailbox for "
                        ^ string_of_int pid));
           (try 
              Queue.push(Hashtbl.find blocked_processes pid) suspended_processes;
              Hashtbl.remove blocked_processes pid
            with NotFound _ -> ());
           `Record []),
   datatype "(Mailbox (a), a) -{b}-> ()",
  IMPURE);

  "self",
  (`PFun (fun _ -> `Int (num_of_int !current_pid)),
   datatype "() -{a}-> Mailbox (a)",
  IMPURE);

  "recv",
  (* this function is not used, as its application is a special case
     in the interpreter. But we need it here, for now, because of its
     type.  Ultimately we should probably not special-case it, but
     rather provide a way to implement this primitive from here.
     (Ultimately, it should perhaps be a true primitive (an AST node),
     because it uses a different evaluation mechanism from functions.
     -- jdy) *)
    (p1 (fun _ -> assert false),
     datatype "() -{a}-> (a)",
  IMPURE);

  "spawn",
  (* This should also be a primitive, as described in the ICFP paper. *)
  (p1 (fun f ->
         let new_pid = fresh_pid () in
           Hashtbl.add messages new_pid (Queue.create ());
           Queue.push ((ApplyCont(empty_env, []) :: toplevel_cont, f), new_pid) suspended_processes;
           (`Int (num_of_int new_pid))),
   (*
     a: spawn's mailbox type (ignored, since spawn doesn't recv)
     b: the mailbox type of the spawned process
     c: the parameter expected by the process function
     d: the return type of the spawned process function (ignored)
   *)
   datatype "(() -{b}-> d) -> Mailbox (b)",
  IMPURE);
(*   datatype "Mailbox (a) -> (Mailbox (b) -> c -> d) -> Mailbox (a) -> c -> Mailbox (b)");*)

  "spawnWait",
  (`Client, datatype "(() -{b}-> d) -> d",
  IMPURE);

  "_MAILBOX_",
  (`Int (num_of_int 0),
   (let u = fresh_type_variable () in
      (* Deliberately non-quantified type.  Mailboxes are
         non-polymorphic, so this is a so-called "weak type
         variable". *)
      u),
   PURE);

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
   datatype "([a]) -> a",
  IMPURE);

  "tl", 
  (p1 (fun list ->
         try 
           box_list(List.tl(unbox_list list))
         with
             Failure "tl" -> failwith "tl() of empty list"
      ),
   datatype "([a]) -> [a]",
  IMPURE);
  
  "length", 
  (p1 (unbox_list ->- List.length ->- num_of_int ->- box_int),
   datatype "([a]) -> Int",
  PURE);

  "take",
  (p2 (fun n l ->
         box_list (Utility.take (int_of_num (unbox_int n)) (unbox_list l))),
   datatype "(Int, [a]) -> [a]",
  PURE);

  "drop",
  (p2 (fun n l ->
         box_list (Utility.drop (int_of_num (unbox_int n)) (unbox_list l))),
   datatype "(Int, [a]) -> [a]",
  PURE);

  "max",
  (p1 (let max2 x y = if less x y then y else x in
         function
           | `List [] -> `Variant ("None", `Record [])
           | `List (x::xs) -> `Variant ("Some", List.fold_left max2 x xs)
           | _ -> failwith "Internal error: non-list passed to max"),
   datatype "([a]) -> [|Some:a | None:()|]",
  PURE);

  "min",
  (p1 (let min2 x y = if less x y then x else y in
         function
           | `List [] -> `Variant ("None", `Record [])
           | `List (x::xs) -> `Variant ("Some", List.fold_left min2 x xs)
           | _ -> failwith "Internal error: non-list passed to min"),
   datatype "([a]) -> [|Some:a | None:()|]",
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
  (`Client, datatype "(a) -> String",
  IMPURE);

  "attribute",
  (p2 (let none = `Variant ("None", `Record []) in
         fun elem attr ->
             match elem with
               | `List ((`XML (Node (_, children)))::_) -> 
                   let attr = charlist_as_string attr in
                   let attr_match = (function
                                       | Attr (k, _) when k = attr -> true
                                       | _ -> false) in
                     (try match find attr_match children with
                        | Attr (_, v) -> `Variant ("Some", string_as_charlist v)
                        | _ -> failwith "Internal error in `attribute'"
                      with NotFound _ -> none)
               | _ -> none),
   datatype "(Xml,String) -> [|Some:String | None:()|]",
  PURE);
  
  "alertDialog",
  (`Client, datatype "(String) -> ()",
   IMPURE);

  "debug", 
  (p1 (fun message -> prerr_endline (unbox_string message); flush stderr;
                      `Record []),
   datatype "(String) -> ()",
  IMPURE);

  "debugObj",
  (`Client, datatype "(a) -> ()",
  IMPURE);
  
  "dump",
  (`Client, datatype "(a) -> ()",
  IMPURE);
  
  "textContent",
  (`Client, datatype "(DomNode) -> String",
  IMPURE);

  "print",
  (p1 (fun msg -> print_endline (unbox_string msg); flush stdout; `Record []),
   datatype "(String) -> ()",
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
  (p1 (unbox_string ->- failwith), datatype "(String) -> a",
  IMPURE);
  
  (* HACK *)
  (*   [DEACTIVATED] *)
  (*   "callForeign", *)
  (*    (`Client, datatype "((a) -> b) -> (a) -> b"); *)

  (* DOM API *)

  "isElementNode",
  (`Client, datatype "(DomNode) -> Bool",
  PURE);


  (* [DEACTIVATED] *)
  (*   "domOp", *)
  (*   (p1 (fun message -> failwith("`domOp' is only available on the client."); *)
  (*          `Record []), *)
  (*    datatype "(a) -> ()"); *)

  "insertBefore",
  (`Client, datatype "(Xml, DomNode) -> ()",
  IMPURE);

  "appendChildren",
  (`Client, datatype "(Xml, DomNode) -> ()",
  IMPURE);

  "replaceNode",
  (`Client, datatype "(Xml, DomNode) -> ()",
  IMPURE); 

  "replaceDocument",
  (`Client, datatype "(Xml) -> ()",
  IMPURE); 

  "domInsertBeforeRef",
  (`Client, datatype "(DomNode, DomNode) -> ()",
  IMPURE);

  "domAppendChildRef",
  (`Client, datatype "(DomNode, DomNode) -> ()",
  IMPURE);

  "removeNode",
  (`Client, datatype "(DomNode) -> ()",
  IMPURE);

  "replaceChildren",
  (`Client, datatype "(Xml, DomNode) -> ()",
  IMPURE);

  "swapNodes",
  (`Client, datatype "(DomNode, DomNode) -> ()",
  IMPURE);

  "getDocumentNode",
  (`Client, datatype "() -> DomNode",
  IMPURE);

  "getNodeById",
  (`Client, datatype "(String) -> DomNode",
  IMPURE);

  "getValue",
  (`Client, datatype "(DomNode) -> Xml",
  IMPURE);

  "isNull",
  (`Client, datatype "(DomNode) -> Bool",
  PURE);

  (* Section: Accessors for XML *)
  "getTagName",
  (`Client, datatype "(Xml) -> String",
  IMPURE);

  "getTextContent",
  (`Client, datatype "(Xml) -> String",
  IMPURE);

  "getAttributes",
  (`Client, datatype "(Xml) -> [(String,String)]",
  IMPURE);

  "hasAttribute",
  (`Client, datatype "(Xml, String) -> Bool",
  PURE);

  "getAttribute",
  (`Client, datatype "(Xml, String) -> String",
  IMPURE);

  (* Section: Navigation for XML *)
  "getChildNodes",
  (`Client, datatype "(Xml) -> Xml",
  PURE);

  (* Section: Accessors for DomNodes *)
  "domGetNodeValueFromRef",
  (`Client, datatype "(DomNode) -> String",
  IMPURE);

  "domGetTagNameFromRef",
  (`Client, datatype "(DomNode) -> String",
  IMPURE);

  "domGetAttributeFromRef",
  (`Client, datatype "(DomNode, String) -> String",
  IMPURE);

  "domSetAttributeFromRef",
  (`Client, datatype "(DomNode, String, String) -> String",
  IMPURE);

  "domGetStyleAttrFromRef",
  (`Client, datatype "(DomNode, String) -> String",
  IMPURE);

  "domSetStyleAttrFromRef",
  (`Client, datatype "(DomNode, String, String) -> String",
  IMPURE);

  (* Section:  Navigation for DomNodes *)
  "parentNode",
  (`Client, datatype "(DomNode) -> DomNode",
  IMPURE);

  "firstChild",
  (`Client, datatype "(DomNode) -> DomNode",
  IMPURE);
  
  "nextSibling",
  (`Client, datatype "(DomNode) -> DomNode",
  IMPURE);

  (* Section: DOM Event API *)
  "getTarget",
  (`Client, datatype "(Event) -> DomNode",
  PURE);

  "getTargetValue",
  (`Client, datatype "(Event) -> String",
  PURE);

  "getTargetElement",
  (`Client, datatype "(Event) -> DomNode",
  PURE);

  "registerEventHandlers",
  (`Client, datatype "([(String,(Event)->())]) -> String",
  IMPURE);

  (* getPageX : (Event) -> Int *)
  "getPageX",
  (`Client, datatype "(Event) -> Int",
  PURE);

  (* getPageY : (Event) -> Int *)
  "getPageY",
  (`Client, datatype "(Event) -> Int",
  PURE);

  (* getFromElement : (Event) -> DomNode *)
  "getFromElement",
  (`Client, datatype "(Event) -> DomNode",
  PURE);

  (* getToElement : (Event) -> DomNode *)
  "getToElement",
  (`Client, datatype "(Event) -> DomNode",
  PURE);

  (* getTime : (Event) -> Int *)
  "getTime",
  (`Client, datatype "(Event) -> Int",
  PURE);

  (* getCharCode : (Event) -> Int *)
  "getCharCode",
  (`Client, datatype "(Event) -> Int",
  PURE);

  "getInputValue",
  (`Client, datatype "(String) -> String",
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
         let cookieName = charlist_as_string cookieName in
         let cookieVal = charlist_as_string cookieVal in
           http_response_headers := 
             ("Set-Cookie", cookieName ^ "=" ^ cookieVal) :: !http_response_headers;
           `Record []
             (* Note: perhaps this should affect cookies returned by
                getcookie during the current request. *)),
   datatype "(String, String) -> ()",
  IMPURE);


  (* WARNING:

     getCookie returns "" to indicate either that the cookie is not
     present or that the header is ill-formed (in debug mode a warning
     will also be sent to stderr if the header is ill-formed).

     When we add exceptions we may want to change the semantics to throw an
     exception instead of returning "".
  *)
  "getCookie",
  (p1 (fun name ->
         let name = charlist_as_string name in
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
   datatype "(String) -> String",
  IMPURE);

  (* getCommandOutput disabled for now; possible security risk. *)
  (*
    "getCommandOutput",
    (p1 ((unbox_string ->- Utility.process_output ->- box_string) :> result -> primitive),
    datatype "(String) -> String");
  *)

  "redirect",
  (p1 (fun url ->
         let url = charlist_as_string url in
           (* This is all quite hackish, just testing an idea. --ez *)
           http_response_headers := ("Location", url) :: !http_response_headers;
           http_response_code := 302;
           `Record []
      ), datatype "(String) -> ()",
  IMPURE);
  (* Should this function really return? 
     I think not --ez*)

  (** reifyK: I choose an obscure name, for an obscure function, until
      a better one can be though up. It just turns a continuation into its
      string representation *)
  "reifyK",
  (p1 (function
           `Continuation k -> 
             let k = minimize k in
               (match string_as_charlist(marshal_continuation k) with
                  `List _ as result -> result
                | _ -> assert(false))
         | _ -> failwith "argument to reifyK was not a continuation"
      ),
   datatype "((a) -> b) -> String",
  IMPURE);
  (* arg type should actually be limited
     to continuations, but we don't have
     any way of specifying that in the 
     type system. *)

  "sleep",
  (p1 (fun duration ->
         (* FIXME: This isn't right : it freezes all threads *)
         (*Unix.sleep (int_of_num (unbox_int duration));
         `Record []*)
         failwith "The sleep function is not implemented on the server yet"
      ),
   datatype "(Int) -> ()",
  IMPURE);

  "clientTime",
  (`Client,
   datatype "() -> Int",
   IMPURE);
  
  "serverTime",
  (`Server
     (`PFun (fun _ ->
               box_int(num_of_float(Unix.time())))),
   datatype "() -> Int",
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
   datatype "((year:Int, month:Int, day:Int, hours:Int, minutes:Int, seconds:Int)) -> Int",
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
  datatype "(Int) -> (year:Int, month:Int, day:Int, hours:Int, minutes:Int, seconds:Int)",
  IMPURE);

  (** Database functions **)
  "asList",
  (p1 (fun _ -> failwith "Unoptimized table access!!!"),
   datatype "(TableHandle(r, w)) -> [r]",
  IMPURE);

  "insertrows",
  (`Server 
     (p2 (fun table rows -> 
            match table, rows with
              | `Table _, `List [] -> `Record []
              | `Table ((db, params), table_name, _), _ ->
                  let field_names = row_columns rows
                  and vss = row_values db rows
                  in
                    prerr_endline("RUNNING INSERT QUERY:\n" ^ (db#make_insert_query(table_name, field_names, vss)));
                    (Database.execute_insert (table_name, field_names, vss) db)
              | _ -> failwith "Internal error: insert row into non-database")),
   datatype "(TableHandle(r, w), [w]) -> ()",
  IMPURE);

  "updaterows", 
  (`Server
     (p2 (fun table rows ->
            match table, rows with
              | (_:result), (`List []:result) -> `Record []
              | `Table ((db, params), table_name, _), `List rows ->
                  List.iter (fun row ->
                               let query_string =
                                 "update " ^ table_name
                                 ^ " set " ^ updates db (links_snd row)
                                 ^ " where " ^ single_match db (links_fst row)
                               in
                                 prerr_endline("RUNNING UPDATE QUERY:\n" ^ query_string);
                                 ignore (Database.execute_command query_string db))
                    rows;
                  `Record [])),
   datatype "(TableHandle(r, w), [(r, w)]) -> ()",
  IMPURE);

  "deleterows", 
  (`Server
     (p2 (fun table rows ->
            match table, rows with
              | `Table _, `List [] -> `Record []
              | `Table ((db, params), table_name, _), _  ->
                  let condition = delete_condition db rows in
                  let query_string = "delete from " ^ table_name ^ " where " ^ condition
                  in
                    prerr_endline("RUNNING DELETE QUERY:\n" ^ query_string);
                    (Database.execute_command query_string db)
              | _ -> failwith "Internal error: delete row from non-database")),
   datatype "(TableHandle(r, w), [r]) -> ()",
  IMPURE);

  "getDatabaseConfig",
  (`PFun
     (fun _ ->
	let driver = Settings.get_value database_driver
	and args = Settings.get_value database_args in
	  if driver = "" then
	    failwith "Internal error: default database driver not defined"
	  else
	    `Record(["driver", string_as_charlist driver;
		     "args", string_as_charlist args])),
   datatype "() -> (driver:String, args:String)",
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
   (`PFun (fun [] -> 
             let makestrpair (x1, x2) = `Record [("1", box_string x1); ("2", box_string x2)] in
             let is_internal s = Str.string_match (Str.regexp "^_") s 0 in
               `List (List.map makestrpair (List.filter (not -<- is_internal -<- fst) !cgi_parameters))),
    datatype "() -> [(String,String)]",
    IMPURE));

  (* regular expression matching *)
  ("tilde",
   (p2 (fun s r -> 
          let regex = Regex.compile_ocaml (Linksregex.Regex.ofLinks r)
          and string = unbox_string s in
            box_bool (Str.string_match regex string 0)),
    datatype "(String, Regex) -> Bool",
    PURE));

  (* All functions below are currenly server only; but client version should be relatively easy to provide *)
  ("ntilde",
   (p2 (fun s r -> 
          let regex = Regex.compile_ocaml (Linksregex.Regex.ofLinks r)
	  and string = (match s with `NativeString ss -> ss | _ -> failwith "Internal error: expected NativeString") in
        box_bool (Str.string_match regex string 0)),
    datatype "(NativeString, Regex) -> Bool",
    PURE));

  (* regular expression matching with grouped matched results as a list *)
  ("ltilde",	
    (`Server (p2 (fun s r ->
        let (re, ngroups) = (Linksregex.Regex.ofLinksNGroups r) 
        and string = unbox_string s in
	let regex = Regex.compile_ocaml re in
	match (Str.string_match regex string 0) with
	 false -> `List []
	| _ -> 
	(let rec accumMatches l : int -> Result.result = function 
           0 -> `List ((box_string (Str.matched_group 0 string))::l)
	|  i -> 
	(try
	let m = Str.matched_group i string in 
        accumMatches ((box_string m)::l) (i - 1)
	with 
	   NotFound _ -> accumMatches ((`List [])::l) (i - 1)) in
	accumMatches [] ngroups))),
     datatype "(String, Regex) -> [String]",
   PURE));

  ("lntilde",	
   (`Server (p2 (fun s r ->
        let (re, ngroups) = (Linksregex.Regex.ofLinksNGroups r) 
        and string = (match s with `NativeString ss -> ss | _ -> failwith "Internal error: expected NativeString") in
	let regex = Regex.compile_ocaml re in
	match (Str.string_match regex string 0) with
	 false -> `List []
	| _ -> 
	(let rec accumMatches l : int -> Result.result = function 
           0 -> `List ((box_string (Str.matched_group 0 string))::l)
	|  i -> 
	(try
	let m = Str.matched_group i string in 
        accumMatches ((box_string m)::l) (i - 1)
	with 
	   NotFound _ -> accumMatches ((`List [])::l) (i - 1)) in
	accumMatches [] ngroups))),
    datatype "(NativeString, Regex) -> [String]",
    PURE));

  (* regular expression substitutions --- don't yet support global substitutions *)
  ("stilde",	
   (`Server (p2 (fun s r ->
	let Regex.Replace(l, t) = Linksregex.Regex.ofLinks r in 
	let (regex, tmpl) = Regex.compile_ocaml l, t in
        let string = unbox_string s in
        box_string (Utility.decode_escapes (Str.replace_first regex tmpl string)))),
    datatype "(String, Regex) -> String",
    PURE));
	
  ("sntilde",	
   (`Server (p2 (fun s r ->
	let Regex.Replace(l, t) = Linksregex.Regex.ofLinks r in 
	let (regex, tmpl) = Regex.compile_ocaml l, t in
	let string = (match s with `NativeString ss -> ss | _ -> failwith "Internal error: expected NativeString") in
	(`NativeString (Utility.decode_escapes (Str.replace_first regex tmpl string))))),
    datatype "(NativeString, Regex) -> NativeString",
    PURE));
   
  (* NativeString utilities *)
  ("char_at",
   (`Server (p2 (fun ((`NativeString ss) : result) ((`Int ix):result) -> `Char (ss.[Num.int_of_num ix]))),
    (datatype ("(NativeString, Int) -> Char")),
    IMPURE));

  ("strlen",
   (`Server (p1 (fun s -> match s with
                     `NativeString ss -> `Int (Num.num_of_int (String.length ss))
	           |  _ -> failwith "Internal error: strlen got wrong arguments")),
    (datatype ("(NativeString) -> Int ")),
    PURE));

  ("to_native_string",
   (`Server (p1 (fun s -> let n = unbox_string s in (`NativeString n))),
    (datatype ("(String) -> NativeString")),
    PURE));
	
  ("from_native_string",
   (`Server (p1 
	       (fun s-> match s with  
	            (`NativeString ss) -> box_string ss
	          | _  -> failwith "Internal error: Bad coercion from native string")),
    (datatype ("(NativeString) -> String")),
    PURE));
	
  ("unsafePickleCont",
   (`Server (p1 (marshal_value ->- box_string)),
    datatype "((a) -> b) -> String",
    IMPURE));

  (* Serialize values to DB *)
  ("pickle_value", 
   (`Server (p1 (fun v -> (box_string (marshal_value v)))),
    datatype "(a) -> String",
    IMPURE));     

  ("unpickle_value",
   (`Server (p1 (fun v -> broken_unmarshal_value (unbox_string v))),
    datatype "(String) -> a",
  IMPURE));

  (* HACK *)
  ("unsafe_cast",
   (`Server (p1 (fun v -> v)),
    datatype "(a) -> b",
    PURE));
  
  (** A silly server-side function, just for testing server-side primitives 
      called from client. Remove this if there is a better one for testing. *)
  ("server_concat",
  (`Server (p2 (fun a b -> box_string (unbox_string a ^ unbox_string b))),
   datatype "(String, String) -> String",
   IMPURE));

  (** non-deterministic random number generator *)
  "random",
  (`PFun (fun _ -> (box_float (Random.float 1.0))),
   datatype "() -> Float",
   IMPURE);

    "dumpTypes",
  (`Server (p1 (fun code ->
                  try
                    let ts = DumpTypes.program (val_of (!prelude_env)) (unbox_string code) in
                      
                    let line ({Lexing.pos_lnum=l}, _, _) = l in
                    let start ({Lexing.pos_bol=b; Lexing.pos_cnum=c}, _, _) = c-b in
                    let finish (_, {Lexing.pos_bol=b; Lexing.pos_cnum=c}, _) = c-b in
                      
                    let box_int = num_of_int ->- box_int in

                    let resolve (name, t, pos) =
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
            datatype "(String) -> [|Success:[(name:String, t:String, pos:(line:Int, start:Int, finish:Int))] | Failure:String|]",
            IMPURE)
]

let impl : located_primitive -> primitive option = function
  | `Client -> None
  | `Server p
  | (#primitive as p) -> Some p

let value_env = 
  ref (List.fold_right
    (fun (name, (p,_,_)) env -> StringMap.add name (impl p) env)
    env
    StringMap.empty)

let primitive_location (name:string) = 
  match fst3 (List.assoc name env) with
  | `Client ->  `Client
  | `Server _ -> `Server
  | #primitive -> `Unknown

let rec function_arity =
  function
    | `Function(`Record (l, _), _, _) ->
        (Some (StringMap.size l))
    | `ForAll(qs, t) -> function_arity t
    | _ -> None

let primitive_arity (name : string) = 
  let _, t, _ = assoc name env in
    function_arity t

let primitive_stub (name : string): result =
  match StringMap.find name (!value_env) with
    | Some (#result as r) -> r
    | Some _ -> `PrimitiveFunction name
    | None  -> `ClientFunction name

let apply_pfun name args = 
  match StringMap.find name (!value_env) with
    | Some #result -> failwith("Attempt to apply primitive non-function (" ^
                                 name ^")")
    | Some (`PFun p) -> p args
    | None -> assert false

module Env = Env.String
        
let type_env : Types.environment =
  List.fold_right (fun (n, (_,t,_)) env -> Env.bind env (n, t)) env Env.empty
and alias_env : Types.alias_environment =
  List.fold_right
    (fun (name, datatype) env ->
       Env.bind env (name, datatype))
    [
      "DomNode" , `Primitive `Abstract;
      "Event"   , `Primitive `Abstract;
      "List"    , `ForAll ([`RigidTypeVar (Types.fresh_raw_variable ())], `Primitive `Abstract);
      "String"  , `Application ("List", [`Primitive `Char]);
      "Xml"     , `Application ("List", [`Primitive `XmlItem]);
      "Mailbox" , `ForAll ([`RigidTypeVar (Types.fresh_raw_variable ())], `Primitive `Abstract);
      "Regex"   , datatype Linksregex.Regex.datatype;
    ]
    Env.empty

let typing_env = (type_env, alias_env)

let primitive_names = StringSet.elements (Env.domain type_env)

let is_primitive name = List.mem_assoc name env

let is_pure_primitive name =
  if List.mem_assoc name env then
    match List.assoc name env with
      | (_, _, PURE) -> true
      | _ -> false
  else
    false

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
