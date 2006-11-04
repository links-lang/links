open Sys
open Num
open List

open Result
open Types
open Utility
open Debug

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
let database_driver = Settings.add_string("database_driver", "", true)
let database_args = Settings.add_string("database_args", "", true)

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
[5~  assumption:
    the only kind of lists that are allowed to be inserted into databases
    are strings
*)
let value_as_string db = function
  | `List ((`Char _)::_) as c  -> "\'" ^ db # escape_string (charlist_as_string c) ^ "\'"
  | `List ([])  -> "\'\'"
  | a -> string_of_result a

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

type primitive =  [
  Result.result
| `PFun of result -> primitive ]

type located_primitive = [ `Client | `Server of primitive | primitive ]

let int_op impl : located_primitive * Types.assumption = 
  (`PFun (fun x -> `PFun (fun y -> `Int (impl (unbox_int x) (unbox_int y))))),
  ([], `Primitive `Int --> (`Primitive `Int --> `Primitive `Int))

let float_op impl : located_primitive * Types.assumption = 
  `PFun (fun x -> `PFun (fun y -> (`Float (impl (unbox_float x) (unbox_float y))))),
  ([], `Primitive `Float --> (`Primitive `Float --> `Primitive `Float))
    
let conversion_op' ~unbox ~conv ~(box :'a->result) =
  let box = (box :> 'a -> primitive) in
    fun x -> (box (conv (unbox x)))

let conversion_op ~from ~unbox ~conv ~(box :'a->result) ~into : located_primitive * Types.assumption =
  (`PFun (conversion_op' ~unbox:unbox ~conv:conv ~box:box),
   ([], from --> into))

let string_to_xml = function 
  | `List _ as c -> `List [`XML (Text (charlist_as_string c))]
  | _ -> failwith "internal error: non-string value passed to xml conversion routine"

let char_test_op fn = 
  (`PFun (fun c -> (`Bool (fn (unbox_char c)))),
   ([], `Primitive `Char --> `Primitive `Bool))

let char_conversion fn = 
  (`PFun (fun c ->  (box_char (fn (unbox_char c)))),
   ([], `Primitive `Char --> `Primitive `Char))

let float_fn fn = 
  (`PFun (fun c ->  (box_float (fn (unbox_float c)))),
   ([], `Primitive `Float --> `Primitive `Float))

let p1 fn = 
  `PFun ((fun a ->  (fn a :> primitive)))
and p2 fn = 
  `PFun (fun a -> `PFun (fun b -> (fn a b :> primitive)))
and p3 fn = 
  `PFun (fun a -> `PFun (fun b -> `PFun (fun c -> (fn a b c :> primitive))))

let unpack_args = function
    (* Convert an argument record into an ordered list of arguments.
       Not currently terribly efficient (assoc, length, sprintf, etc.) *)
  | `Record args -> 
      List.fold_right (fun n outargs ->
                         List.assoc (Printf.sprintf "%d" (n+1)) args :: outargs)
        (Utility.fromTo 0 (List.length args))
        []                           
  | _ -> failwith "Internal error: arguments not passed as tuple"

let one_arg = function
  | [a] -> a
  | l -> failwith (Printf.sprintf "Wrong number of arguments: %d (expected one)" (List.length l));;
let two_args = function
  | [a;b] -> (a,b)
  | l -> failwith (Printf.sprintf "Wrong number of arguments: %d (expected two)" (List.length l));;

let client_only_1 fn = 
  p1 (fun _ -> failwith (Printf.sprintf "%s is not implemented on the server" fn))
let client_only_2 fn = 
  p2 (fun _ _ -> failwith (Printf.sprintf "%s is not implemented on the server" fn))

let datatype = Parse.parse_string Parse.datatype

let rec equal l r =
  match l, r with
    | `Bool l  , `Bool r   -> l = r
    | `Int l   , `Int r    -> eq_num l r
    | `Float l , `Float r  -> l = r
    | `Char l  , `Char r   -> l = r
    | `Function _, `Function _ -> Pickle_result.pickleS l = Pickle_result.pickleS r
    | `Record lfields, `Record rfields -> 
        let rec one_equal_all = (fun alls (ref_label, ref_result) ->
                                   match alls with
                                     | [] -> false
                                     | (label, result) :: _ when label = ref_label -> equal result ref_result
                                     | _ :: alls -> one_equal_all alls (ref_label, ref_result)) in
          for_all (one_equal_all rfields) lfields && for_all (one_equal_all lfields) rfields
    | `Variant (llabel, lvalue), `Variant (rlabel, rvalue) -> llabel = rlabel && equal lvalue rvalue
    | `List (l), `List (r) -> length l = length r &&
            fold_left2 (fun result x y -> result && equal x y) true l r
    | l, r ->  failwith ("Comparing "^ string_of_result l ^" with "^ string_of_result r ^" either doesn't make sense or isn't implemented")

let rec less l r =
  match l, r with
    | `Bool l, `Bool r   -> l < r
    | `Int l, `Int r     -> lt_num l r
    | `Float l, `Float r -> l < r
    | `Char l, `Char r -> l < r
    | `Function _ , `Function _                  -> Pickle_result.pickleS l < Pickle_result.pickleS r
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
  | (lcar::lcdr), (rcar::rcdr) -> less lcar rcar && less_lists (lcdr, rcdr)

let less_or_equal l r = less l r || equal l r

let env : (string * (located_primitive * Types.assumption)) list = [
  "+", int_op (+/);
  "-", int_op (-/);
  "*", int_op ( */);
  "/", int_op (fun x y -> integer_num (x // y));
  "^", int_op ( **/ );
  "mod", int_op mod_num;

  "+.", float_op (+.);
  "-.", float_op (-.);
  "*.", float_op ( *.);
  "/.", float_op (/.);
  "^.", float_op ( ** );

  (** Conversions (any missing?) **)
  "stringToInt",   conversion_op ~from:Types.string_type ~unbox:unbox_string ~conv:num_of_string ~box:box_int ~into:(`Primitive `Int);
  "intToFloat",    conversion_op ~from:(`Primitive `Int) ~unbox:unbox_int ~conv:float_of_num ~box:box_float ~into:(`Primitive `Float);
  "intToString",   conversion_op ~from:(`Primitive `Int) ~unbox:unbox_int ~conv:string_of_num ~box:box_string ~into:Types.string_type;
  "floatToString", conversion_op ~from:(`Primitive `Float) ~unbox:unbox_float ~conv:string_of_float ~box:box_string ~into:Types.string_type;

  "stringToXml",
  ((p1 string_to_xml),
   datatype "String -> Xml");
  
  "intToXml",
  ((p1 (string_to_xml -<-
	  (conversion_op' ~unbox:unbox_int ~conv:string_of_num ~box:box_string))),
   datatype "Int -> Xml");
  
  "exit",
  (`Continuation [],
   (datatype "a -> b")
  );

  "send",
  (p2 (fun pid msg -> 
         let pid = int_of_num (unbox_int pid) in
           (try 
              Queue.push msg (Hashtbl.find messages pid)
            with Not_found -> 
              (* Is this really an internal error? Maybe target has finished? *)
              failwith ("Internal error while sending message: no mailbox for "
                        ^ string_of_int pid));
           (try 
              Queue.push(Hashtbl.find blocked_processes pid) suspended_processes;
              Hashtbl.remove blocked_processes pid
            with Not_found -> ());
           `Record []),
   datatype "Mailbox (a) -> a -> ()");

  "self",
  (p1 (fun _ -> `Int (num_of_int !current_pid)),
   datatype "Mailbox (a) -> () -> Mailbox (a)");
  
  "recv",
  (* this function is not used, as its application is a special case
     in the interpreter. But we need it here, for now, because of its
     type.  Ultimately we should probably not special-case it, but
     rather provide a way to implement this primitive from here.
     (Ultimately, it should perhaps be a true primitive (an AST node),
     because it uses a different evaluation mechanism from functions.
     -- jdy) *)
    (p1 (fun _ -> assert false),
     datatype "Mailbox (a) -> () -> (a)");
 
  "spawn",
  (* This should also be a primitive, as described in the ICFP paper. *)
  (p2 (fun f p ->
         let new_pid = fresh_pid () in
           Hashtbl.add messages new_pid (Queue.create ());
           Queue.push ((FuncApply(f, []) :: [], p), new_pid) suspended_processes;
           (`Int (num_of_int new_pid))),
   (*
     a: spawn's mailbox type (ignored, since spawn doesn't recv)
     b: the mailbox type of the spawned process
     c: the parameter expected by the process function
     d: the return type of the spawned process function (ignored)
   *)
   datatype "Mailbox (a) -> (Mailbox (b) -> c -> d) -> Mailbox (a) -> c -> Mailbox (b)");

  "_MAILBOX_",
  (`Int (num_of_int 0), 
   let _, u = fresh_type () in
     (* Deliberately non-quantified type.  Mailboxes are
        non-polymorphic, so this is a so-called "weak type
        variable". *)
     ([], u));

  (** Lists and collections **)
  "hd",
  (p1 (unbox_list ->- List.hd),
   datatype "[a] -> a");

  "tl", 
  (p1 (unbox_list ->- List.tl ->- box_list),
   datatype "[a] -> [a]");
  
  "length", 
  (p1 (unbox_list ->- List.length ->- num_of_int ->- box_int),
   datatype "[a] -> Int");

  "take",
  (p1 (fun args ->
         let n,l = two_args (unpack_args args) in
           box_list (Utility.take (int_of_num (unbox_int n)) (unbox_list l))),
   datatype "(Int, [a]) -> [a]");

  "drop",
  (p1 (fun args ->
         let n, l = two_args (unpack_args args) in
           box_list (Utility.drop (int_of_num (unbox_int n)) (unbox_list l))),
   datatype "(Int, [a]) -> [a]");

  "max",
  (p1 (let max2 x y = if less x y then y else x in
         function
           | `List [] -> `Variant ("None", `Record [])
           | `List (x::xs) -> `Variant ("Some", List.fold_left max2 x xs)
           | _ -> failwith "Internal error: non-list passed to max"),
   datatype "[a] -> [|Some:a | None:()|]");

  "min",
  (p1 (let min2 x y = if less x y then x else y in
         function
           | `List [] -> `Variant ("None", `Record [])
           | `List (x::xs) -> `Variant ("Some", List.fold_left min2 x xs)
           | _ -> failwith "Internal error: non-list passed to min"),
   datatype "[a] -> [|Some:a | None:()|]");

  (** XML **)
  "childNodes",
  (p1 (function
         | `List [`XML (Node (_, children))] ->
             let children = filter (function (Node _) -> true | _ -> false) children in
               `List (map (fun x -> `XML x) children)
         | _ -> failwith "non-XML given to childNodes"),
   datatype "Xml -> Xml");

  "objectType",
  (`Client, datatype "a -> String");

  "attribute",
  (p1 (let none = `Variant ("None", `Record []) in
         function
           | `Record elems -> 
               (let elem = assoc "1" elems
                and attr = charlist_as_string (assoc "2" elems) in
                  match elem with
                    | `List ((`XML (Node (_, children)))::_) -> 
                        let attr_match = (function
                                            | Attr (k, _) when k = attr -> true
                                            | _ -> false) in
                          (try match find attr_match children with
                             | Attr (_, v) -> `Variant ("Some", string_as_charlist v)
                             | _ -> failwith "Internal error in `attribute'"
                           with Not_found -> none)
                    | _ -> none)
           | _ -> failwith "Internal error: bad arguments to attribute"),
   datatype "(Xml,String) -> [|Some:String | None:()|]");

  "alertDialog",
  (client_only_1 "alertDialog",
   datatype "String -> ()");

  "debug", 
  (p1 (fun message -> prerr_endline (unbox_string message); flush stderr; `Record []),
   datatype "String -> ()");

  "debugObj",
  (client_only_1 "debugObj", datatype "a -> ()");
  
  "dump",
  (client_only_1 "dump", datatype "a -> ()");
  
  "textContent",
  (client_only_1 "textContent", datatype "a -> String");
  "print",
  (p1 (fun msg -> print_endline (unbox_string msg); flush stdout; `Record []),
   datatype "String -> ()");

  "javascript",
  (`Bool false, datatype "Bool");

  "not", 
  (p1 (unbox_bool ->- not ->- box_bool),
   datatype "Bool -> Bool");
  
  "negate", 
  (p1 (unbox_int ->- minus_num ->- box_int), datatype "Int -> Int");

  "negatef", 
  (p1 (fun f -> box_float (-. (unbox_float f))), datatype "Float -> Float");

  "error",
  (p1 (unbox_string ->- failwith), datatype "String -> a");
  

  (* HACK *)
  (*   [DEACTIVATED] *)
  (*   "callForeign", *)
  (*    (client_only_1 "callForeign", datatype "(a -> b) -> a -> b"); *)

  (* DOM API *)

  "isElementNode",
  (`Client, datatype "(DomNode) -> Bool");


  (* [DEACTIVATED] *)
  (*   "domOp", *)
  (*   (p1 (fun message -> failwith("`domOp' is only available on the client."); *)
  (*          `Record []), *)
  (*    datatype "a -> ()"); *)

  "insertBefore",
  (`Client, datatype "(Xml, DomNode) -> ()");

  "appendChildren",
  (`Client, datatype "(Xml, DomNode) -> ()");

  "replaceNode",
  (`Client, datatype "(Xml, DomNode) -> ()"); 

  "replaceDocument",
  (`Client, datatype "Xml -> ()"); 

  "domInsertBeforeRef",
  (`Client, datatype "(DomNode, DomNode) -> ()");

  "domAppendChildRef",
  (`Client, datatype "(DomNode, DomNode) -> ()");

  "removeNode",
  (`Client, datatype "DomNode -> ()");

  "replaceChildren",
  (`Client, datatype "(Xml, DomNode) -> ()");

  "swapNodes",
  (`Client, datatype "(DomNode, DomNode) -> ()");

  "getDocumentNode",
  (`Client, datatype "() -> DomNode");

  "getNodeById",
  (`Client, datatype "String -> DomNode");

  "getValue",
  (`Client, datatype "DomNode -> Xml");

  "isNull",
  (`Client, datatype "DomNode -> Bool");

  (* Section: Accessors for XML *)
  "getTagName",
  (`Client, datatype "Xml -> String");

  "getTextContent",
  (`Client, datatype "Xml -> String");

  "getAttributes",
  (`Client, datatype "Xml -> [(String,String)]");

  "hasAttribute",
  (`Client, datatype "(Xml, String) -> Bool");

  "getAttribute",
  (`Client, datatype "(Xml, String) -> String");

  (* Section: Navigation for XML *)
  "getChildNodes",
  (`Client, datatype "Xml -> Xml");

  (* Section: Accessors for DomNodes *)
  "domGetTagNameFromRef",
  (`Client, datatype "DomNode -> String");

  "domGetAttributeFromRef",
  (`Client, datatype "(DomNode, String) -> String");

  "domSetAttributeFromRef",
  (`Client, datatype "(DomNode, String, String) -> String");

  "domGetStyleAttrFromRef",
  (`Client, datatype "(DomNode, String) -> String");

  "domSetStyleAttrFromRef",
  (`Client, datatype "(DomNode, String, String) -> String");

  (* Section:  Navigation for DomNodes *)
  "parentNode",
  (`Client, datatype "DomNode -> DomNode");

  "firstChild",
  (`Client, datatype "DomNode -> DomNode");

  "nextSibling",
  (`Client, datatype "DomNode -> DomNode");

  (* Section: DOM Event API *)
  "getTarget",
  (`Client, datatype "Event -> DomNode");

  "getTargetValue",
  (`Client, datatype "Event -> String");

  "getTargetElement",
  (`Client, datatype "Event -> DomNode");

  (* getPageX : Event -> Int *)
  "getPageX",
  (`Client, datatype "Event -> Int");

  (* getPageY : Event -> Int *)
  "getPageY",
  (`Client, datatype "Event -> Int");

  (* getFromElement : Event -> DomNode *)
  "getFromElement",
  (`Client, datatype "Event -> DomNode");

  (* getToElement : Event -> DomNode *)
  "getToElement",
  (`Client, datatype "Event -> DomNode");

  (* getTime : Event -> Int *)
  "getTime",
  (`Client, datatype "Event -> Int");

  (* getCharCode : Event -> Char *)
  "getCharCode",
  (`Client, datatype "Event -> Char");

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
      ),
   datatype "String -> String -> unit");

  (* FIXME: this one should probably the only setCookie. *)
  "setCookieUncurried",
  (p1 (fun pair ->
         (match pair with
              `Record elems ->
                let cookieName = charlist_as_string (assoc "1" elems) in
                let cookieVal = charlist_as_string (assoc "2" elems) in
                  http_response_headers := 
                    ("Set-Cookie", cookieName ^ "=" ^ cookieVal) :: !http_response_headers;
                  `Record []
            | _ -> failwith "Impossible error.")
      ),
   datatype "(String, String) -> unit");

  "getCookie",
  (p1 (fun cookieName -> 
         let cookieName = charlist_as_string cookieName in
           match getenv "HTTP_COOKIE" with
             | Some cookie_header ->
                 (let cookies = Str.split (Str.regexp ",") cookie_header in
                  let cookies = map (fun str -> 
                                       let [nm; vl] = Str.split (Str.regexp "=") str in 
                                         nm, vl) cookies in
                    box_string (snd (find (fun (nm, _) -> nm = cookieName) 
                                       cookies)))
             | None -> `List []),
   datatype "String -> String");


  (* getCommandOutput disabled for now; possible security risk. *)
  (*
    "getCommandOutput",
    (p1 ((unbox_string ->- Utility.process_output ->- box_string) :> result -> primitive),
    datatype "String -> String");
  *)

  "redirect",
  (p1 (fun url ->
         let url = charlist_as_string url in
           (* This is all quite hackish, just testing an idea. --ez *)
           http_response_headers := ("Location", url) :: !http_response_headers;
           http_response_code := 302;
           `Record []
      ), datatype "String -> ()");   (* Should this function really return? 
                                        I think not --ez*)

  (** reifyK: I choose an obscure name, for an obscure function, until
      a better one can be though up. It just turns a continuation into its
      string representation *)
  "reifyK",
  (p1 (function
           `Continuation k -> 
             (match string_as_charlist(marshal_continuation k) with
                  `List _ as result -> result
                | _ -> failwith "")
         | _ -> failwith "argument to reifyK was not a continuation"
      ),
   datatype "(a -> b) -> String"); (* arg type should actually be limited
                                      to continuations, but we don't have
                                      any way of specifying that in the 
                                      type system. *)

  "sleep",
  (* FIXME: This isn't right : it freezes all threads *)
  (p1 (fun duration -> Unix.sleep (int_of_num (unbox_int duration));
         `Record []),
   datatype "Int -> ()");

  (** Database functions **)
  "asList",
  (p1 (fun _ -> failwith "Unoptimized table access!!!"),
   datatype "TableHandle(r) -> [{r}]");

  "insertrows",
  (`Server 
     (p1 (function
            | `Record fields ->
                let table = assoc "1" fields
                and rows = assoc "2" fields in
                  begin
                    match table, rows with
                      | `Table _, `List [] -> `Record []
                      | `Table (db, table_name, _), _ ->
                          let field_names = row_columns rows
                          and vss = row_values db rows
                          in
                            prerr_endline("RUNNING INSERT QUERY:\n" ^ (db#make_insert_query(table_name, field_names, vss)));
                            (Database.execute_insert (table_name, field_names, vss) db)
                      | _ -> failwith "Internal error: insert row into non-database"
                  end
            | _ -> failwith "Internal error unboxing args (insertrow)")),
   datatype "(TableHandle(r), [{r}]) -> ()");

  "updaterows", 
  (`Server
     (p1 (function
            | `Record fields ->
                let table = assoc "1" fields
                and rows = assoc "2" fields in begin
                    match table, rows with
                      | _, `List [] -> `Record []
                      | `Table (db, table_name, _), `List rows ->
                          List.iter (fun row ->
                                       let query_string =
                                         "update " ^ table_name
                                         ^ " set " ^ updates db (links_snd row)
                                         ^ " where " ^ single_match db (links_fst row)
                                       in
                                         prerr_endline("RUNNING UPDATE QUERY:\n" ^ query_string);
                                         ignore (Database.execute_command query_string db))
                            rows;
                          `Record []
                      | _ -> failwith "Internal error: bad value passed to `updaterows'"
                  end
            | _ -> failwith "Internal error unboxing args (updaterows)")),
   datatype "(TableHandle(r), [({r},{r})]) -> ()");

  "deleterows", 
  (`Server
     (p1 (function
            | `Record fields ->
                let table = assoc "1" fields
                and rows = assoc "2" fields in
                  begin
                    match table, rows with
                      | `Table _, `List [] ->
                          `Record []
                      | `Table (db, table_name, _), _  ->
                          let condition = delete_condition db rows in
                          let query_string = "delete from " ^ table_name ^ " where " ^ condition
                          in
                            prerr_endline("RUNNING DELETE QUERY:\n" ^ query_string);
                            (Database.execute_command query_string db)
                      | _ -> failwith "Internal error: delete row from non-database"
                  end
            | _ -> failwith "Internal error unboxing args (deleterows)")),
   datatype "(TableHandle(r), [{r}]) -> ()");

  "getDatabaseConfig",
  (`Server
     (p1 (fun _ ->
	    let driver = Settings.get_value database_driver
	    and args = Settings.get_value database_args in
	      if driver = "" then
	        failwith "Internal error: default database driver not defined"
	      else
	        `Record(["driver", string_as_charlist driver;
		         "args", string_as_charlist args])
	 )),
   datatype "() -> (driver:String, args:String)");
  
  (** some char functions **)
  "isAlpha",  char_test_op (function 'a'..'z' | 'A'..'Z' -> true | _ -> false);
  "isAlnum",  char_test_op (function 'a'..'z' | 'A'..'Z' | '0'..'9' -> true | _ -> false);
  "isLower",  char_test_op (function 'a'..'z' -> true | _ -> false);
  "isUpper",  char_test_op (function 'A'..'Z' -> true | _ -> false);
  "isDigit",  char_test_op (function '0'..'9' -> true | _ -> false);
  "isXDigit", char_test_op (function '0'..'9'|'a'..'f'|'A'..'F' -> true | _ -> false);
  "isBlank",  char_test_op (function ' '|'\t' -> true | _ -> false);
  (* isCntrl, isGraph, isPrint, isPunct, isSpace *)
  
  "toUpper", char_conversion Char.uppercase;
  "toLower", char_conversion Char.lowercase;

  "ord",
  (p1 (fun c -> box_int (num_of_int (Char.code (unbox_char c)))), 
   datatype "Char -> Int");

  "chr",
  (p1 (fun n -> (box_char (Char.chr (int_of_num (unbox_int n))))), 
   datatype "Int -> Char");

  (* some trig functions *)
  "floor",   float_fn floor;
  "ceiling", float_fn ceil;
  "cos",     float_fn cos;
  "sin",     float_fn sin;
  "tan",     float_fn tan;
  "log",     float_fn log;
  "sqrt",    float_fn sqrt;

  (* regular expression matching *)
  ("~",
   (p2 (fun s r -> 
          let regex = Regex.compile_ocaml (Linksregex.Regex.ofLinks r)
          and string = unbox_string s in
            box_bool (Str.string_match regex string 0)),
    let qs, regex = datatype Linksregex.Regex.datatype in
      qs, (string_type --> (regex --> `Primitive `Bool))));

  ("environment",
   (p1 (fun _ -> 
          let makestrpair (x1, x2) = `Record [("1", box_string x1); ("2", box_string x2)] in
          let is_internal s = Str.string_match (Str.regexp "^_") s 0 in
            `List (List.map makestrpair (List.filter (not -<- is_internal -<- fst) !cgi_parameters))),
    datatype "() -> [(String,String)]"));
]

type continuationized_val = [
  result
| `PFun of (continuation -> result -> result) * continuation * result -> continuationized_val
]

(* Transform a primitive function so that it accepts a continuation
   and an 'apply-continuation' primitive as well as an argument *)
let rec continuationize : primitive -> continuationized_val = function
    | `PFun f -> `PFun (fun ((applycont : continuation -> result -> result), (cont : continuation), (arg : result)) ->
                          match f arg with 
                            | #result as r -> (applycont cont r :> continuationized_val)
                            | prim         -> continuationize prim)
    | (#result as a) -> a
        
let continuationize : located_primitive -> continuationized_val option = function
  | `Client -> None
  | `Server p
  | (#primitive as p) -> Some (continuationize p)

type primitive_environment = (string*continuationized_val) list

let continuationize_env = Utility.concat_map
  (fun (n, (v,_)) ->
    match continuationize v with
      | None -> []
      | Some v -> [n,v])

let value_env = ref (continuationize_env env)
and type_env : Types.environment = 
  Inference.retype_primitives (List.map (fun (n, (_,t)) -> (n,t)) env)

let impl : located_primitive -> primitive = function
  | `Client -> failwith "client function requested"
  | `Server p
  | (#primitive as p) -> p

(* [DISGUSTING HACK] *)
(* no mailbox type threaded through *)
let pure_type_env = Inference.unretype_primitives type_env

let apply_pfun (apply_cont :continuation -> result -> result) cont (name : string) (args : result list) = 
  let rec aux args' = function
    | #result as r -> 
        assert (args' = []);
        apply_cont cont r
    | `PFun f      -> 
        match args' with 
          | []      -> apply_cont cont (`PFunction (name, args))
          | r::rest -> aux rest (f r) in
    if mem_assoc name env then
      aux args (impl (fst (assoc name env)))
    else 
      let result_of_cval : continuationized_val -> result = function
        | #result as r -> r
        | _ -> failwith ("Error calling client function " ^ name ^ " (is it curried?)")
      in match assoc name !value_env, args with
        | `PFun f, [a]  -> result_of_cval (f (apply_cont, cont, a))
        | `PFun _, _  -> failwith (Printf.sprintf "expected one arg, but found %d during server->client call of %s"
                                     (length args) name)
        | #result, _ -> failwith ("value found, expecting function during server->client call of "
                                   ^ name)
let primitive_stub (name : string) : result =
  match assoc name !value_env with 
    | `PFun _      -> `PFunction (name, [])
    | #result as r -> r

let primitive_location p : Syntax.location = 
  match fst (assoc p env) with
  | `Client -> `Client
  | `Server _ -> `Server
  | #primitive -> `Unknown (* not the best nsme: this means "available on both client and server" *)
