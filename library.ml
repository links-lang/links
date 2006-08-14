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

(* http_headers: this is state for the webif interface. it's rubbish
   having it here. *)
let http_headers = ref []

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

(* let xml = `List (`Primitive `XMLitem) *)

let value_as_string db = function
  | `List ((`Char _)::_) as c  -> "\'" ^ db # escape_string (charlist_as_string c) ^ "\'"
  | `List ([])  -> "NULL"
  | a -> string_of_result a

let cond_from_field db (k, v) =
  match (k, v) with
      (_, `List([])) -> k ^ " is null" (* Is [] really always null? *)
    | _ -> "("^ k ^" = "^ value_as_string db v ^")"

let single_match db = 
  function
    | `Record fields -> "("^ (String.concat " AND " (map (cond_from_field db) fields)) ^")"
    | _ -> failwith "Internal error: forming query from non-row"

let row_columns = function
  | `Record fields -> String.concat ", " (map fst fields)
  | _ -> failwith "Internal error: forming query from non-row"
and row_values db = function
  | `Record fields -> String.concat ", " (map (value_as_string db -<- snd) fields)
  | _ -> failwith "Internal error: forming query from non-row"
and delete_condition db = function
  | `List(rows) -> "("^ (String.concat " OR " (map (single_match db) rows)) ^")"
  | _ -> failwith "Internal error: forming query from non-row"
and updates db : Result.result -> string = function
  | `Record fields -> 
      let field (k, v) = (k ^" = "^ value_as_string db v) in
        (String.concat ", " (map field fields))
  | _ -> failwith "Internal error: forming query from non-row"

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
  `PFun (fun a ->  (fn a))
and p2 fn = 
  `PFun (fun a -> `PFun (fun b ->  (fn a b)))
and p3 fn = 
  `PFun (fun a -> `PFun (fun b -> `PFun (fun c ->  (fn a b c))))

let client_only_1 fn = 
  p1 (fun _ -> failwith (Printf.sprintf "%s is not implemented on the server" fn))
let client_only_2 fn = 
  p2 (fun _ _ -> failwith (Printf.sprintf "%s is not implemented on the server" fn))

let datatype = Parse.parse_datatype

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
  ((p1 string_to_xml :> located_primitive),
   ([], Types.string_type --> xml));
  
  "intToXml",
  ((p1 (string_to_xml -<-
	  (conversion_op' ~unbox:unbox_int ~conv:string_of_num ~box:box_string))),
   ([], (`Primitive `Int) --> xml));
  
  "toplevel",   (* or `exit' *)
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
  (p1 (function
         | `List ((#result as x)::_) -> x
         | `List [] -> failwith "Head of empty list"
         | _ -> failwith "Internal error: head of non-list"),
   datatype "[a] -> a");

  "tl", 
  (p1 (function
         | `List (_::xs) -> `List xs
         | `List [] -> failwith "Tail of empty list"
         | _ -> failwith "Internal error: tail of non-list"),
   datatype "[a] -> [a]");
       
  "length", 
  (p1 (function
         | `List (elems) -> `Int (num_of_int (length elems))
         | _ -> failwith "Internal error: length of non-collection"),
   datatype "[a] -> Int");

  "take",
  (p2 (fun n l -> 
         match l with 
           | `List elems -> `List (take (int_of_num (unbox_int n)) elems)
           | _ -> failwith "Internal error: non-list passed to take"),
   datatype "Int -> [a] -> [a]");

  "drop",
  (p2 (fun n l ->
         match l with 
           | `List elems -> `List (drop (int_of_num (unbox_int n)) elems)
           | _ -> failwith "Internal error: non-list passed to drop"),
   datatype "Int -> [a] -> [a]");

  (** XML **)
  "childNodes",
  (p1 (function
         | `List [`XML (Node (_, children))] ->
             let children = filter (function (Node _) -> true | _ -> false) children in
               `List (map (fun x -> `XML x) children)
         | _ -> failwith "non-XML given to childNodes"),
   datatype "XML -> XML");

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
   datatype "(XML,String) -> [|Some:String | None:()|]");

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

(* [DEACTIVATED] *)
(*   "domOp", *)
(*   (p1 (fun message -> failwith("`domOp' is only available on the client."); *)
(*          `Record []), *)
(*    datatype "a -> ()"); *)

  "domInsertBefore",
  (`Client, datatype "(XML, DomRef) -> ()");

  "domAppendChild",
  (`Client, datatype "(XML, DomRef) -> ()");

  "domReplaceNode",
  (`Client, datatype "(XML, DomRef) -> ()"); 

  "domReplaceDocument",
  (`Client, datatype "XML -> ()"); 

  "domInsertBeforeRef",
  (`Client, datatype "(DomRef, DomRef) -> ()");

  "domAppendChildRef",
  (`Client, datatype "(DomRef, DomRef) -> ()");

  "domRemoveRef",
  (`Client, datatype "DomRef -> ()");

  "domReplaceChildren",
  (`Client, datatype "(XML, DomRef) -> ()");

  "domSwapNodeRefs",
  (`Client, datatype "(DomRef, DomRef) -> ()");

  "domGetDocumentRef",
  (`Client, datatype "() -> DomRef");

  "domGetRefById",
  (`Client, datatype "String -> DomRef");

  "domGetXml",
  (`Client, datatype "DomRef -> XML");

  "domIsNullRef",
  (`Client, datatype "DomRef -> Bool");

(* Section: Accessors for XML *)
  "getTagName",
  (`Client, datatype "XML -> String");

  "getAttributes",   (* FIXME: undocumented; nonsensical type? *)
  (`Client, datatype "XML -> a");

  "getTextContent",
  (`Client, datatype "XML -> String");

  "getAttribute",
  (`Client, datatype "(XML, String) -> String");

(* Section: Navigation for XML *)
  "getChildNodes",
  (`Client, datatype "XML -> [XML]");

(* Section: Accessors for DomRefs *)
  "domGetTagNameFromRef",
  (`Client, datatype "DomRef -> String");

  "domGetAttributeFromRef",
  (`Client, datatype "(DomRef, String) -> String");

  "domSetAttributeFromRef",
  (`Client, datatype "(DomRef, String, String) -> String");

  "domGetStyleAttrFromRef",
  (`Client, datatype "(DomRef, String) -> String");

  "domSetStyleAttrFromRef",
  (`Client, datatype "(DomRef, String, String) -> String");

(* Section:  Navigation for DomRefs *)
  "domGetParentFromRef",
  (`Client, datatype "DomRef -> DomRef");

  "domGetFirstChildFromRef",
  (`Client, datatype "DomRef -> DomRef");

  "domGetNextSiblingFromRef",
  (`Client, datatype "DomRef -> DomRef");

(* Section: DOM Event API *)
  "eventGetTarget",
  (`Client, datatype "Event -> DomRef");

  "eventGetTargetValue",
  (`Client, datatype "Event -> String");

  "eventGetTargetElement",
  (`Client, datatype "Event -> DomRef");

(* getPageX : Event -> Int *)
  "eventGetPageX",
  (`Client, datatype "Event -> Int");

(* getPageY : Event -> Int *)
  "eventGetPageY",
  (`Client, datatype "Event -> Int");

(* getRelatedTarget : Event -> DomRef *)
  "eventGetRelatedTarget",
  (`Client, datatype "Event -> DomRef");

(* getTime : Event -> Int *)
  "eventGetTime",
  (`Client, datatype "Event -> Int");

(* # stopEvent : ??? *)
(* # stopPropagation : ??? *)
(* # preventDefault : ??? *)
(* getCharCode : Event -> Char *)
  "eventGetCharCode",
  (`Client, datatype "Event -> Char");

(* Cookies *)
  "setCookie",
  (p2 (fun cookieName cookieVal ->
         let cookieName = charlist_as_string cookieName in
         let cookieVal = charlist_as_string cookieVal in
           http_headers := 
             ("Set-Cookie", cookieName ^ "=" ^ cookieVal) :: !http_headers;
           `Record []
      ),
   datatype "String -> String -> unit");

  "getCookie",
  (p1 (fun cookieName -> 
         try 
           let cookieName = charlist_as_string cookieName in
             let cookie_header = getenv "HTTP_COOKIE" in
               let cookies = Str.split (Str.regexp ",") cookie_header in
               let cookies = map (fun str -> 
                                    let [nm; vl] = Str.split (Str.regexp "=") str in 
                                      nm, vl) cookies in
               let the_cookie = snd (find (fun (nm, vl) -> nm = cookieName) 
                                       cookies) in
                 match string_as_charlist the_cookie with
                     `List _ as result -> result
                   | _ -> failwith "Internal Error library l469"
         with Not_found ->
           `List []
      ),
   datatype "String -> String");


(* [SL] Aarghhh... this is too dangerous *)
(*
  "getCommandOutput",
  (p1 ((unbox_string ->- Utility.process_output ->- box_string) :> result -> primitive),
   datatype "String -> String");
*)

  "sleep",
  (* FIXME: This isn't right : it freezes all threads *)
  (p1 (fun duration -> Unix.sleep (int_of_num (unbox_int duration));
         `Record []),
   datatype "Int -> ()");

  (** Database functions **)
  "asList",
  (p1 (fun th -> failwith "Unoptimized table access!!!"),
   datatype "TableHandle(r) -> [{r}]");

  "insertrow",
  (`Server 
     (p1 (function
            | `Record fields ->
                let table = assoc "1" fields
                and row = assoc "2" fields in begin
                    match table with 
                      | `Table (db, table_name, _) ->
                          let query_string =
                            "insert into " ^ table_name ^ "("^ row_columns row ^") values ("^ row_values db row ^")"
                          in
                            prerr_endline("RUNNING INSERT QUERY:\n" ^ query_string);
                            (Database.execute_command 
                               (`List unit_type)
                               query_string
                               db :> primitive)
                      | _ -> failwith "Internal error: insert row into non-database"
                  end
            | _ -> failwith "Internal error unboxing args (insertrow)")),
   datatype "(TableHandle(r), {r}) -> ()");

  "deleterows", 
  (`Server
     (p1 (function
            | `Record fields ->
                let table = assoc "1" fields
                and rows = assoc "2" fields in begin
                    match table with 
                      | `Table (db, table_name, _)  ->
                          let query_string =
                            "delete from " ^ table_name ^ " where " ^ delete_condition db rows
                          in
                          prerr_endline("RUNNING DELETE QUERY:\n" ^ query_string);
                          (Database.execute_command
                             (`List unit_type)
                             query_string
                             db :> primitive)
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
	   
(* [BROKEN] *)
(*
    "updaterows", 
  (p1 (function
         | `Record fields ->
             let table = assoc "1" fields
             and database = assoc "2" fields
             and rows = assoc "3" fields in begin
                 match database, rows with 
                   |  `Database (db, _), `List rows ->
                        List.iter (fun row -> 
                                     ignore (Database.execute_select
                                               (`List unit_type)
                                               ("update " ^ unbox_string table
                                                ^ " set " ^ updates db (links_snd row)
                                                ^ " where " ^ single_match db (links_fst row))
                                               db))
                          rows;
                        `Record []
                   | _ -> failwith "Internal error: bad value passed to `updaterows'"
               end
         | _ -> failwith "Internal error unboxing args (updaterows)"),
   let v', v = fresh_row () in
   let u', u = fresh_row () in
     [u'; v'],
   tuplify [Types.string_type; `Primitive `DB; `List (tuplify [`Record u; `Record v])] --> unit_type);
*)
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
    let qs, regex = Parse.parse_datatype Linksregex.Regex.datatype in
      qs, (`List (`Primitive `Char) --> (regex --> `Primitive `Bool))));
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
