open Sys
open Num
open List

open Kind
open Result
open Utility

(* Data structures/utilities for proc mgmt *)

type pid = int

type proc_state = Result.continuation * Result.result 

let suspended_processes = (Queue.create () : (proc_state * pid) Queue.t)
and blocked_processes = (Hashtbl.create 10000 : (pid, (proc_state * pid)) Hashtbl.t)
and messages = (Hashtbl.create 10000  : (int, Result.result Queue.t) Hashtbl.t)
and current_pid = (ref 0 :  pid ref)

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

(* let xml = `Collection (`List, `Primitive `XMLitem) *)

type primop = (Result.result -> Result.result)

let continuationize_fn : 
    (Result.result -> Result.result)
->  ((Result.continuation -> Result.result -> Result.result) * Result.continuation * Result.result)
->  Result.result = 
  fun prim ->
    fun (applycont, cont, arg) ->
      applycont cont (prim arg)

let continuationize_primfn = function
  | `Primitive (`PFunction (n, Some s, a)) -> 
      `Primitive (`PFunction (n, Some (continuationize_fn s), a))
  | otherwise -> failwith "boom"


(* FIXME: need a better way to get the parsed query string in here *)
let query_environment : (string * string) list ref = ref []
let set_query_environment env =
  query_environment := env
    ; debug("query environment set to " ^ (string_of_alist !query_environment))

(* template for builtin binary operators *)
let binary_op box unbox op name : primop = 
  fun left -> 
    let l = unbox left  in
      primfunappl name
	(* (name ^ "(" ^ string_of_result left ^ ")") *)
	(continuationize_fn (fun right -> let r = unbox right in box (op l r)))
	[left]
       
(* instantiations of binary_op for integers and floats *)
let intop name op =
  (name, ((primfun name (binary_op box_int unbox_int op name)), 
          ([],  (`Primitive `Int --> (`Primitive `Int --> `Primitive `Int)))))
    
and floatop name op  = 
  (name, ((primfun name (binary_op box_float unbox_float op name)),
          ([], (`Primitive `Float -->
		  (`Primitive `Float --> `Primitive `Float)))))


    
(* template for builtin conversion functions *)
let conversion_op box unbox op : primop = 
  fun from -> 
    try box (op (unbox from))
    with _ -> failwith ("Failure converting " ^ string_of_result from)

let conversion name op result from = (name, (primfun name op, ([], from --> result)))

let char_test_op name func =
  (name,
   (primfun name
      (compose box_bool (compose func unbox_char)),
    ([], `Primitive `Char --> `Primitive `Bool)))
and char_conversion name func = 
  (name,
   (primfun name
      (compose box_char (compose func unbox_char)),
    ([], `Primitive `Char --> `Primitive `Char)))
and float_fun name func = 
  (name,
   (primfun name
      (compose box_float (compose func unbox_float)),
    ([], `Primitive `Float --> `Primitive `Float)))

let format_attrs : result -> string = function
  | `Collection (`List, attrs)  ->
      let format_attr pair = 
        let (k, v) = pair_as_ocaml_pair pair in 
        let k = charlist_as_string k
        and v = charlist_as_string v
        in " " ^ k ^ "=\"" ^ v ^ "\""
      in String.concat "" (map format_attr attrs)
  | _ -> failwith "Internal error: format attributes applied to non-list"

(*let format_contents (`Collection (_, contents)) : (string) =
  String.concat "" (map charlist_as_string contents)
*)

let value_as_string = function
  | `Collection (`List, `Primitive(`Char _)::elems) as c  -> "\'" ^ Postgresql.escape_string (charlist_as_string c) ^ "\'"
  | `Collection (`List, [])  -> "NULL"
  | a -> string_of_result a

let cond_from_field (k, v) =
  match (k, v) with
      (_, `Collection(_, [])) -> k ^ " is null" (* Is [] really always null? *)
    | _ -> "("^ k ^" = "^ value_as_string v ^")"

let single_match = 
  function
    | `Record fields -> "("^ (String.concat " AND " (map cond_from_field fields)) ^")"
    | _ -> failwith "Internal error: forming query from non-row"

let row_columns = function
  | `Record fields -> String.concat ", " (map fst fields)
  | _ -> failwith "Internal error: forming query from non-row"
and row_values = function
  | `Record fields -> String.concat ", " (map (compose value_as_string snd) fields)
  | _ -> failwith "Internal error: forming query from non-row"
and delete_condition = function
  | `Collection(`List, rows) -> "("^ (String.concat " OR " (map single_match rows)) ^")"
  | _ -> failwith "Internal error: forming query from non-row"
and updates : Result.result -> string = function
  | `Record fields -> 
      let field (k, v) = (k ^" = "^ value_as_string v) in
        (String.concat ", " (map field fields))
  | _ -> failwith "Internal error: forming query from non-row"



open Netencoding

(* value and type environments *)
(* value_env = [string * 'conn result]
   type_env = [string * ((int list) * kind)]
*)
let env : (string * (result * Kind.assumption)) list = map
  (fun (name, (fn, kind)) -> (name, (continuationize_primfn fn, kind))) [
(* arithmetic operators *)
  intop "+" (+/);
  intop "-" (-/);
  intop "*" ( */);
  intop "/" (fun x y -> integer_num (x // y));
  intop "^" ( **/ );
  intop "mod" (mod_num);
  floatop "+." (+.);
  floatop "-." (-.);
  floatop "*." ( *.);
  floatop "/." (/.);
  floatop "^^" ( ** );

  conversion "string_of_float" (conversion_op box_string unbox_float string_of_float) 
    Kind.string (`Primitive `Float);

  conversion "float_of_int" (conversion_op box_float unbox_int float_of_num) 
    (`Primitive `Float) (`Primitive `Int);

  conversion "string_of_int" (conversion_op box_string unbox_int string_of_num) 
    Kind.string (`Primitive `Int);

  conversion "int_of_string" (conversion_op box_int unbox_string num_of_string) 
    (`Primitive `Int)  Kind.string;

  ("recv",
   (primfun "recv"
      (fun _ -> 
         (* this function is not used, as its application is a special
            case in the interpreter. But we need it here, for now,
            because of its type.  Ultimately we should probably not
            special-case it, but rather provide a way to implement
            this primitive from here. *)
         assert(false)),
    let (`RowVar r) = fresh_row_variable () in
      ([`RowVar r], make_unit () --> `Variant (TypeOps.make_empty_open_row_with_var r))));

  ("send",
   (primfun "send"
      (function
	 | `Primitive (`Int dest_pid)  -> 
	     continuationize_primfn (
	       primfun "send..."
		 (fun msg -> 
                    let dest_pid = int_of_num dest_pid in
                      (try
                         Queue.push msg (Hashtbl.find messages dest_pid);
                       with Not_found -> failwith ("Internal error while sending message: no mailbox for " ^ string_of_int dest_pid));
                      (try
                         Queue.push (Hashtbl.find blocked_processes dest_pid) suspended_processes;
                         Hashtbl.remove blocked_processes dest_pid
                       with Not_found -> ());
		      `Record []))
	 | _ -> failwith "Internal error (argument to send)"),
    let (`RowVar r) = fresh_row_variable () in
      ([`RowVar r], `Primitive `Int --> (`Variant (TypeOps.make_empty_open_row_with_var r) --> make_unit ()))));
  
  ("spawn",
   (primfun "spawn"
      (fun f ->
         continuationize_primfn (
           primfun
             "spawn..."
             (fun p ->
                (* Push the new process onto the queue. *)
		let new_pid = fresh_pid () in
		  Hashtbl.add messages new_pid (Queue.create ());
                  Queue.push ((FuncApply(f, []) :: [], p), new_pid) suspended_processes;
                  `Primitive (`Int (num_of_int new_pid))))),
    let a = fresh_type_variable () in
    let b = fresh_type_variable () in
      ([a], (a --> b) --> (a --> `Primitive `Int))));

  ("self",
   (primfun "self"
      (fun r -> (`Primitive (`Int (num_of_int !current_pid)))),
    ([], (make_unit () --> `Primitive `Int))));

  ("hd", 
   (primfun "hd" 
      (function
         | `Collection (_, elems) -> 
             (match elems with 
                | [] -> failwith "Head of empty list"
                | x :: xs -> x)
         | _ -> failwith "Internal error: head of non-list"),
    let v = fresh_type_variable () in 
      ([v],
       (`Collection (`List, v) --> v))
   ));

  ("length", 
   (primfun "length" 
      (function
         | `Collection (_, elems) -> `Primitive (`Int (num_of_int (length elems)))
         | _ -> failwith "Internal error: length of non-collection"),
    let v = fresh_type_variable () in
    let c = fresh_collection_variable () in
      ([c; v],
       (`Collection (c, v) --> `Primitive `Int))));
  
  ("take",
   (primfun "take"
      (function 
	 | `Primitive (`Int n) -> 
             continuationize_primfn (
               primfun
		 "take..."
		 (function
		    | `Collection (`List, elems) -> `Collection (`List, take (int_of_num n) elems)
		    | _ -> failwith "Internal error: non-list passed to take"))
	 | _ -> failwith "Internal error: non-integer passed to take"),
    let a = fresh_type_variable () in
      ([a], (`Primitive `Int --> (`Collection (`List, a) -->  `Collection (`List, a))))));


  ("childNodes",
   (primfun "childNodes"
      ((function
         | `Collection(_, [elem]) ->
             (match elem with 
                  `Primitive(`XML(Node(tag, children))) -> 
                    let children = filter (function (Node x) -> true | _ -> false) children in
                      (`Collection(`List, map (fun x -> `Primitive(`XML x)) children) : Result.result)
                | _ -> failwith ("non-XML given to childNodes")
             )
         | _ -> failwith ("non-XML given to childNodes")
       ) : Result.result -> Result.result
      ),
   ([],
       xml --> xml)
   )
  );

  ("drop",
   (primfun "drop"
      (function 
	 | `Primitive (`Int n) -> 
             continuationize_primfn (
               primfun
		 "drop..."
		 (function
		    | `Collection (`List, elems) -> `Collection (`List, drop (int_of_num n) elems)
		    | _ -> failwith "Internal error: non-list passed to drop"))
	 | _ -> failwith "Internal error: non-integer passed to drop"),
    let a = fresh_type_variable () in
      ([a], (`Primitive `Int --> (`Collection (`List, a) -->  `Collection (`List, a))))));

  ("tl", 
   (primfun "tl"
      (function
         | `Collection (_, elems) -> 
         (match elems with 
           | [] -> failwith "Tail of empty list"
           | x :: xs -> `Collection (`List, xs))
         | _ -> failwith "Internal error: tail of non-list"),
    let v = fresh_type_variable () in 
      ([v],
       (`Collection (`List, v) --> `Collection (`List, v)))));

  ("childNodes",
   (primfun "childNodes"
      ((function
         | `Collection(_, [elem]) ->
             (match elem with 
                  `Primitive(`XML(Node(tag, children))) -> 
                    let children = filter (function (Node x) -> true | _ -> false) children in
                      (`Collection(`List, map (fun x -> `Primitive(`XML x)) children) : Result.result)
                | _ -> failwith ("non-XML given to childNodes")
             )
         | _ -> failwith ("non-XML given to childNodes")
       ) : Result.result -> Result.result
      ),
   ([],
       xml --> xml)
   )
  );

  ("objectType",
   (primfun "objectType"
      (fun obj ->
         failwith("objectType not implemented for server-side code.")),
    let u = fresh_type_variable() in
    ([u], u --> string)
   ));

  ("attribute",
   (primfun "attribute"
      (function
         | `Record (elems) -> (let elem = List.assoc "1" elems
                               and attr = charlist_as_string (List.assoc "2" elems)
                               and none = `Variant ("None", `Record []) in
                                 (match elem with 
                                    | `Collection (_, `Primitive (`XML (Node (tag, children)))::_) -> 
                                        (try
                                           (match (List.find (function
                                                                | Attr (k, v) when k = attr -> true
                                                                | _ -> false) children) with
                                              | Attr (_, v) -> `Variant ("Some", string_as_charlist v)
                                              | _ -> failwith "boom")
                                         with Not_found -> none)
                                    | _ -> none))
         | _ -> failwith "Internal error: bad arguments to attribute"),
    let pair = `Record (TypeOps.set_field ("1", `Present (`Primitive `XMLitem))
	                  (TypeOps.set_field ("2", `Present string)
	                     (TypeOps.make_empty_closed_row ()))) in
      ([],
       pair --> 
         `Variant (TypeOps.set_field ("Some", `Present string)
                     (TypeOps.set_field ("None", `Present (`Record (TypeOps.make_empty_closed_row ())))
                        (TypeOps.make_empty_closed_row ()))))));

  ("elementById",
   (primfun "elementById"
      (fun _ -> failwith "elementById not implemented in the server"),
    let pair = `Record (TypeOps.set_field ("1", `Present xml)
	                  (TypeOps.set_field ("2", `Present string)
	                     (TypeOps.make_empty_closed_row ()))) in
      ([],
       string --> `Variant (TypeOps.set_field ("Some", `Present xml)
                              (TypeOps.set_field ("None", `Present ((`Record (TypeOps.make_empty_closed_row ())):kind))
                                 (TypeOps.make_empty_closed_row ()))))));

  ("string_of_cont",
   (primfun "string_of_cont"
      (fun cont ->
	 match cont with
	     `Continuation cont -> 
	       box_string(Utility.base64encode(serialise_continuation cont))
	   | _ -> failwith "string_of_cont applied to non-continuation" ),(*TYPEME!*)
    let v = fresh_type_variable () and
        u = fresh_type_variable () 
    in 
      ([v; u],
       (v --> u) --> string)));
  
  ("enxml",
   (primfun "enxml"
      (function 
         | `Collection _ as c -> `Collection (`List, [`Primitive (`XML (Text (charlist_as_string c)))])
         | _ -> failwith ("internal error: non-string value passed to xml conversion routine")),
    ([], 
     string --> xml))); 

  ("debug", (* destructive *)
   (primfun "debug"
      (fun message -> prerr_endline (unbox_string message); flush stderr; `Record []),
    ([], string --> make_unit ())));
   
  ("debugObj", (* destructive *)
   (primfun "debugObj"
      (fun message -> failwith("no debugObj on server")),
    let u = fresh_type_variable() in 
    ([u], u --> make_unit ())));

  ("dump", (* destructive *)
   (primfun "dump"
      (fun message -> failwith("no dump on server")),
    let u = fresh_type_variable() in 
    ([u], u --> make_unit ())));

  ("textContent",
   (primfun "textContent"
      (fun _ -> failwith("textContent is not implemented on the server side")),
    let u = fresh_type_variable() in 
      ([u], u --> string)));
   
  ("print", (* destructive *)
   (primfun "print"
      (fun message -> print_endline (unbox_string message); flush stdout; `Record []),
    ([], string --> make_unit ())));
   
  ("insertrow", (* destructive *)
   (primfun "insertrow"
      (fun (table : result) -> 
         let table = charlist_as_string table in
           continuationize_primfn (
             primfun ("insert into (" ^ table ^ ", ...) values ...")
             (function
                | `Database (db, _) as database -> 
                    continuationize_primfn (
                    primfun ("insert into (" ^ table ^ ", "^ string_of_result database ^ ") values ...")
                      (fun (row : result) ->
                         Database.execute_select
                           (`Collection (`List, make_unit ()))
                           (prerr_endline("*RUNNING SQL: " ^ "insert into " ^ table ^ "("^ row_columns row ^") values ("^ row_values row ^")");
                            ("insert into " ^ table ^ "("^ row_columns row ^") values ("^ row_values row ^")"))

                           db))
                | _ -> failwith "Internal error: insert row into non-database"))),
    (* FIXME: reboxing of `RowVar <-> Row_variable *)
    let v = new_raw_variable () in
      ([`RowVar v],
       (string --> (`DB --> (make_empty_record_with_row_var v --> make_unit ()))))));

  ("deleterows", (* destructive *)
   (primfun "deleterows"
    (fun table ->
       let table = charlist_as_string table in 
         continuationize_primfn (primfun ("delete from (" ^ table ^ ", ...) values ...")
             (function
                | `Database (db, _) as database ->
                    continuationize_primfn (primfun ("delete from (" ^ table ^ ", "^ string_of_result database ^ ") values ...")
                      (fun (rows : result) ->
                         Database.execute_select
                           (`Collection (`List, make_unit ()))
                           ("delete from " ^ table ^ " where " ^ delete_condition rows)
                           db))
                | _ -> failwith "Internal error: delete row from non-database"))),
    let v = new_raw_variable () in
      ([`RowVar v],
       (string --> (`DB --> (`Collection (`List, make_empty_record_with_row_var v) --> make_unit ()))))));

  ("updaterows", (* destructive *)
   (primfun "updaterows"
    (fun table ->
       let table = charlist_as_string table in 
         continuationize_primfn (
primfun ("update (" ^ table ^ ", ... ) by ...")
         (function
            | `Database (db, _) as database ->
continuationize_primfn (
                primfun ("update (" ^ table ^ ", "^ string_of_result database ^ ") by ...")
                  (function
                     |  (`Collection(`List, rows)) ->
                          (List.iter (fun row -> 
(*                                         debug("update " ^ table ^ " set " ^ *)
(*                                                 updates row  ^ " where " ^ single_match (map (fst -<- pair_as_ocaml_pair) row)); *)
                                        ignore(Database.execute_select
                                                 (`Collection (`List, make_unit ()))
                                                 ("update " ^ table ^ " set " ^ updates (links_snd row)  ^ " where " ^ single_match (links_fst row))
                                                 db))
                             rows);
                          `Record []
                     | _ -> failwith "Internal error: non-list passed to UPDATE"))
            | _ -> failwith "Internal error: update row in non-database"))),
    let v = new_raw_variable () in
    let u = new_raw_variable () in

    let pair = `Record
      (TypeOps.set_field ("1", `Present (make_empty_record_with_row_var u))
	 (TypeOps.set_field ("2", `Present (make_empty_record_with_row_var v))
	    (TypeOps.make_empty_closed_row ())))
(*
    let Row_variable v = fresh_row_variable () in
    let Row_variable u = fresh_row_variable () in
    let pair = `Record [Kind.Field_present ("1", `Record [Row_variable u]);
                        Kind.Field_present ("2", `Record [Row_variable v])] 
*)
    in
      ([`RowVar v],
       (string --> (`DB --> (`Collection (`List, pair) --> make_unit ()))))));

(*  ("javascript",
   (`Primitive(`Bool false),
    ([], `Primitive `Bool))); *)
  ("javascript",
(primfun "not" 
      (function 
         | `Primitive (`Bool x) -> `Primitive(`Bool (not x))
         | _ -> failwith "Internal error: non-boolean passed to `not'"),
    ([], `Primitive `Bool))); 

  ("dom",
   (primfun "dom"
      (function _ -> failwith "dom not implemented on server"),
    ([], `Primitive `Int)));

  ("not", 
   (primfun "not" 
      (function 
         | `Primitive (`Bool x) -> `Primitive(`Bool (not x))
         | _ -> failwith "Internal error: non-boolean passed to `not'"),
    ([],
     (`Primitive `Bool --> `Primitive `Bool))));
  
  ("negate", 
   (primfun "negate" 
      (function 
         | `Primitive (`Int x) -> `Primitive(`Int (minus_num x))
         | _ -> failwith "Internal error: non-integer passed to `negate"),
    ([],
     (`Primitive `Int --> `Primitive `Int))));  

  ("negatef", 
   (primfun "negatef" 
      (function 
         | `Primitive (`Float x) -> `Primitive(`Float (-. x))
         | _ -> failwith "Internal error: non-float passed to `negatef"),
    ([],
     (`Primitive `Float --> `Primitive `Float))));

  ("is_integer", 
   (primfun "is_integer" 
      (fun s -> `Primitive (`Bool (Str.string_match (Str.regexp "^[0-9]+$") (charlist_as_string s) 0))),
    ([],
     (Kind.string --> `Primitive `Bool))));

(*   ("fullname", *)
(*    (primfun "fullname" *)
(*       (function uid -> *)
(*          try *)
(*            `Variant ("fullname",  *)
(*                      box_string (Str.global_replace (Str.regexp ",*$") ""  *)
(*                                    (Unix.getpwnam (unbox_string uid)).Unix.pw_gecos)) *)
(*          with Not_found ->  *)
(*            `Variant ("not_found", make_unit ())), *)
(*     let `Row_variable v = fresh_row_variable () in *)
(*       ([`RowVar v], Kind.string -->  *)
(*          `Variant ([`Row_variable v; *)
(*                      `Field_present ("fullname", Kind.string); *)
(*                      `Field_present ("not_found", make_unit ())])))); *)

  ("query_param",           (* Get query parameters from the web environment
                                   (Right now only works in CGI environment) *)
   (primfun "query_param"
      (function name_charlist -> 
	 let name = charlist_as_string name_charlist in
	   debug("trying to grab " ^ name ^ " from " ^ 
		   (string_of_alist !query_environment));
	   try
	     string_as_charlist(assoc name !query_environment)
	   with
	       (* TBD: need option types within Links *)
	       Not_found -> string_as_charlist "" 
      ),
    ([],
     (Kind.string --> Kind.string))));

  ("get_cookie",
   (primfun "get_cookie"
      (function cookiename_charlist ->
	 let cookiename = charlist_as_string cookiename_charlist 
         and cookie_header = Sys.getenv "HTTP_COOKIE" in
	   string_as_charlist(cookie_header)
      ),
    ([],
     Kind.string --> Kind.string))
  );
  

  ("error",
   (primfun "error"
      (function msg -> failwith (unbox_string msg)),
    let v = fresh_type_variable () in
      ([v],
       (Kind.string --> v))));

  ("sleep",
   (primfun "sleep"
      (function duration ->
         Unix.sleep(int_of_num (unbox_int duration));
         `Record []),
    let v = fresh_type_variable () in
      ([v],
       (`Primitive `Int --> v))));

  ("domutate",
   (primfun "domutate"
      (function mutations ->
         failwith("domutate not implemented on server side.")),
    let u = fresh_type_variable () in
    let v = fresh_type_variable () in
      ([u; v],
       (u --> v))));


  (* some char functions *)
  char_test_op "isAlpha" (function 'a'..'z' | 'A'..'Z' -> true | _ -> false);
  char_test_op "isAlpha" (function 'a'..'z' | 'A'..'Z' | '0'..'9' -> true | _ -> false);
  char_test_op "isLower" (function 'a'..'z' -> true | _ -> false);
  char_test_op "isUpper" (function 'A'..'Z' -> true | _ -> false);
  char_test_op "isDigit" (function '0'..'9' -> true | _ -> false);
  char_test_op "isXDigit" (function '0'..'9'|'a'..'f'|'A'..'F' -> true | _ -> false);
  char_test_op "isBlank" (function ' '|'\t' -> true | _ -> false);
  (* isCntrl, isGraph, isPrint, isPunct, isSpace *)

  char_conversion  "toUpper" Char.uppercase;
  char_conversion  "toLower" Char.lowercase;

  ("ord",
   (primfun "ord"
      (function 
         | `Primitive (`Char c) -> `Primitive (`Int (num_of_int (Char.code c)))
         | c -> failwith ("Error unboxing char : "  ^ string_of_result c)),
    ([], `Primitive `Char --> `Primitive `Int)));

  ("chr",
   (primfun "chr"
      (function 
         | `Primitive (`Int n) -> `Primitive (`Char (Char.chr (int_of_num n)))
         | c -> failwith ("Error unboxing char : "  ^ string_of_result c)),
    ([], `Primitive `Int --> `Primitive `Char)));


  (* some trig functions *)
  float_fun "floor" floor;
  float_fun "ceiling" ceil;
  float_fun "cos" cos;
  float_fun "sin" sin;
  float_fun "tan" tan;
  float_fun "log" log;
  float_fun "sqrt" sqrt;
]

(* Our primitive environment currently has the implementations
   embedded within it. This is full of problems. For now, kosherize
   splits such an environment into a meat meal and a dairy meal. The
   meat meal has the implementations, tagged by name, and the dairy
   meal has everything else (also tagged by name).  Eventually we
   should split this into two things, rather than creating the
   combined meal and pulling it apart.

   Proper dairy meal (goes in the interpreter's environment:

   ["+"  =>  `Primitive("+", []);
   "-" => `Primitive("-", [])]

   Proper meat meal:

   ["+" => (function foo -> bar);
   "-" => (function baz -> bust)
   ]

*)
let kosherize_primfunc = function
    (name, `Primitive(`PFunction(name2, impl, pargs))) ->
      (name, `Primitive(`PFunction(name2, None, pargs))),
      (name, valOf impl)
  | _ -> failwith "boom"

let kosherize_primenv env = split (map kosherize_primfunc env)

let envs environment : (Result.environment * Kind.environment) = 
  split (map (fun (name, (value, kind)) -> (name, value), (name, kind)) environment)

let primvalue_env, type_env = envs env
let value_env, implementations = kosherize_primenv primvalue_env 

let get_prim goal = 
  snd (List.find (fun (name, impl) -> (name = goal)) implementations)
    
