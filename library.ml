open Sys
open Num
open List

open Kind
open Result
open Utility
open Debug

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

(* let xml = `List (`Primitive `XMLitem) *)

type primop = (Result.result -> Result.result)

let value_as_string db = function
  | `List (`Primitive(`Char _)::_) as c  -> "\'" ^ db # escape_string (charlist_as_string c) ^ "\'"
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

let int_op impl : primitive * Kind.assumption = 
  (`PFun (fun x -> `PFun (fun y -> (`Primitive (`Int (impl (unbox_int x) (unbox_int y))))))),
  ([], `Primitive `Int --> (`Primitive `Int --> `Primitive `Int))

let float_op impl : primitive * Kind.assumption = 
  (`PFun (fun x -> `PFun (fun y -> (`Primitive (`Float (impl (unbox_float x) (unbox_float y))))))),
  ([], `Primitive `Float --> (`Primitive `Float --> `Primitive `Float))

let conversion_op ~from ~unbox ~conv ~(box :'a->result) ~into : primitive * Kind.assumption =
  let box = (box :> 'a -> primitive) in
  (`PFun (fun x -> (box (conv (unbox x)))),
   ([], from --> into))

let char_test_op fn = 
  (`PFun (fun c -> (`Primitive (`Bool (fn (unbox_char c))))),
   ([], `Primitive `Char --> `Primitive `Bool))

let char_conversion fn = 
  (`PFun (fun c ->  (box_char (fn (unbox_char c)))),
   ([], `Primitive `Char --> `Primitive `Char))

let float_fn fn = 
  (`PFun (fun c ->  (box_float (fn (unbox_float c)))),
   ([], `Primitive `Float --> `Primitive `Float))

let p1 fn : primitive = 
  `PFun (fun a ->  (fn a))
and p2 fn : primitive = 
  `PFun (fun a -> `PFun (fun b ->  (fn a b)))
and p3 fn : primitive = 
  `PFun (fun a -> `PFun (fun b -> `PFun (fun c ->  (fn a b c))))

let notimpl fn = 
  p1 (fun _ -> failwith (Printf.sprintf "%s is not implemented on the server" fn))

let kind = Parse.parse_kind
let _UNTYPED_ = kind "a"

let env : (string * (primitive * Kind.assumption)) list = [
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
  "^^", float_op ( ** );

  (** Conversions (any missing?) **)
  "int_of_string",   conversion_op ~from:Kind.string_type ~unbox:unbox_string ~conv:num_of_string ~box:box_int ~into:(`Primitive `Int);
  "float_of_int",    conversion_op ~from:(`Primitive `Int) ~unbox:unbox_int ~conv:float_of_num ~box:box_float ~into:(`Primitive `Float);
  "string_of_int",   conversion_op ~from:(`Primitive `Int) ~unbox:unbox_int ~conv:string_of_num ~box:box_string ~into:Kind.string_type;
  "string_of_float", conversion_op ~from:(`Primitive `Float) ~unbox:unbox_float ~conv:string_of_float ~box:box_string ~into:Kind.string_type;

  (** concurrency **)
  "send",
  (p2 (fun pid msg -> 
         let pid = int_of_num (unbox_int pid) in
           (try 
              Queue.push msg (Hashtbl.find messages pid)
            with Not_found -> failwith ("Internal error while sending message: no mailbox for " ^ string_of_int pid));
           (try 
              Queue.push (Hashtbl.find blocked_processes pid) suspended_processes;
              Hashtbl.remove blocked_processes pid
            with Not_found -> ());
           `Record []),
   kind "Mailbox a -> a -> ()");

  "self",
  (p1 (fun _ -> `Primitive (`Int (num_of_int !current_pid))),
   kind "Mailbox a -> () -> Mailbox a");
  
  "recv",
  (* this function is not used, as its application is a special case
     in the interpreter. But we need it here, for now, because of its
     type.  Ultimately we should probably not special-case it, but
     rather provide a way to implement this primitive from here.
     (Ultimately, it should perhaps be a true primitive (an AST node),
     because it uses a different evaluation mechanism from functions.
     -- jdy) *)
    (p1 (fun _ -> assert false), kind "Mailbox a -> () -> a"); (* Yes, this is the right type. *)
  
  "spawn",
  (* This should also be a primitive, as described in the ICFP paper. *)
  (p2 (fun f p ->
         let new_pid = fresh_pid () in
           Hashtbl.add messages new_pid (Queue.create ());
           Queue.push ((FuncApply(f, []) :: [], p), new_pid) suspended_processes;
           `Primitive (`Int (num_of_int new_pid))),
   (*
     a: spawn's mailbox type (ignored, since spawn doesn't recv)
     b: the mailbox type of the spawned process
     c: the parameter expected by the process function
     d: the return type of the spawned process function (ignored)
   *)
   kind "Mailbox a -> (Mailbox b -> c -> d) -> Mailbox a -> c -> Mailbox b");

  "_MAILBOX_",
  (`Primitive (`Int (num_of_int 0)), 
   let _, u = fresh_type () in
     (* Deliberately non-quantified type.  Mailboxes are
        non-polymorphic, so this is a so-called "weak type
        variable". *)
     ([], `Mailbox u));

  (** Lists and collections **)
  "hd",
  (p1 (function
         | `List ((#result as x)::_) -> x
         | `List [] -> failwith "Head of empty list"
         | _ -> failwith "Internal error: head of non-list"),
   kind "[a] -> a");

  "tl", 
  (p1 (function
         | `List (_::xs) -> `List xs
         | `List [] -> failwith "Tail of empty list"
         | _ -> failwith "Internal error: tail of non-list"),
   kind "[a] -> [a]");
       
  "length", 
  (p1 (function
         | `List (elems) -> `Primitive (`Int (num_of_int (length elems)))
         | _ -> failwith "Internal error: length of non-collection"),
   kind "[a] -> Int");

  "take",
  (p2 (fun n l -> 
         match l with 
           | `List elems -> `List (take (int_of_num (unbox_int n)) elems)
           | _ -> failwith "Internal error: non-list passed to take"),
   kind "Int -> [a] -> [a]");

  "drop",
  (p2 (fun n l ->
         match l with 
           | `List elems -> `List (drop (int_of_num (unbox_int n)) elems)
           | _ -> failwith "Internal error: non-list passed to drop"),
   kind "Int -> [a] -> [a]");

  (** XML **)
  "childNodes",
  (p1 (function
         | `List [`Primitive(`XML(Node(_, children)))] ->
             let children = filter (function (Node _) -> true | _ -> false) children in
               `List (map (fun x -> `Primitive (`XML x)) children)
         | _ -> failwith "non-XML given to childNodes"),
   kind "XML -> XML");

  "objectType",
  (notimpl "objectType",
   let u', u = fresh_type () in
     ([u'], u --> Kind.string_type));

  "attribute",
  (p1 (let none = `Variant ("None", `Record []) in
         function
           | `Record elems -> 
               (let elem = assoc "1" elems
                and attr = charlist_as_string (assoc "2" elems) in
                  match elem with
                    | `List (`Primitive (`XML (Node (_, children)))::_) -> 
                        let attr_match = (function
                                            | Attr (k, _) when k = attr -> true
                                            | _ -> false) in
                          (try match find attr_match children with
                             | Attr (_, v) -> `Variant ("Some", string_as_charlist v)
                             | _ -> failwith "Internal error in `attribute'"
                           with Not_found -> none)
                    | _ -> none)
           | _ -> failwith "Internal error: bad arguments to attribute"),
   kind "(XML,String) -> [|Some:String | None:()|]");

  "elementById",
  (notimpl "elementById",
   kind "String -> [|Some:XML |None:()|]");
  
  "enxml",
  (p1 (function 
         | `List _ as c -> `List [`Primitive (`XML (Text (charlist_as_string c)))]
         | _ -> failwith "internal error: non-string value passed to xml conversion routine"),
   ([], Kind.string_type --> xml));

  "dom",
  (* Not available on the server *)
  ((`Primitive (`Int (num_of_int (-1)))),
   kind "Mailbox a");

  "debug", 
  (p1 (fun message -> prerr_endline (unbox_string message); flush stderr; `Record []),
   kind "String -> ()");

  "debugObj",
  (notimpl "debugObj", kind "a -> ()");
  
  "dump",
  (notimpl "dump", kind "a -> ()");
  
  "textContent",
  (notimpl "textContent", kind "a -> String");
  "print",
  (p1 (fun msg -> print_endline (unbox_string msg); flush stdout; `Record []),
   kind "String -> ()");

  "javascript",
  (`Primitive (`Bool false), kind "Bool");

  "not", 
  (p1 (fun b -> box_bool (not (unbox_bool b))),
   kind "Bool -> Bool");
 
  "negate", 
  (p1 (fun i -> box_int (minus_num (unbox_int i))), kind "Int -> Int");

  "negatef", 
  (p1 (fun f -> box_float (-. (unbox_float f))), kind "Float -> Float");

  "is_integer", 
  (p1 (fun s -> box_bool (Str.string_match (Str.regexp "^[0-9]+$") (unbox_string s) 0)),
   kind "String -> Bool");

  "error",
  (p1 (fun msg -> failwith (unbox_string msg)),
   kind "String -> a");


  (* HACK *)
  "callForeign",
   (notimpl "callForeign",
    let a', a = fresh_type() in
    let b', b = fresh_type() in
      ([a'; b'], a --> b));

  "sleep",
  (* This doesn't seem right : it freezes all threads *)
  (p1 (fun duration -> Unix.sleep (int_of_num (unbox_int duration));
         `Record []),
   kind "Int -> ()");

  (** Database functions **)
  "insertrow",
  (p3 (fun table database row ->
         match database with 
           | `Database (db, _) -> 
               (Database.execute_select 
                 (`List unit_type)
                 ("insert into " ^ unbox_string table ^ "("^ row_columns row ^") values ("^ row_values db row ^")")
                 db :> primitive)
           | _ -> failwith "Internal error: insert row into non-database"),
   (* FIXME: reboxing of `RowVar <-> Row_variable *)
   let r', r = fresh_row () in
     [r'],
   Kind.string_type --> (`DB --> (`Record r --> unit_type)));
  
  "deleterows", 
  (p3 (fun table database rows -> 
       match database with 
         | `Database (db, _)  ->
             (Database.execute_select
                (`List unit_type)
                ("delete from " ^ unbox_string table ^ " where " ^ delete_condition db rows)
                db :> primitive)
         | _ -> failwith "Internal error: delete row from non-database"),
   let r', r = fresh_row () in
     [r'],
   Kind.string_type --> (`DB --> (`List (`Record r) --> unit_type)));


  "updaterows", 
  (p3 (fun table database rows ->
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
       | _ -> failwith "Internal error: bad value passed to `updaterows'"),
   let v', v = fresh_row () in
   let u', u = fresh_row () in
   let pair = `Record
     (TypeOps.set_field ("1", `Present (`Record u))
        (TypeOps.set_field ("2", `Present (`Record v))
           (TypeOps.make_empty_closed_row ())))
   in
     [u'; v'],
   Kind.string_type --> (`DB --> (`List pair --> unit_type)));

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
   kind "Char -> Int");

  "chr",
  (p1 (fun n -> (box_char (Char.chr (int_of_num (unbox_int n))))), 
   kind "Int -> Char");

  (* some trig functions *)
  "floor",   float_fn floor;
  "ceiling", float_fn ceil;
  "cos",     float_fn cos;
  "sin",     float_fn sin;
  "tan",     float_fn tan;
  "log",     float_fn log;
  "sqrt",    float_fn sqrt;
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

type primitive_environment = (string*continuationized_val) list

let value_env = ref (List.map (fun (n, (v,_)) -> (n, continuationize v)) env)
and type_env : Kind.environment = Inference.retype_primitives (List.map (fun (n, (_,t)) -> (n,t)) env)

let apply_pfun (apply_cont :continuation -> result -> result) cont (name : string) (args : result list) = 
  let rec aux args' = function
    | #result as r -> 
	assert (args' = []);
	apply_cont cont r
    | `PFun f      -> 
	match args' with 
	  | []      -> apply_cont cont (`Primitive (`PFunction (name, args)))
	  | r::rest -> aux rest (f r) in
    if mem_assoc name env then
      aux args (fst (assoc name env))
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
  match assoc name env with 
    | `PFun _, _      -> `Primitive (`PFunction (name, []))
    | #result as r, _ -> r


