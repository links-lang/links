(* Code needed for the "latent processes" technique. 

   Our goal is to send an HTML document, not a stub, to the client.

   If we want to support "latent spawning" of processes, what do we need?

   The value sent to the server should be HTML, enhanced with <script>
   tags containing the extras.


   Extras: 
   * functions compiled to JavaScript
   * processes "spawned" on the server.

   HTML document results from evaluation on the server, not just
   boilerplate.

   Strategy:
   1. compile all functions to JavaScript.

   2. evaluate the "main" expression.  

   When spawn occurrs, generate an id and add the label to a
   table mapping ids to code labels / values.


   Problem: the body of the spawn can contain all sorts of not very
   serialisable things: closures, continuations, etc.  Actually, we can
   serialise those things, but it's just a bit harder work.

*)


(* evaluate a so-called "client" program, delaying the evaluation of:
 * event handlers
 * spawned processes
 *)


(* 
   TODO:
      during compilation below:
          * compile the handler attributes to javascript
          * store the closed-over values at the top level, renaming as necessary.
      change SPAWN to return a suspended process.
          * it should return the id, and store the process in a table somewhere.
          * during compilation, add code to spawn suspeneded processes on reaching the client.
      change SEND and RECEIVE to fail if evaluated on the server.

   for now we don't need any new globals: we can just put suspended
   processes into the environment, perhaps with special variable names.


*)

open Utility
open Result

let evaluate = Interpreter.run_program [] ->- snd

let sprintf = Printf.sprintf

let rec js_of_result : result -> string = function
  | #primitive_value as p -> js_of_primitive p
  | `List fields -> sprintf "[%s]" (String.concat ", " (List.map js_of_result fields))
  | `Record (fields) -> sprintf "{%s}" (String.concat ", " 
                                          (List.map (uncurry (sprintf "'%s':%s")) 
                                             (Utility.alistmap js_of_result fields)))
  | `Variant (l, v) -> sprintf "{label:%s,value:%s}" l (js_of_result v)
  | `Function _
  | `PFunction _
  | `Continuation _ -> failwith "(currently) unjavascriptable result"
and js_of_primitive = function
  | `Bool b -> sprintf "%b" b
  | `Char c -> sprintf "'%c'" c
  | `Float f -> sprintf "%f" f
  | `Int n -> Num.string_of_num n
  | `XML xml -> js_of_xml xml
  | `Table _
  | `Database _ -> failwith "(currently) unjavascriptable primitive value"
and js_of_xml = function
  | _ -> failwith "xml serialisation not yet implemented"

let compile_active_attrs : xmlitem -> xmlitem * (string * string) list = 
  let values = ref [] in
  let rec compile = function
    | Node (n, children) -> Node (n, List.map compile children)
    | Text t -> Text t
    | Attr a -> Attr a
    | ActiveAttr (name, code) ->
        match code with
          | `Function ("event", locals, _, body) -> 
              ListLabels.iter locals
                ~f:(fun (name, value) -> 
                      values := (name, js_of_result value) :: !values)
              ;
              Attr (name, Js.make_handler_code body)
          | _ -> failwith "unexpected code while compiling active attributes"
  in
    fun xml -> 
      let result = compile xml in
        result, !values

let insert_script_code (code : string) : result -> result =
  let rec addnode globals : xmlitem -> xmlitem = function
    | Node ("head", children) -> 
        let base_url = Js.get_js_lib_url () in
        let include_script name =  
          Node ("script", [Attr ("type", "text/javascript");
                           Attr ("src", base_url ^ name);
                           Text " "]) in
        Node ("head", 
                 include_script "json.js"
              :: include_script "regex.js"
              :: include_script "yahoo/YAHOO.js"
              :: include_script "yahoo/event.js"
              :: Node ("script", [Attr ("type","text/javascript"); 
                                  Text (Printf.sprintf "var DEBUGGING = %b;\n"
                                          (Settings.get_value(Debug.debugging_enabled)));
                                  Text (String.concat "\n" (List.map (uncurry (sprintf "var %s = %s;")) globals))])
              :: include_script "jslib.js"
              :: Node ("script", [Attr ("type","text/javascript"); Text code])
              :: children)
    | Node (tag, children) -> 
        Node (tag, List.map (addnode globals) children)
    | other -> other 
  in function
    | `XML node -> (let node, globals = compile_active_attrs node in
                      `XML (addnode globals node))
    | p -> assert false

(* print the result, compiling handlers along the way *)
let print_xml_result : result -> string = Result.string_of_result
  
let mostly_run_program program = 
  match evaluate program, Js.compile (butlast program) with
    | `List [`XML _ as x], compiled -> 
        Debug.debugf "generated : %s" compiled;
        insert_script_code compiled x
    | other, _ -> failwith (Printf.sprintf "expected top-level expression to be XML (not %s)" (Result.Show_result.show other))
        
let dump_suspended_process : result -> string
  = fun _ -> failwith "NYI"
