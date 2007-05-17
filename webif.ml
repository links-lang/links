(*pp deriving *)

open Performance
open Utility
open Result

(*
 Whether to cache programs after the optimization phase
*)
let cache_programs = Settings.add_bool ("cache_programs", false, `User)

type query_params = (string * result) list
deriving (Show)

type web_request = ContInvoke of continuation * query_params
                   | ExprEval of Syntax.expression * environment
                   | ClientReturn of continuation * result
                   | RemoteCall of result * result list
                   | CallMain
                       deriving (Show)

(* Does at least one of the functions have to run on the client? *)
let is_client_program (Syntax.Program (defs, _) as program) =
  let is_client_def = function
    | Syntax.Define (_, _, `Client, _) -> true
    | _ -> false 
  and toplevels = (concat_map 
                     (function
                        | Syntax.Define (n, _, _, _) -> [n]
                        | _ -> [])) defs
  and is_client_prim p = 
    (* Syntax.freevars is currently broken: it doesn't take l:name
       bindings into account.  It's tricky to fix, because the Syntax
       module doesn't know about l:name.  The problem that arises here
       is that anything bound by l:name ends up looking like a
       primitive (because analysis indicates that it's free in the
       program).  When l:name goes away this problem will, too.  
       Let's just work around it for now. *)
    try 
      (Library.primitive_location ->- (=) `Client) p
    with Not_found ->  false
  in
  let freevars = StringSet.elements (Syntax.freevars_program program) in
  let prims = List.filter (not -<- flip List.mem toplevels) freevars
  in 
    List.exists is_client_def defs || List.exists is_client_prim prims

let with_prelude prelude (Syntax.Program (defs, body)) =
  Syntax.Program (prelude @ defs, body)

(* Read in and optimise the program *)
let read_and_optimise_program prelude typenv filename = 
  let program = lazy(Parse.parse_file Parse.program filename)
    <|measure_as|> "parse" in
  let tenv, program = lazy(Inference.type_program typenv program)
    <|measure_as|> "type" in
  let tenv, program = 
    tenv, lazy((Optimiser.optimise_program(tenv, with_prelude prelude program)))
      <|measure_as|> "optimise" in
  let tenv, program = tenv, Syntax.labelize program in
    tenv, program
              
let read_and_optimise_program prelude env arg 
    : Types.typing_environment * Syntax.program
  = 
  if Settings.get_value cache_programs then
    Loader.read_file_cache arg
  else
    read_and_optimise_program prelude env arg

let serialize_call_to_client (continuation, name, arg) = 
  Json.jsonize_call continuation name arg

let untuple r =
  let rec un n accum list = 
    match List.partition (fst ->- (=) (string_of_int n)) list with
      | [_,item], rest -> un (n+1) (item::accum) rest
      | [], [] -> List.rev accum
      | _ -> assert false
  in match r with
    | `Record args -> un 1 [] args
    | _ -> assert false

let stubify_client_funcs globals (Syntax.Program(defs,body) as program) : Result.environment = 
  Interpreter.program_source := program;
  let is_server_fun = function
    | Syntax.Define (_, _, (`Server|`Unknown), _) -> true
    | Syntax.Define (_, _, (`Client|`Native), _) -> false
    | Syntax.Alien ("javascript", _, _, _) -> false
    | Syntax.Alias _ -> true
  in 
  let server_defs, client_defs = List.partition is_server_fun defs in
  let client_env =
    List.map (function
                 | Syntax.Define (name, _, _, _)
                 | Syntax.Alien (_, name, _, _) -> 
(*                      Library.value_env := StringMap.add *)
(*                        name (`PFun (fun [cont; args] ->  *)
(*                                       match cont, args with *)
(*                                           `Continuation cont, `List args ->  *)
(*                                             (client_call_impl program name cont args) *)
(*                                         | _ -> assert false)) *)
(*                        (!Library.value_env); *)
                     (name, `ClientFunction name))
      client_defs in
    match server_defs with 
        [] -> []
      | server_defs -> (* evaluate the definitions to get Result.result values. *)
          Interpreter.run_defs globals [] server_defs
            @ client_env

let get_remote_call_args env cgi_args = 
  let fname = Utility.base64decode (List.assoc "__name" cgi_args) in
  let args = Utility.base64decode (List.assoc "__args" cgi_args) in
  let args = untuple (Json.parse_json args) in
    RemoteCall(List.assoc fname env, args)

let decode_continuation (cont : string) : Result.continuation =
  let fixup_cont = 
  (* At some point, '+' gets replaced with ' ' in our base64-encoded
     string.  Here we put it back as it was. *)
    Str.global_replace (Str.regexp " ") "+" 
  in Marshal.from_string (Utility.base64decode (fixup_cont cont)) 0

let is_special_param (k, _) =
  List.mem k ["_cont"; "_k"; "_jsonArgs"]

let string_dict_to_charlist_dict =
  alistmap Result.string_as_charlist

let lookup_either a b env = 
  try List.assoc a env
  with Not_found -> List.assoc b env

(* Extract continuation from the parameters passed in over CGI.*)
let contin_invoke_req valenv program params =
  let pickled_continuation = List.assoc "_cont" params in
  let params = List.filter (not -<- is_special_param) params in
  let params = string_dict_to_charlist_dict params in
    ContInvoke(unmarshal_continuation valenv program pickled_continuation, params)

let unpack_links_list = function
  | `List items -> items
  | _ -> assert false

let unpack_links_alist = function
  | `List items -> List.map (function `List [k;v] -> assert(Result.is_string k);
                               (charlist_as_string k,v)
                               | _ -> assert false) items
  | _ -> assert false

(* Extract expression/environment pair from the parameters passed in over CGI.*)
let expr_eval_req valenv program params =
  let expr, env = unmarshal_exprenv (rng valenv) program (List.assoc "_k" params) in
  let jsonArgs = (try
            unpack_links_alist (Json.parse_json_b64 (List.assoc "_jsonArgs" params))
          with Not_found -> [])
  in
  let params = List.filter (not -<- is_special_param) params in
  let params = string_dict_to_charlist_dict params in
    ExprEval(expr, params @ env @ jsonArgs)

let is_remote_call params =
  List.mem_assoc "__name" params && List.mem_assoc "__args" params

let is_func_appln params =
  List.mem_assoc "__name" params && List.mem_assoc "__args" params

let is_client_call_return params = 
  List.mem_assoc "__continuation" params && List.mem_assoc "__result" params

let is_contin_invocation params = 
  List.mem_assoc "_cont" params

let is_expr_request = List.exists is_special_param
        
let client_return_req cgi_args = 
  let continuation = decode_continuation(List.assoc "__continuation" cgi_args) in
(*   Debug.print("Got web request to apply continuation " ^ *)
(*                 string_of_cont continuation); *)
  let arg = Json.parse_json_b64 (List.assoc "__result" cgi_args) in
(*   Debug.print("Continuation argument: " ^ *)
(*                 string_of_result (untuple_single arg)); *)
    ClientReturn(continuation, arg)

let error_page_stylesheet = 
  "<style>pre {border : 1px solid #c66; padding: 4px; background-color: #fee} code.typeError {display: block; padding:1em;}</style>"

let error_page body = 
  "<html>\n  <head>\n    <title>Links error</title>\n    " ^ 
    error_page_stylesheet ^ 
    "\n  </head>\n  <body>" ^ 
    body ^ 
    "\n  </body></html>"

let is_multipart () =
  ((Cgi.safe_getenv "REQUEST_METHOD") = "POST" &&
      Cgi.string_starts_with (Cgi.safe_getenv "CONTENT_TYPE") "multipart/form-data")

let perform_request 
    program (* orig. src.: only used for gen'ing js *)
    globals main req =
(*   Debug.print("free variables of main: " ^  *)
(*                 String.concat ", " ((Syntax.freevars main) *)
(*                                     <|difference|> (dom globals))); *)
(*   assert(Syntax.is_closed_wrt main (dom globals)); *)
  match req with
    | ContInvoke (cont, params) ->
        Library.print_http_response [("Content-type", "text/html")]
          (Result.string_of_result 
             (Interpreter.apply_cont_safe globals cont (`Record params)))
    | ExprEval(expr, env) ->
(*         Debug.print("Web request to evaluate:\n" ^ *)
(*                       Syntax.string_of_expression expr); *)
(*         Debug.print("Given env:\n" ^ Result.string_of_environment env); *)
(*         Debug.print("undef'd variables: " ^ *)
(*                       String.concat "," (difference (Syntax.freevars expr) *)
(*                                            (dom globals @ dom env @ dom (fst Library.typing_env))) *)
(*                    ); *)
        (* This assertion failing indicates that not everything needed
           was serialized into the link: *)
        assert(Syntax.is_closed_wrt expr 
                 (StringSet.from_list (dom globals @ dom env @ dom (fst Library.typing_env))));
        Library.print_http_response [("Content-type", "text/html")]
          (Result.string_of_result 
             (snd (Interpreter.run_program globals env (Syntax.Program ([], expr)))))
    | ClientReturn(cont, value) ->
        Interpreter.has_client_context := true;
(*         Debug.print("Web request to continue with: " ^ Result.string_of_cont cont *)
(*                        ^ " given value " ^ string_of_result value); *)
        let result_json = (Json.jsonize_result 
                             (Interpreter.apply_cont_safe globals cont value)) in
(*            Debug.print("sending back result of client->server call: " ^  *)
(*                          result_json); *)
        Library.print_http_response [("Content-type", "text/plain")]
          (Utility.base64encode 
             result_json)
    | RemoteCall(func, args) ->
(*        Debug.print("Entering program for remote call.");*)
        Interpreter.has_client_context := true;
        let cont, value = 
          match args with
            | [] -> [Result.ThunkApply []], func
            | _::_ -> [Result.FuncApply ([], func, [], (butlast args))], (last args)
        in
	  Library.print_http_response [("Content-type", "text/plain")]
            (Utility.base64encode
               (Json.jsonize_result
                  (Interpreter.apply_cont_safe globals cont value)))
    | CallMain -> 
(*        Debug.print("Entering program through Main");*)
        Library.print_http_response [("Content-type", "text/html")] 
          (if is_client_program program then
             catch_notfound_l "generate_program"
               (lazy(Js.generate_program program))
           else 
             let _env, rslt = Interpreter.run_program globals [] (Syntax.Program ([], main)) in
               Result.string_of_result rslt)

let serve_request prelude (valenv, typenv) filename = 
  try 
    let _, (Syntax.Program (defs, main) as program) = Utility.catch_notfound_l "reading and optimising"
      (lazy (read_and_optimise_program prelude typenv filename)) in
(*    if (List.length main < 1) then raise Errors.NoMainExpr else*)
    let defs = Utility.catch_notfound_l "stubifying" 
      (lazy (stubify_client_funcs valenv program)) in
    let cgi_args =
      if is_multipart () then
        List.map (fun (name, {Cgi.value=value}) ->
                    (name, value)) (Cgi.parse_multipart_args ())
      else
        Cgi.parse_args () in
      Library.cgi_parameters := cgi_args;
    let request = 
      if is_remote_call cgi_args then
        get_remote_call_args defs cgi_args
      else if is_client_call_return cgi_args then
        client_return_req cgi_args
      else if (is_contin_invocation cgi_args) then
        contin_invoke_req (rng valenv) program cgi_args
      else if (is_expr_request cgi_args) then
        expr_eval_req valenv program cgi_args
      else
        CallMain
    in
      Utility.catch_notfound_l "performing request"
        (lazy (perform_request program (defs @ valenv) main request))
  with
      (* FIXME: errors need to be handled differently
         btwn. user-facing and remote-call modes. *)
      Failure msg -> prerr_endline msg;
        Library.print_http_response [("Content-type", "text/html; charset=utf-8")] 
        (error_page (Errors.format_exception_html (Failure msg)))
    | exc -> Library.print_http_response [("Content-type", "text/html; charset=utf-8")]
        (error_page (Errors.format_exception_html exc))
          
let serve_request envs filename =
  Errors.display (lazy (serve_request envs filename))

