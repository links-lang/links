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
(*
  [REMARKS (sl)]
   - Currently print_http_response outputs the headers and body in
     one go.
   - At some point in the future we may want to consider implementing
     some form of incremental output.
   - Flushing the output stream prematurely (e.g. after outputting
     the headers and newline) appears to break client calls.
  [FURTHER REMARKS (eekc)]
   - Generally, many of the HTTP response headers will only be
     determined by running the program. It's fairly standard for a
     web app to wait until it knows there are no more headers coming
     (e.g., the program is finished) before printing the headers.
*)

(* output the headers and content to stdout *)
let print_http_response headers body =
  let headers = headers @ !Library.http_response_headers @
    if (!Library.http_response_code <> 200) then
      [("Status", string_of_int !Library.http_response_code)] else []
  in
    for_each headers
      (fun (name, value) -> print_endline(name ^ ": " ^ value));
    print_endline "";
    print_string body
      
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
  let freevars = Syntax.freevars_program program in
  let prims = List.filter (not -<- flip List.mem toplevels) freevars
  in 
    List.exists is_client_def defs || List.exists is_client_prim prims

let with_prelude prelude (Syntax.Program (defs, body)) =
  Syntax.Program (prelude @ defs, body)

(* Read in and optimise the program *)
let read_and_optimise_program prelude typenv filename = 
  (fun (env, program) ->
     (env,
      measure "optimise" Optimiser.optimise_program (env, with_prelude prelude program)))
    ((fun (env, program) ->
        env, Syntax.labelize program)
       ((measure "type" (Inference.type_program typenv))
          ((measure "parse" (Parse.parse_file Parse.program)) filename)))
              
let read_and_optimise_program prelude env arg 
    : Types.typing_environment * Syntax.program
  = 
  if Settings.get_value cache_programs then
    Loader.read_file_cache arg
  else
    read_and_optimise_program prelude env arg

let parse_json = Jsonparse.parse_json Jsonlex.jsonlex -<- Lexing.from_string

let untuple r =
  let rec un n accum list = 
    match List.partition (fst ->- (=) (string_of_int n)) list with
      | [_,item], rest -> un (n+1) (item::accum) rest
      | [], [] -> List.rev accum
      | _ -> assert false
  in match r with
    | `Record args -> un 1 [] args
    | _ -> assert false

let stubify_client_funcs globals defs : Result.environment = 
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
                     (name, `ClientFunction name)) client_defs in
    match server_defs with 
        [] -> []
      | server_defs -> (* evaluate the definitions to get Result.result values. *)
          Interpreter.run_defs globals [] server_defs
            @ client_env

let get_remote_call_args env cgi_args = 
  let fname = Utility.base64decode (List.assoc "__name" cgi_args) in
  let args = Utility.base64decode (List.assoc "__args" cgi_args) in
  let args = untuple (parse_json args) in
    RemoteCall(List.assoc fname env, args)

let decode_continuation (cont : string) : Result.continuation =
  let fixup_cont = 
  (* At some point, '+' gets replaced with ' ' in our base64-encoded
     string.  Here we put it back as it was. *)
    Str.global_replace (Str.regexp " ") "+" 
  in Marshal.from_string (Utility.base64decode (fixup_cont cont)) 0

let is_special_param (k, _) =
  List.mem k ["_cont"; "_k"]

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

(* Extract expression/environment pair from the parameters passed in over CGI.*)
let expr_eval_req valenv program params =
  let expression, environment = unmarshal_exprenv valenv program (List.assoc "_k" params) in
  let params = List.filter (not -<- is_special_param) params in
  let params = string_dict_to_charlist_dict params in
    ExprEval(expression, params @ environment)

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
  let continuation = decode_continuation (List.assoc "__continuation" cgi_args) in
  let parse_json_b64 = parse_json -<- Utility.base64decode in
  let arg = parse_json_b64 (List.assoc "__result" cgi_args) in
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
    (program (* orig. src.: only used for gen'ing js*))
    globals main req =
  match req with
    | ContInvoke (cont, params) ->
        print_http_response [("Content-type", "text/html")]
          (Result.string_of_result 
             (Interpreter.apply_cont_safe globals cont (`Record params)))
    | ExprEval(expr, env) ->
        print_http_response [("Content-type", "text/html")]
          (Result.string_of_result 
             (snd (Interpreter.run_program globals env (Syntax.Program ([], expr)))))
    | ClientReturn(cont, value) ->
        print_http_response [("Content-type", "text/plain")]
          (Utility.base64encode 
             (Json.jsonize_result 
                (Interpreter.apply_cont_safe globals cont value)))
    | RemoteCall(func, args) ->
        let cont, value = 
          match args with
            | [] -> [Result.ThunkApply []], func
            | _::_ -> [Result.FuncApply ([], func, [], (butlast args))], (last args)
        in
	  print_http_response [("Content-type", "text/plain")]
            (Utility.base64encode
               (Json.jsonize_result 
                  (Interpreter.apply_cont_safe globals cont value)))
    | CallMain -> 
        print_http_response [("Content-type", "text/html")] 
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
      (lazy (stubify_client_funcs valenv defs)) in
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
        expr_eval_req (rng valenv) program cgi_args
      else
        CallMain
    in
      Utility.catch_notfound_l "performing request"
        (lazy (perform_request program (defs @ valenv) main request))
  with
      (* FIXME: errors need to be handled differently
         btwn. user-facing and remote-call modes. *)
      Failure msg -> prerr_endline msg;
        print_http_response [("Content-type", "text/html; charset=utf-8")] 
        (error_page (Errors.format_exception_html (Failure msg)))
    | exc -> print_http_response [("Content-type", "text/html; charset=utf-8")]
        (error_page (Errors.format_exception_html exc))
          
let serve_request envs filename =
  Errors.display (lazy (serve_request envs filename))
