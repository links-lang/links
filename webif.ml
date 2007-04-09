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
                   | RemoteCall of result * result
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
let is_client_program defs =
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
  let freevars = Utility.concat_map Syntax.freevars defs in
  let prims = List.filter (not -<- flip List.mem toplevels) freevars
  in 
    List.exists is_client_def defs || List.exists is_client_prim prims

(* Read in and optimise the program *)
let read_and_optimise_program prelude typenv filename = 
  let program = lazy(Parse.parse_file Parse.program filename)
    <|measure_as|> "parse" in
  let (env, exprs) = lazy(Inference.type_program typenv program)
    <|measure_as|> "type" in
  let (env, exprs) = 
    env, lazy ((Optimiser.optimise_program (env, prelude @ exprs)))
      <|measure_as|> "optimise" 
  in
  let (env, exprs) = env, List.map Syntax.labelize exprs in
    (env, exprs)
              
let read_and_optimise_program prelude env arg 
    : Types.typing_environment * Syntax.expression list 
  = 
  if Settings.get_value cache_programs then
    Loader.read_file_cache arg
  else
    read_and_optimise_program prelude env arg

let has_client_context = ref false

let serialize_call_to_client (continuation, name, arg) = 
  Json.jsonize_call continuation name arg

let client_call_impl env name cont arg =
  let callPkg = Utility.base64encode(serialize_call_to_client(cont, name, arg)) 
  in
    if (not !has_client_context) then 
      begin
(*         failwith("Server-to-client calls not available during initial server phase of execution."); *)
        let start_script = "_invokeClientCall(_start, JSON.parseB64Safe(\"" ^ callPkg ^ "\"))" in
          print_http_response ["Content-type", "text/html"]
            (Js.make_boiler_page ~onload:start_script
               (Js.generate_program_defs env [name]))
          ; exit 0
      end
    else begin
      print_http_response ["Content-type", "text/plain"] callPkg;
      exit 0
    end

let untuple_single = function
  | `Record ["1",arg] -> arg
  | r -> r

let stubify_client_funcs globals env : Result.environment = 
  let is_server_fun = function
    | Syntax.Define (_, _, (`Server|`Unknown), _) -> true
    | Syntax.Define (_, _, (`Client|`Native), _) -> false
    | Syntax.Alien ("javascript", _, _, _) -> false
    | Syntax.TypeDecl _ -> true
    | e  -> failwith ("Unexpected non-definition in environment : " 
		      ^ Syntax.Show_expression.show e)
  in 
  let server_env, client_env = List.partition is_server_fun env in
  let client_env =
    List.map (function
                 | Syntax.Define (name, _, _, _)
                 | Syntax.Alien (_, name, _, _) -> 
		      let f (_, cont, arg) =
                        client_call_impl env name cont arg
		      in 
                        unshift Library.value_env (name, `PFun(f));
                        (name, `PFunction(name, []))
             )
      client_env in
    client_env @ match server_env with 
        [] -> []
      | server_env -> (* evaluate the definitions to get Result.result values. *)
          fst (Interpreter.run_program globals [] server_env)

let get_remote_call_args env cgi_args = 
  let fname = Utility.base64decode (List.assoc "__name" cgi_args) in
  let args = Utility.base64decode (List.assoc "__args" cgi_args) in
  let args = untuple_single (Json.parse_json args) in
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
    ClientReturn(continuation, untuple_single arg)

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
        print_http_response [("Content-type", "text/html")]
          (Result.string_of_result 
             (Interpreter.apply_cont_safe globals cont (`Record params)))
    | ExprEval(expr, env) ->
(*         Debug.print("Web request to evaluate:\n" ^ *)
(*                       Syntax.string_of_expression expr); *)
(*         Debug.print("Given env:\n" ^ Result.string_of_environment env); *)
        Debug.print("undef'd variables: " ^
                      String.concat "," (difference (Syntax.freevars expr)
                                           (dom globals @ dom env @ dom (fst Library.typing_env)))
                   );
        assert(Syntax.is_closed_wrt expr 
                 (dom globals @ dom env @ dom (fst Library.typing_env)));
        print_http_response [("Content-type", "text/html")]
          (Result.string_of_result 
             (snd (Interpreter.run_program globals env [expr])))
    | ClientReturn(cont, value) ->
(*         Debug.print("Web request to continue with: " ^ Result.string_of_cont cont *)
(*                       ^ " given value " ^ string_of_result value); *)
        let result_json = (Json.jsonize_result 
                             (Interpreter.apply_cont_safe globals cont value)) in
(*           Debug.print("sending back result of client->server call: " ^  *)
(*                         result_json); *)
        print_http_response [("Content-type", "text/plain")]
          (Utility.base64encode 
             result_json)
    | RemoteCall(func, arg) ->
        has_client_context := true;
        let cont = [Result.FuncApply (func, [])] in
	  print_http_response [("Content-type", "text/plain")]
            (Utility.base64encode
               (Json.jsonize_result 
                  (Interpreter.apply_cont_safe globals cont arg)))
    | CallMain -> 
        print_http_response [("Content-type", "text/html")] 
          (if is_client_program program then
             catch_notfound_l "generate_program"
               (lazy(Js.generate_program program main))
           else 
             let _env, rslt = Interpreter.run_program globals [] [main] in
               Result.string_of_result rslt)

let serve_request prelude (valenv, typenv) filename = 
  try 
    let _, program = Utility.catch_notfound_l "reading and optimising"
      (lazy (read_and_optimise_program prelude typenv filename)) in
    let defs, main = List.partition Syntax.is_define program in
    if (List.length main < 1) then raise Errors.NoMainExpr else
    let main = last main in
    let defs = Utility.catch_notfound_l "stubifying" 
      (lazy (stubify_client_funcs valenv defs)) in
    let cgi_args =
      if is_multipart () then
        List.map (fun (name, {Cgi.filename=_; Cgi.content_type=_; Cgi.value=value}) ->
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
        print_http_response [("Content-type", "text/html; charset=utf-8")] 
        (error_page (Errors.format_exception_html (Failure msg)))
    | exc -> print_http_response [("Content-type", "text/html; charset=utf-8")]
        (error_page (Errors.format_exception_html exc))
          
let serve_request envs filename =
  Errors.display (lazy (serve_request envs filename))

