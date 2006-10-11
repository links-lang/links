open Utility
open Result

(*
 Whether to cache programs after the optimization phase
*)
let cache_programs = Settings.add_bool ("cache_programs", false, true)

type query_params = (string * result) list

type web_request = ContInvoke of continuation * query_params
                   | ExprEval of Syntax.expression * environment
                   | ClientReturn of continuation * result
                   | RemoteCall of result * result
                   | CallMain

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
    iter_over headers
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
let read_and_optimise_program filename : (Syntax.expression list) = 
  (Performance.measure "optimise" Optimiser.optimise_program)
    ((fun (env, exprs) -> env, List.map Syntax.labelize exprs)
       ((Performance.measure "type" (Inference.type_program Library.type_env))
          ((Performance.measure "parse" (Parse.parse_file Parse.program)) filename)))
              
let read_file_cache filename : (Syntax.expression list) = 
  let cachename = filename ^ ".cache" in
    try
      if ((Unix.stat cachename).Unix.st_mtime > (Unix.stat filename).Unix.st_mtime) then
        let infile = open_in cachename in
        let program = Marshal.from_channel infile in
          close_in infile;
          program
      else
        raise (Sys_error "booyah")
    with (Sys_error _| Unix.Unix_error _) ->
      let program = 
        (Performance.measure "optimise" Optimiser.optimise_program)
          ((fun (env, exprs) -> env, List.map Syntax.labelize exprs)
             ((Performance.measure "type" (Inference.type_program Library.type_env))
                ((Performance.measure "parse" (Parse.parse_file Parse.program)) filename)))
      in 
	(try (* try to write to the cache *)
	   let outfile = open_out cachename in 
           let (pos, dt, _) = Syntax.no_expr_data in
             Marshal.to_channel outfile (List.map (Syntax.Functor_expression'.map (fun (_,_,l) -> pos, dt, l)) program) [Marshal.Closures] ;
             close_out outfile
	 with _ -> ());
        program

let read_and_optimise_program arg = 
  if Settings.get_value cache_programs then
    read_file_cache arg
  else 
    read_and_optimise_program arg

let encode_continuation (cont : Result.continuation) : string =
  Utility.base64encode (Marshal.to_string cont [Marshal.Closures])

let serialize_call_to_client (continuation, name, arg) =
  Json.jsonize_result
    (`Record [
       "__continuation", Result.string_as_charlist (encode_continuation continuation);
       "__name", Result.string_as_charlist name;
       "__arg", arg
     ])

let parse_json = Jsonparse.parse_json Jsonlex.jsonlex -<- Lexing.from_string

let untuple_single = function
  | `Record ["1",arg] -> arg
  | r -> r

let stubify_client_funcs env = 
  let is_server_fun = function
    | Syntax.Define (_, _, (`Server|`Unknown), _) -> true
    | Syntax.Define (_, _, (`Client|`Native), _) -> false
    | Syntax.Alien ("javascript", _, _, _) -> false
    | e  -> failwith ("Unexpected non-definition in environment : " 
		      ^ Syntax.string_of_expression e)
  in 
  let server_env, client_env = List.partition is_server_fun env in
    List.iter (function
                 | Syntax.Define (name, _, _, _)
                 | Syntax.Alien (_, name, _, _) -> 
		      let f (_, cont, arg) =
			let call = serialize_call_to_client (cont, name, arg) in
			  (print_endline ("Content-type: text/plain\n\n" ^ 
					    Utility.base64encode call);
			   exit 0)
		      in 
                        Library.value_env := (name,`PFun f):: !Library.value_env)
      client_env;
    match server_env with 
        [] -> []
      | server_env ->
          fst (Interpreter.run_program [] server_env)

let get_remote_call_args env cgi_args = 
  let fname = Utility.base64decode (List.assoc "__name" cgi_args) in
  let args = Utility.base64decode (List.assoc "__args" cgi_args) in
  let args = untuple_single (parse_json args) in
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
let contin_invoke_req program params =
  let pickled_continuation = List.assoc "_cont" params in
  let params = List.filter (not -<- is_special_param) params in
  let params = string_dict_to_charlist_dict params in
    ContInvoke(unmarshal_continuation program pickled_continuation, params)

(* Extract expression/environment pair from the parameters passed in over CGI.*)
let expr_eval_req program prim_lookup params =
  let expression, environment = unmarshal_exprenv program (List.assoc "_k" params) in
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
        
let client_return_req env cgi_args = 
  let continuation = decode_continuation (List.assoc "__continuation" cgi_args) in
  let parse_json_b64 = parse_json -<- Utility.base64decode in
  let arg = parse_json_b64 (List.assoc "__result" cgi_args) in
    ClientReturn(continuation, untuple_single arg)

let perform_request program globals main req =
  match req with
    | ContInvoke (cont, params) ->
        print_http_response [("Content-type", "text/html")]
          (Result.string_of_result 
             (Interpreter.apply_cont_safe globals cont (`Record params)))
    | ExprEval(expr, env) ->
        print_http_response [("Content-type", "text/html")]
          (Result.string_of_result 
             (snd (Interpreter.run_program (globals @ env) [expr])))
    | ClientReturn(cont, value) ->
        print_http_response [("Content-type", "text/plain")]
          (Utility.base64encode 
             (Json.jsonize_result 
                (Interpreter.apply_cont_safe globals cont value)))
    | RemoteCall(func, arg) ->
        let cont = [Result.FuncApply (func, [])] in
	  print_http_response [("Content-type", "text/plain")]
            (Utility.base64encode
               (Json.jsonize_result 
                  (Interpreter.apply_cont_safe globals cont arg)))
    | CallMain -> 
        print_http_response [("Content-type", "text/html")] 
          (if is_client_program program then
             Js.generate_program program main
           else Result.string_of_result (snd (Interpreter.run_program globals [main])))

let error_page_stylesheet = 
  "<style>pre {border : 1px solid #c66; padding: 4px; background-color: #fee}</style>"

let error_page body = 
  "<html>\n  <head>\n    <title>Links error</title>" ^ error_page_stylesheet ^ 
    "\n  </head>\n  <body>" ^ 
    body ^ 
    "\n  </body></html>"

let serve_request filename = 
  try 
    let program = read_and_optimise_program filename in
    let global_env, main = List.partition Syntax.is_define program in
    if (List.length main < 1) then raise Errors.NoMainExpr
    else
    let main = last main in
    let global_env = stubify_client_funcs global_env in
    let cgi_args = Cgi.parse_args () in
      Library.cgi_parameters := cgi_args;
    let request = 
      if is_remote_call cgi_args then 
        get_remote_call_args global_env cgi_args
      else if is_client_call_return cgi_args then
        client_return_req global_env cgi_args
      else if (is_contin_invocation cgi_args) then
        contin_invoke_req program cgi_args 
      else if (is_expr_request cgi_args) then
        expr_eval_req program (flip List.assoc global_env) cgi_args           
      else
        CallMain
    in
      perform_request program global_env main request
  with
      (* FIXME: errors need to be handled differently
         btwn. user-facing and remote-call modes. *)
      Failure msg -> prerr_endline msg;
        print_http_response [("Content-type", "text/html; charset=utf-8")] 
        (error_page (Errors.format_exception_html (Failure msg)))
    | exc -> print_http_response [("Content-type", "text/html; charset=utf-8")]
        (error_page (Errors.format_exception_html exc))
          
let serve_request filename =
  Errors.display_errors_fatal stderr
    serve_request filename
