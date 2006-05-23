open Utility
open Result


type query_params = (string * result) list

type web_request = ContInvoke of continuation * query_params
                   | ExprEval of Syntax.expression * environment
                   | ClientReturn of continuation * result
                   | RemoteCall of result * result
                   | CallMain

let print_http_response headers body = 
  List.map (fun (name, value) -> print_endline(name ^ ": " ^ value)) headers;
  print_endline "";
  print_string body


(* Does at least one of the functions have to run on the client? *)
let is_client_program p =
  List.exists (function
                 | Syntax.Define (_, _, `Client, _) -> true
                 | _ -> false) p

(* Hacky cache.  Should be neater, and moved somewhere else *)
let read_file_cache filename : (Syntax.expression list) = 
  Settings.set_value Performance.measuring false; (* temp *)
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
                ((Performance.measure "parse" Parse.parse_file) filename)))
      in 
	(try 
	   let outfile = open_out cachename in 
             Marshal.to_channel outfile program [Marshal.Closures] ;
             close_out outfile
	 with _ -> ());
        program
          
              
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
    | Syntax.Define (_, _, `Client, _) -> false
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

(* let handle_client_call unevaled_env f args =  *)
(*   let env = stubify_client_funcs unevaled_env in *)
(*   let f, args = Utility.base64decode f, Utility.base64decode args in *)
(*   let continuation = [Result.FuncApply (List.assoc f env, [])] in *)
(*   let result = (Interpreter.apply_cont_safe env continuation *)
(* 		  (untuple_single (parse_json args))) *)
(*   in *)
(*     print_http_response [("Content-type", "text/plain")] *)
(*       (Utility.base64encode (Json.jsonize_result result)); *)
(*     exit 0 *)

let get_remote_call_args env cgi_args = 
  let fname = Utility.base64decode (List.assoc "__name" cgi_args) in
  let args = Utility.base64decode (List.assoc "__args" cgi_args) in
  let args = untuple_single (parse_json args) in
    RemoteCall(List.assoc fname env, args)

open Errors
open Syntax (* needed for Parse_failure exception *)
(* really error handling should happen at a different level than
   the web interface *)
	
let decode_continuation (cont : string) : Result.continuation =
  let fixup_cont = 
  (* At some point, '+' gets replaced with ' ' in our base64-encoded
     string.  Here we put it back as it was. *)
    Str.global_replace (Str.regexp " ") "+" 
  in Marshal.from_string (Utility.base64decode (fixup_cont cont)) 0

let is_special_param (k, _) =
  List.mem k ["continuation%25"; "continuation%"; 
              "environment%25"; "environment%";
              "expression%25"; "expression%"]

let string_dict_to_charlist_dict =
  dict_map Result.string_as_charlist

let lookup_either a b env = 
  try List.assoc a env
  with Not_found -> List.assoc b env

exception Not_thunk

let undelay_expr = function
  | `Function (_, _, _, p) -> p
  | _ -> raise Not_thunk

let unpickle_expr_arg lookupf str = undelay_expr (deserialise_result_b64 lookupf str) 

(* Extract continuation from the parameters passed in over CGI.*)
let contin_invoke_req prim_lookup params =
  let pickled_continuation = (lookup_either "continuation%25" "continuation%" params) in
  let params = List.filter (not -<- is_special_param) params in
  let params = string_dict_to_charlist_dict params in
  let continuation =
    (deserialise_continuation prim_lookup
       (Utility.base64decode pickled_continuation))
  in
    ContInvoke(continuation, params)

(* Extract expression/environment pair from the parameters passed in over CGI.*)
let expr_eval_req program prim_lookup params =
  let pickled_expression = lookup_either "expression%25" "expression%" params in
  let pickled_environment = lookup_either "environment%25" "environment%"  params in
  let environment =
    if pickled_environment = "" then []
    else
      fst (deserialise_environment prim_lookup (Utility.base64decode pickled_environment))
  in 
  let expression = unpickle_expr_arg prim_lookup pickled_expression in
  let expression = resolve_placeholders_expr program expression in
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
  List.mem_assoc "continuation%" params

let is_expr_request params = 
  try 
    begin
      ignore(lookup_either "expression%" "expression%25" params);
      ignore(lookup_either "environment%" "environment%25" params);
      true
    end
  with Not_found -> false
      
        
let client_return_req env cgi_args = 
  let continuation = decode_continuation (List.assoc "__continuation" cgi_args) in
  let parse_json_b64 = parse_json -<- Utility.base64decode in
  let arg = parse_json_b64 (List.assoc "__result" cgi_args) in
    ClientReturn(continuation, untuple_single arg)

let perform_request program globals main req =
  match req with
    | ContInvoke (cont, params) -> 
        prerr_endline "Continuation";
        print_http_response [("Content-type", "text/html")]
          (Result.string_of_result 
             (Interpreter.apply_cont_safe globals cont (`Record params)))
    | ExprEval(expr, env) ->
        prerr_endline "expr/env pair";
        print_http_response [("Content-type", "text/html")]
          (Result.string_of_result 
             (snd (Interpreter.run_program (globals @ env) [expr])))
    | ClientReturn(cont, value) ->
        prerr_endline "client call return";
        print_http_response [("Content-type", "text/plain")]
          (Utility.base64encode 
             (Json.jsonize_result 
                (Interpreter.apply_cont_safe globals cont value)))
    | RemoteCall(func, arg) ->
        prerr_endline "server rpc call";
        let cont = [Result.FuncApply (func, [])] in
          print_http_response [("Content-type", "text/plain")]
            (Utility.base64encode
               (Json.jsonize_result 
                  (Interpreter.apply_cont_safe globals cont arg)))
    | CallMain -> 
        if is_client_program program then
          (prerr_endline "generating js";
           print_http_response [("Content-type", "text/html")]
             (Js.generate_program program main)
          )
        else (
          prerr_endline "running main";
          print_http_response [("Content-type", "text/html")]
            (Result.string_of_result (snd (Interpreter.run_program globals [main])))
        )
          
(*       let result = continue_from_client_call global_env cgi_args in *)
(*         (print_http_response [("Content-type", "text/plain")]  *)
(*            (\* FIXME: Why is this not JSON-encoded? *\) *)
(*            (Utility.base64encode (Result.string_of_result result)); *)
(*          exit 0) *)

let serve_requests filename = 
  Settings.set_value Performance.measuring true;
  Pervasives.flush(Pervasives.stderr);
  let program = read_file_cache filename in
  let global_env, [main] = List.partition is_define program in
  let global_env = stubify_client_funcs global_env in
  let cgi_args = Cgi.parse_args () in
  let request = 
    if is_remote_call cgi_args then 
      get_remote_call_args global_env cgi_args
    else if is_client_call_return cgi_args then
      client_return_req global_env cgi_args
    else if (is_contin_invocation cgi_args) then
      contin_invoke_req global_env cgi_args 
    else if (is_expr_request cgi_args) then
      expr_eval_req program (flip List.assoc global_env) cgi_args           
    else
      CallMain
  in
    perform_request program global_env main request
(*       let headers, result = perform_request program global_env main request *)
(*       in *)
(*         print_http_response headers *)
(*           (Result.string_of_result result) *)
          
let serve_requests filename =
  Errors.display_errors_fatal stderr
    serve_requests filename
