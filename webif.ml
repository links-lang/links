open Unix
open Sl_utility

(* Does at least one of the functions have to run on the client? *)
let is_client_program p =
  List.exists (function
                 | Sl_syntax.Define (_, _, `Client, _) -> true
                 | _ -> false) p

(* Hacky cache.  Should be neater, and moved somewhere else *)
let read_file_cache filename : (Sl_syntax.expression list) = 
  Performance.measuring := false; (* temp *)
  let cachename = filename ^ ".cache" in
    try
      if ((Unix.stat cachename).st_mtime > (Unix.stat filename).st_mtime) then
        let infile = open_in cachename in
        let program = Marshal.from_channel infile in
          close_in infile;
          program
      else
        raise (Sys_error "booyah")
    with (Sys_error _| Unix.Unix_error _) ->
      let program = 
        (Performance.measure "optimise" Sl_optimiser.optimise_program)
          ((Performance.measure "type" (Sl_inference.type_program Sl_library.type_env))
             ((Performance.measure "parse" Parse.parse_file) filename)) in 
	(try 
	   let outfile = open_out cachename in 
             Marshal.to_channel outfile program [Marshal.Closures] ;
             close_out outfile
	 with _ -> ());
        program


let encode_continuation (cont : Sl_result.continuation) : string =
  Netencoding.Base64.encode (Marshal.to_string cont [Marshal.Closures])

let serialize_call_to_client (continuation, name, arg) =
  Json.jsonize_result
    (`Record [
       "__continuation", Sl_result.string_as_charlist (encode_continuation continuation);
       "__name", Sl_result.string_as_charlist name;
       "__arg", arg
     ])

let untuple_single = function
  | `Record ["1",arg] -> arg
  | r -> r

let clientize_unevaled_env env = 
  let is_server_fun = (function
			 | Sl_syntax.Define (_, _, (`Server|`Unknown), _) -> true
			 | Sl_syntax.Define (_, _, `Client, _) -> false
			 | e  -> failwith ("Unexpected non-definition in environment : " 
					   ^ Sl_syntax.string_of_expression e)) 
  and def_as_client_fun = function 
    | Sl_syntax.Define (name, _, _, _) -> 
      (name,
       `Primitive
	 (`PFunction
	    (name, 
             Some (fun (_, cont, arg) -> 
                     let call = serialize_call_to_client (cont, name, arg) in
                       (print_endline ("Content-type: text/plain\n\n" ^ Netencoding.Base64.encode call);
                        exit 0)),
             []))) in
  let server_env, client_env = List.partition is_server_fun env in
  let client_env = List.map def_as_client_fun client_env in
    (fst ((Sl_interpreter.run_program Sl_library.value_env) server_env)) @ client_env

let handle_client_call unevaled_env f args = 
  let env = clientize_unevaled_env unevaled_env in
  let f, args = Netencoding.Base64.decode f, Netencoding.Base64.decode args in
    let continuation = [Sl_result.FuncApply (List.assoc f env, [])] in
      prerr_endline("A connection was accepted in handle_client_call");
    let result = (Sl_interpreter.apply_cont_safe env continuation
		    (untuple_single (Jsonparse.parse_json 
				       Jsonlex.jsonlex (Lexing.from_string args)))) in
      print_string ("Content-type: text/plain\n\n" ^ Netencoding.Base64.encode (Json.jsonize_result result));
      exit 0

open Sl_errors
open Sl_syntax (* needed for Parse_failure exception *)
(* really error handling should happen at a different level than
   the web interface *)
	
let decode_continuation (cont : string) : Sl_result.continuation =
  let fixup_cont = 
  (* At some point, '+' gets replaced with ' ' in our base64-encoded
     string.  Here we put it back as it was. *)
    Str.global_replace (Str.regexp " ") "+" 
  in Marshal.from_string (Netencoding.Base64.decode (fixup_cont cont)) 0

let serve_requests filename = 
  try
  Performance.measuring := true;
    prerr_endline("A connection was accepted in serve_requests");
    Pervasives.flush(Pervasives.stderr);
  let global_env = read_file_cache filename in
    (* TBD: Allow multiple expressions; execute them all in turn. *)
  let global_env, [expression] = List.partition (function Sl_syntax.Define _ -> true | _ -> false) global_env in
  let cgi_args = Cgi.parse_args () in
    if Sl_forms.is_remote_call cgi_args then 
      handle_client_call
        global_env
        (List.assoc "__name" cgi_args) 
        (List.assoc "__args" cgi_args)
    else if List.mem_assoc "__continuation" cgi_args then
      begin
        let parse_json = Jsonparse.parse_json Jsonlex.jsonlex -<- Lexing.from_string in
        let continuation = decode_continuation  (List.assoc "__continuation" cgi_args) 
        and arg = parse_json (Netencoding.Base64.decode (List.assoc "__result" cgi_args))
        and env = clientize_unevaled_env global_env in
          debug("continuation is " ^ Sl_syntax.string_of_expression expression);
        let result = Sl_interpreter.apply_cont_safe env continuation (untuple_single arg) in
          print_endline ("Content-type: text/plain\n\n" ^ Netencoding.Base64.encode (Sl_result.string_of_result result));
          exit 0
      end
    else 

      (* Print headers *)
    print_endline "Content-type: text/html\n";

    if true (* is_client_program global_env *)
    then
       print_endline (Sl_js.generate_program filename global_env expression)
    else
      let global_env, _ = (Sl_interpreter.run_program Sl_library.value_env) global_env in
      begin
	(* Are we being called via a continuation? *)
	match Sl_forms.cont_from_params (flip List.assoc global_env) cgi_args  with
	  | Some (Sl_forms.ContParams (cont, params)) -> 
	      debug("in env:" ^ Sl_result.string_of_environment_ez global_env);
	      print_endline
		(Sl_result.string_of_result (Sl_interpreter.apply_cont_safe global_env cont (`Record params)))
	  | Some (Sl_forms.ExprEnv(expr, env)) ->
	      print_endline (Sl_result.string_of_result (snd (Sl_interpreter.run_program (global_env @ env) [expr])))
	  | None -> 
              debug("parsed program is " ^ Sl_syntax.string_of_expression expression);
	      print_endline (Sl_result.string_of_result (snd (Sl_interpreter.run_program global_env [expression])))
      end
  with
    | Type_error (x, y) -> Sl_errors.display_error (Type_error (x, y)) ("Why does display_error want to be passed the line to print?")

