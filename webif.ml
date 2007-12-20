(*pp deriving *)

open Performance
open Utility
open Result

let correct_optimised_types = Settings.add_bool ("correct_optimised_types", true, `User)

(*
 Whether to cache programs after the optimization phase
*)
let cache_programs = Settings.add_bool ("cache_programs", false, `User)

type query_params = (string * result) list deriving (Show)

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
    try Library.primitive_location p = `Client
    with NotFound _ -> false in
  let freevars = StringSet.elements (Syntax.freevars_program program) in
  let prims = List.filter (not -<- flip List.mem toplevels) freevars
  in 
    List.exists is_client_def defs || List.exists is_client_prim prims

let with_prelude prelude (Syntax.Program (defs, body)) =
  Syntax.Program (prelude @ defs, body)

(* Read in and optimise the program *)
let read_and_optimise_program prelude typenv filename = 
  let sugar, pos_context = measure "parse" (Parse.parse_file ~pp:(Settings.get_value Basicsettings.pp) Parse.program) filename in
  let resolve = Parse.retrieve_code pos_context in
  let (bindings, expr), _, _ = Frontend.Pipeline.program Library.typing_env resolve sugar in
  let defs = Sugar.desugar_definitions resolve bindings in
  let expr = opt_map (Sugar.desugar_expression resolve) expr in
  let program = Syntax.Program (defs, from_option (Syntax.unit_expression (`U Syntax.dummy_position)) expr) in
  let tenv, program = measure "type" (Inference.type_program typenv) program in
  let tenv, program = 
    (* The prelude is already optimized (via loader.ml) so we don't run 
       it through again. *)
    tenv, with_prelude prelude (lazy((Optimiser.optimise_program(tenv, program)))
       <|measure_as|> "optimise") in
  let tenv, program =
    if Settings.get_value (correct_optimised_types) then
      lazy(Inference.type_program Library.typing_env (Syntax.erase program))
        <|measure_as|> "type again"
    else
      tenv, program
  in
    tenv, Syntax.labelize program

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
                     (name, `ClientFunction name))
      client_defs in
    match server_defs with 
        [] -> []
      | server_defs -> (* evaluate the definitions to get Result.result values. *)
          Interpreter.run_defs globals [] server_defs
            @ client_env

let get_remote_call_args lookup cgi_args = 
  let fname = Utility.base64decode (List.assoc "__name" cgi_args) in
  let args = Utility.base64decode (List.assoc "__args" cgi_args) in
  let args = untuple (Json.parse_json args) in
  let func = lookup fname in
    RemoteCall(func, args)

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

(* Extract continuation from the parameters passed in over CGI.*)
let contin_invoke_req valenv program params =
  let pickled_continuation = List.assoc "_cont" params in
  let params = List.filter (not -<- is_special_param) params in
  let params = string_dict_to_charlist_dict params in
    (* TBD: create a debug setting for printing webif modes. *)
(*     Debug.print("Invoking " ^ string_of_cont(unmarshal_continuation valenv program pickled_continuation)); *)
    ContInvoke(unmarshal_continuation valenv program pickled_continuation, params)

(* Extract expression/environment pair from the parameters passed in over CGI.*)
let expr_eval_req valenv program params =
  let data = Syntax.no_expr_data in
  let mkStringPair (l, r) =
    Syntax.Record_intro (StringMap.from_alist 
                           [("1", Syntax.Constant (Syntax.String l, data));
                            ("2", Syntax.Constant (Syntax.String r, data))], 
                         None, data) in
    match Result.unmarshal_result (rng valenv) program  (List.assoc "_k" params) with
      | `RecFunction ([(_,f)],locals,_) ->
          let json_args = try (match Json.parse_json_b64 (List.assoc "_jsonArgs" params) with
                                 | `Record fields -> fields
                                 | _ -> assert false) 
          with NotFound _ -> [] in
          let env = List.filter (not -<- is_special_param) params in
          let env = (List.fold_right
                       (fun pair env -> 
                          Syntax.Concat (Syntax.List_of(mkStringPair pair, data),
                                         env, data))
                       env (Syntax.Nil data))
          in ExprEval (Syntax.Apply (f, [env], data), locals @ json_args)
      | _ -> assert false

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
  let arg = Json.parse_json_b64 (List.assoc "__result" cgi_args) in
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
  match req with
    | ContInvoke (cont, params) ->
        Library.print_http_response [("Content-type", "text/html")]
          (Result.string_of_result 
             (Interpreter.apply_cont_safe globals cont (`Record params)))
    | ExprEval(expr, env) ->
        let pos = Syntax.position expr in
        let data s =
            `T (pos, DesugarDatatype.read_datatype s, None) in

        (* This assertion failing indicates that not everything needed
           was serialized into the link: *)
        assert(Syntax.expr_closed_wrt expr 
                 (StringSet.union
                    (StringSet.from_list (dom globals @ dom env))
                    (Env.String.domain (fst Library.typing_env))));
        Library.print_http_response [("Content-type", "text/html")]
          (Result.string_of_result 
             (snd (Interpreter.run_program globals env
                     (Syntax.Program
                        ([],
                         Syntax.Apply (Syntax.Variable ("renderPage", data "(Page) -> Xml"), [expr], data "Xml"))))))
    | ClientReturn(cont, value) ->
        Interpreter.has_client_context := true;
        let result_json = (Json.jsonize_result 
                             (Interpreter.apply_cont_safe globals cont value)) in
        Library.print_http_response [("Content-type", "text/plain")]
          (Utility.base64encode 
             result_json)
    | RemoteCall(func, args) ->
        Interpreter.has_client_context := true;
        let args = List.rev args in
        let cont, value = 
          ApplyCont(Result.empty_env, args) :: toplevel_cont, func in
        let result = Interpreter.apply_cont_safe globals cont value in
	  Library.print_http_response [("Content-type", "text/plain")]
            (Utility.base64encode (Json.jsonize_result result))
    | CallMain -> 
        Library.print_http_response [("Content-type", "text/html")] 
          (if is_client_program program then             
             if Settings.get_value (Basicsettings.use_monadic_ir) then
               Irtojs.generate_program_page Library.typing_env (List.map fst globals) program
             else
               Js.generate_program (List.map fst globals) program
           else 
             let _env, rslt = Interpreter.run_program globals [] (Syntax.Program ([], main)) in
               Result.string_of_result rslt)

let serve_request prelude (valenv, typenv) filename = 
  try 
    let _, (Syntax.Program (defs, main) as program) =
      read_and_optimise_program prelude typenv filename in
    let defs = stubify_client_funcs valenv program in
    let cgi_args =
      if is_multipart () then
        List.map (fun (name, {Cgi.value=value}) ->
                    (name, value)) (Cgi.parse_multipart_args ())
      else
        Cgi.parse_args () in
      Library.cgi_parameters := cgi_args;
    let lookup name = 
      try List.assoc name defs
      with NotFound _ -> Library.primitive_stub name
        | _ -> failwith("Internal error: called unknown server function " ^ name)
    in
    let request = 
      if is_remote_call cgi_args then
        get_remote_call_args lookup cgi_args
      else if is_client_call_return cgi_args then
        client_return_req cgi_args
      else if (is_contin_invocation cgi_args) then
        contin_invoke_req (rng valenv) program cgi_args
      else if (is_expr_request cgi_args) then
        expr_eval_req valenv program cgi_args
      else
        CallMain
    in
      perform_request program (defs @ valenv) main request
  with
      (* FIXME: errors need to be handled differently
         btwn. user-facing and remote-call modes. *)
      Failure msg -> prerr_endline msg;
        Library.print_http_response [("Content-type", "text/html; charset=utf-8")] 
        (error_page (Errors.format_exception_html (Failure msg)))
    | exc -> Library.print_http_response [("Content-type", "text/html; charset=utf-8")]
        (error_page (Errors.format_exception_html exc))
          
let serve_request prelude envs filename =
  Errors.display (lazy (serve_request prelude envs filename))
