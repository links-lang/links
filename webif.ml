(*pp deriving *)

open Performance
open Utility
open Result

type query_params = (string * result) list deriving (Show)

type web_request = ContInvoke of continuation * query_params
                   | ExprEval of Syntax.expression * environment
                   | ClientReturn of continuation * result
                   | RemoteCall of string * result list
                   | GetIncludeFile of string
                   | CallMain
                       deriving (Show)

(** Does at least one of the functions have to run on the client? *)
(* Ultimately this should be phased out: any program should do as much
   as possible on the server and should send code to the client only
   when it's needed. *)
let rec is_client_program (Syntax.Program (defs, _) as program) =
  let is_client_def = function
    | Syntax.Define (_, _, `Client, _) -> true
    | Syntax.Module (_, Some defs, d) -> 
        is_client_program(Syntax.Program(defs, Syntax.unit_expression d))
    | _ -> false in
  let is_client_prim p = 
    try Library.primitive_location p = `Client
    with NotFound _ -> false in
  let toplevel_names = Syntax.defined_names defs in
  let freevars = StringSet.elements (Syntax.freevars_program program) in
  let prims = List.filter (not -<- flip List.mem toplevel_names) freevars
  in 
    List.exists is_client_def defs || List.exists is_client_prim prims

let with_prelude prelude (Syntax.Program (defs, body)) =
  Syntax.Program (prelude @ defs, body)

let read_and_optimise_program prelude env filename =
  let tenv, Syntax.Program(defs, body) = 
    Loader.load_file env ["formlets.links"] filename in
    tenv, Syntax.Program(defs, body)

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

let get_remote_call_args cgi_args = 
  let fname = Utility.base64decode (List.assoc "__name" cgi_args) in
  let args = Utility.base64decode (List.assoc "__args" cgi_args) in
  let args = untuple (Json.parse_json args) in
    RemoteCall(fname, args)

let is_includefile_request cgi_args = 
  List.mem_assoc "__include" cgi_args 

let get_includefile_args cgi_args = 
  let filename = List.assoc "__include" cgi_args in
    GetIncludeFile(filename)

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
    ContInvoke(unmarshal_continuation valenv program pickled_continuation,params)

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
          let json_args = try recfields(Json.parse_json_b64
                                          (List.assoc "_jsonArgs" params))
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
  "<style>pre {border : 1px solid #c66; padding: 4px; background-color: #fee} pre.typeError, code.typeError {display: block; padding:1em;}</style>"

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
    globals
    ((Syntax.Program(defs,main)) as program)
    req =
  let global_names = List.map fst globals in
  let prelude_types () =
    let prelude_types, _ =
      (Errors.display_fatal Loader.load_file Library.typing_env
         []
         (Settings.get_value Basicsettings.prelude_file))
    in
      Types.extend_env Library.typing_env prelude_types in
  match req with
    | ContInvoke (cont, params) ->
        Library.print_http_response [("Content-type", "text/html")]
          (Result.string_of_result 
             (Interpreter.apply_cont_safe globals cont (`Record params)))
    | ExprEval(expr, locals) ->
        let undefd_vars = (Syntax.freevars expr)
          <|StringSet.diff|>
              (StringSet.union
                 (StringSet.from_list (Syntax.defined_names defs @ 
                                         dom globals @ dom locals))
                 (Env.String.domain (fst Library.typing_env))) in
          if not(StringSet.is_empty undefd_vars) then
            (Debug.print("Undefined variables:");
             Debug.print(String.concat "," (StringSet.elements undefd_vars)));
        let pos = Syntax.position expr in
        let data s =
            `T (pos, fst (Parse.parse_string Parse.datatype s), None) in

        (* This assertion failing indicates that not everything needed
           was serialized into the link: *)
        assert(Syntax.expr_closed_wrt expr
                 (StringSet.union
                    (StringSet.from_list (Syntax.defined_names defs @ dom globals
                                          @ dom locals))
                    (Env.String.domain (fst Library.typing_env))));
          (* Note: following is the only place where run_program is
             invoked with other than an empty local environment; also
             here we artificially construct a Syntax.program with no
             definition part. Factor this differently? *)
          Library.print_http_response [("Content-type", "text/html")]
            (Result.string_of_result 
               (snd (Interpreter.run_program globals locals
                       (Syntax.Program
                          ([],Syntax.Apply(Syntax.Variable("renderPage",
                                                           data "(Page) -> Xml"),
                                           [expr], data "Xml"))))))

    | ClientReturn(cont, value) ->
        Interpreter.has_client_context := true;
        let result = Interpreter.apply_cont_safe globals cont value in
        let result_encoded = Utility.base64encode(Json.jsonize_result result) in
          Library.print_http_response [("Content-type", "text/plain")]
            result_encoded

    | RemoteCall(fname, args) ->
        Interpreter.has_client_context := true;
        let args = List.rev args in
        let _, result = Interpreter.run_expr globals Result.empty_env
          (Syntax.Variable(fname, Syntax.no_expr_data))
          (ApplyCont(Result.empty_env, args) :: toplevel_cont)
        in
          Library.print_http_response [("Content-type", "text/plain")]
             (Utility.base64encode (Json.jsonize_result result))

    | GetIncludeFile filename ->
        (* load the prelude for type-checking purposes: *)
        let tyenv =
          if filename = "prelude.links" then
            Library.typing_env 
          else 
            prelude_types ()
        in
          Library.print_http_response[("Content-type", "application/javascript")]
            (if Settings.get_value (Basicsettings.use_monadic_ir) then
               Irtojs.compile_file tyenv filename
             else
               Js.compile_file tyenv filename)

    | CallMain -> 
        Library.print_http_response [("Content-type", "text/html")] 
          (if is_client_program program then
             if Settings.get_value (Basicsettings.use_monadic_ir) then
               Irtojs.generate_program_page (prelude_types ()) global_names program
             else
               Js.generate_program global_names program
           else 
             let _env, rslt = Interpreter.run_program globals []
                                (Syntax.Program (defs, main)) in
               Result.string_of_result rslt)

let serve_request prelude (valenv, typenv) filename = 
  try 
    let _, (Syntax.Program (defs, main) as program) =
      read_and_optimise_program prelude typenv filename in
    let defs_env = Interpreter.run_defs valenv Result.empty_env defs in
    let valenv = valenv @ defs_env in
    Interpreter.program_source := program; (* used to implement client calls. *)
    let cgi_args =
      if is_multipart () then
        List.map (fun (name, {Cgi.value=value}) ->
                    (name, value)) (Cgi.parse_multipart_args ())
      else
        Cgi.parse_args () in
      Library.cgi_parameters := cgi_args;
    let path_info = Cgi.path_info in
    let request = 
      if is_remote_call cgi_args then
        get_remote_call_args cgi_args
      else if is_client_call_return cgi_args then
        client_return_req cgi_args
      else if (is_contin_invocation cgi_args) then
        contin_invoke_req (rng valenv) program cgi_args
      else if (is_expr_request cgi_args) then
        expr_eval_req valenv program cgi_args
      else if is_includefile_request cgi_args then
        get_includefile_args cgi_args
      else
        CallMain
    in
      perform_request valenv program request
  with
      (* FIXME: errors need to be handled differently
         btwn. user-facing and remote-call modes. *)
      Failure msg -> prerr_endline msg;
        Library.print_http_response [("Content-type", "text/html; charset=utf-8")] 
        (error_page (Errors.format_exception_html (Failure msg)))
    | exc -> 
        prerr_endline("exiting web mode with HTML page; error: " ^
                        Errors.format_exception exc);
        Library.print_http_response [("Content-type", "text/html; charset=utf-8")]
        (error_page (Errors.format_exception_html exc))

let serve_request prelude envs filename =
  Errors.display (lazy (serve_request prelude envs filename))
