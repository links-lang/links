(*pp deriving *)

open Notfound

open Performance
open Utility

type query_params = (string * Value.t) list deriving (Show)

type web_request =
  | ContApply of Value.continuation * query_params
  | ExprEval of Ir.tail_computation * Value.env
  | ClientReturn of Value.continuation * Value.t
  | RemoteCall of Value.t * Value.t list
  | EvalMain
      deriving (Show)

(** Does at least one of the functions have to run on the client? *)
let is_client_program : Ir.program -> bool =
  fun (bs, main) ->
    List.exists
      (function
         | `Fun (_, _, `Client)
         | `Alien (_, "javascript") -> true
         | `Rec defs ->
             List.exists
               (fun (_, _, location) -> location = `Client)
               defs
         | _ -> false)
      bs

let serialize_call_to_client (continuation, name, arg) = 
  Json.jsonize_call continuation name arg

let parse_remote_call lookup cgi_args = 
  let fname = Utility.base64decode (List.assoc "__name" cgi_args) in
  let args = Utility.base64decode (List.assoc "__args" cgi_args) in
  let args = Value.untuple (Json.parse_json args) in
  let func = lookup fname in
    Debug.print ("client --> server call: "^fname);
    RemoteCall(func, args)

let decode_continuation (cont : string) : Value.continuation =
  let fixup_cont = 
  (* At some point, '+' gets replaced with ' ' in our base64-encoded
     string.  Here we put it back as it was. *)
    Str.global_replace (Str.regexp " ") "+" 
  in Marshal.from_string (Utility.base64decode (fixup_cont cont)) 0

let make_unmarshal_envs (valenv, nenv, tyenv) program = 
  let tenv = Var.varify_env (nenv, tyenv.Types.var_env) in
  let closures = Ir.ClosureTable.program tenv Lib.primitive_vars program in
  let valenv = Value.with_closures valenv closures in
  let unmarshal_envs= Value.build_unmarshal_envs(valenv, nenv, tyenv) program in
    closures, unmarshal_envs

(** NOTE: The invocation mode [ContApply] is used by the
    [freshResource] function defined in the prelude, which creates an
    explicit link using a [_cont] parameter.
*)

let is_cont_apply_param (key, _) = key == "_cont"

(** Extract continuation from the parameters passed in over CGI.*)
let parse_cont_apply (valenv, nenv, tyenv) program params =
  Debug.print ("Invoking a server continuation");
  let pickled_continuation = List.assoc "_cont" params in
  let params = List.filter (not -<- is_cont_apply_param) params in
  let params = alistmap Value.string_as_charlist params in
  let _, unmarshal_envs = make_unmarshal_envs (valenv, nenv, tyenv) program in
    (* TBD: create a debug setting for printing webif modes. *)
      ContApply (Value.unmarshal_continuation unmarshal_envs pickled_continuation, params)

(** Extract expression/environment pair from the CGI parameters.*)
let parse_expr_eval (valenv, nenv, tyenv) program params =
  Debug.print ("eval expression request");
  let string_pair (l, r) =
    `Extend
      (StringMap.from_alist [("1", `Constant (`String l));
                             ("2", `Constant (`String r))],
       None) in
  let closures, unmarshal_envs = make_unmarshal_envs (valenv, nenv, tyenv) 
    program in
    (* FIXME: "_k" is a misnomer; it should be "_expr" *)
    match Value.unmarshal_value unmarshal_envs (List.assoc "_k" params) with
        | `RecFunction ([(f, (_xs, _body))], locals, _, _) as v ->
          let json_env =
            if List.mem_assoc "_jsonArgs" params then
              match Json.parse_json_b64 (List.assoc "_jsonArgs" params) with
                | `Record fields ->
                       List.fold_left
                         (fun env (name, v) ->
                            Value.bind (int_of_string name) (v, `Local) env)
                         (Value.empty_env closures)
                         fields
                | _ -> assert false
            else
              Value.empty_env closures in

          (* we don't need to pass the args in here as they are read using the environment
             function *)

          (*           let params = List.filter (not -<- is_cont_apply_param) params in *)            
          (*           let args = *)
          (*             List.fold_right *)
          (*               (fun pair env -> *)
          (*                  `ApplyPure (`Variable (Env.String.lookup nenv "Cons"), [string_pair pair; env])) *)
          (*               params *)
          (*               (`Variable (Env.String.lookup nenv "Nil")) in *)


          let env = Value.shadow (Value.bind f (v, `Local) locals) ~by:json_env in
            ExprEval (`Apply (`Variable f, []), env)
      | _ -> assert false

let is_remote_call params =
  List.mem_assoc "__name" params && List.mem_assoc "__args" params

let is_client_return params = 
  List.mem_assoc "__continuation" params && List.mem_assoc "__result" params

let is_cont_apply params = 
  List.mem_assoc "_cont" params

let is_expr_eval args =
  List.mem_assoc "_k" args
        
let parse_client_return cgi_args = 
  let continuation = decode_continuation (List.assoc "__continuation" cgi_args) in
  let arg = Json.parse_json_b64 (List.assoc "__result" cgi_args) in
    ClientReturn(continuation, arg)

let error_page_stylesheet = 
  "<style>pre {border : 1px solid #c66; padding: 4px; background-color: #fee} code.typeError {display: block; padding:1em;}</style>"

let error_page body = 
  "<html>\n  <head>\n    <title>Links error</title>\n    " ^ 
    error_page_stylesheet ^ 
    "\n  </head>\n  <body>" ^ 
    body ^ 
    "\n  </body></html>\n"

let is_multipart () =
  ((Cgi.safe_getenv "REQUEST_METHOD") = "POST" &&
      Cgi.string_starts_with (Cgi.safe_getenv "CONTENT_TYPE") "multipart/form-data")

let get_cgi_args() =
  if is_multipart() then
    List.map (fun (name, {Cgi.value=value}) -> (name, value))
      (Cgi.parse_multipart_args())
  else
    Cgi.parse_args()

(** In web mode, we wrap the continuation of the whole program in a
    call to renderPage. We also return the resulting continuation so
    that we can use it elsewhere (i.e. in processing ExprEval).
*)
let wrap_with_render_page (nenv, {Types.tycon_env=tycon_env; Types.var_env=_})
                          (bs, body) =
  let xb, x = Var.fresh_var_of_type (Instantiate.alias "Page" [] tycon_env) in
  let tail = Ir.var_appln nenv "renderPage" [`Variable x] in
  let cont = fun env -> [(`Local, x, env, ([], tail))] in
    (bs @ [`Let (xb, ([], body))], tail), cont

let perform_request (valenv, nenv, tyenv) (globals, (locals, main)) cont =
  function
    | ContApply(cont, params) ->
        ("text/html",
         (Value.string_of_value
            (Evalir.apply_cont_toplevel cont valenv (`Record params))))
    | ExprEval(expr, locals) ->        
        let env = Value.shadow valenv ~by:locals in
        let v = snd (Evalir.run_program_with_cont 
                       (cont (Value.empty_env (Value.get_closures env)))
                       env ([], expr)) in
          ("text/html",
           Value.string_of_value v)
    | ClientReturn(cont, arg) ->
        let result = Evalir.apply_cont_toplevel cont valenv arg in
        let result_json = Json.jsonize_value result in
          ("text/plain",
           Utility.base64encode result_json)
    | RemoteCall(func, args) -> 
        let result = Evalir.apply_toplevel valenv (func, args) in
          if not(Proc.singlethreaded()) then
            (prerr_endline "Remaining procs on server after remote call!";
             assert(false));
          ("text/plain",
           Utility.base64encode (Json.jsonize_value result))
    | EvalMain -> 
        ("text/html",
         if is_client_program (globals @ locals, main) then
           let program = (globals @ locals, main) in
             Debug.print "Running client program.";
             let tenv = Var.varify_env (nenv, tyenv.Types.var_env) in
             let closures = Ir.ClosureTable.program tenv Lib.primitive_vars program in
               Irtojs.generate_program_page
                 (closures, Lib.nenv, Lib.typing_env) 
                 program
         else
           let program = locals, main in (* wrap_with_render_page (nenv, tyenv) (locals, main) in*)
             Debug.print "Running server program";
             let tenv = Var.varify_env (nenv, tyenv.Types.var_env) in
             let closures = Ir.ClosureTable.program tenv Lib.primitive_vars 
               (globals @ (fst program), snd program) in
             let valenv = Value.with_closures valenv (closures) in
             let _env, v = Evalir.run_program valenv program in
               Value.string_of_value v)

let serve_request (valenv, nenv, (tyenv : Types.typing_environment)) 
                  prelude filename =
  try 
    let (nenv', tyenv'), (globals, (locals, main), t) =
      Errors.display_fatal Loader.load_file (nenv, tyenv) filename in

    begin try
      Unify.datatypes (t, Instantiate.alias "Page" [] tyenv.Types.tycon_env)
    with
        Unify.Failure error ->
          begin match error with
            | `Msg s -> Debug.print ("Unification error: " ^ s)
            | _ -> ()
          end;
          failwith("Web programs must have type Page but this program has type "
                   ^ Types.string_of_datatype t)
    end;

    let (locals, main), render_cont = 
      wrap_with_render_page (nenv, tyenv) (locals, main) in
    let closures = Ir.ClosureTable.program
      (Var.varify_env (nenv, tyenv.Types.var_env)) 
      Lib.primitive_vars
      (globals @ locals, main) in

      (* FIXME: why is this evaluating the definitions? *)
    let valenv = Evalir.run_defs(Value.with_closures valenv closures) globals in

    let valenv, nenv, tyenv  =
      (valenv,
       Env.String.extend nenv nenv',
       Types.extend_typing_environment tyenv tyenv') in
    let globals = prelude @ globals in
    let cgi_args = get_cgi_args() in
      Lib.cgi_parameters := cgi_args;
      let lookup name =
        let var = Env.String.lookup nenv name in
          match Value.lookup var valenv with
            | Some v -> v
            | None -> Lib.primitive_stub name in
      let request =
        if is_remote_call cgi_args then
          parse_remote_call lookup cgi_args
        else if is_client_return cgi_args then
          parse_client_return cgi_args
        else if (is_cont_apply cgi_args) then
          parse_cont_apply (valenv, nenv, tyenv) (globals@locals, main) cgi_args
        else if (is_expr_eval cgi_args) then
          parse_expr_eval (valenv, nenv, tyenv) (globals@locals, main) cgi_args
        else
          EvalMain
      in
      let (content_type, content) =
        perform_request (valenv, nenv, tyenv) (globals, (locals, main)) 
          render_cont request
      in
        Lib.print_http_response [("Content-type", content_type)] content
  with
      (* FIXME: errors need to be handled differently between
         user-facing (text/html) and remote-call (text/plain) modes. *)
      Failure msg as e -> 
        prerr_endline msg;
        Lib.print_http_response [("Content-type", "text/html; charset=utf-8")] 
          (error_page (Errors.format_exception_html e))
    | exc -> Lib.print_http_response [("Content-type", "text/html; charset=utf-8")]
        (error_page (Errors.format_exception_html exc))
          
let serve_request envs prelude filename =
  Errors.display (lazy (serve_request envs prelude filename))
