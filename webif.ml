(*pp deriving *)

open Notfound
open List
open Proc

open Performance
open Utility

let realpages = Settings.add_bool ("realpages", false, `System)

type web_request =
  | ServerCont of
      Value.t                (* thunk *)
  | ClientReturn of
      Value.continuation *   (* continuation *)
      Value.t                (* argument *)
  | RemoteCall of
      Value.t *              (* function *)
      Value.env *            (* closure environment *)
      Value.t list           (* arguments *)
  | EvalMain of
      Ir.binding list *
      Ir.program
      deriving (Show)

(** Does at least one of the functions have to run on the client? *)
let is_client_program : Ir.program -> bool =
  fun (bs, main) ->
    exists
      (function
         | `Fun (_, _, _, `Client)
         | `Alien (_, "javascript") -> true
         | `Rec defs ->
             exists
               (fun (_, _, _, location) -> location = `Client)
               defs
         | _ -> false)
      bs

(* SL: dead code *)

(* let serialize_call_to_client (continuation, name, arg) = *)
(*   Json.jsonize_call continuation name arg *)

let parse_remote_call (valenv, nenv, tyenv) cgi_args =
  let fname = Utility.base64decode (assoc "__name" cgi_args) in
  let args = Utility.base64decode (assoc "__args" cgi_args) in
  (* Debug.print ("args: " ^ Value.Show_t.show (Json.parse_json args)); *)
  let args = Value.untuple (Json.parse_json args) in

  let fvs = Json.parse_json_b64 (assoc "__env" cgi_args) in

  let func =
    match fvs with
    | `Record [] -> `FunctionPtr (int_of_string fname, None)
    | _          -> `FunctionPtr (int_of_string fname, Some fvs) in
  RemoteCall(func, valenv, args)

(** Boolean tests for cgi parameters *)

(** remote client->server call *)
let is_remote_call params =
  mem_assoc "__name" params && mem_assoc "__args" params

(** return __result from server->client call with server continuation __continuation *)
let is_client_return params =
  mem_assoc "__continuation" params && mem_assoc "__result" params

(** invoke server continuation _k
    (e.g. from a hypertext link or a formlet post)
 *)
let is_server_cont args =
  mem_assoc "_k" args

(** Extract continuation thunk from the CGI parameter _k *)
let parse_server_cont (valenv, _, _) program params =
  ServerCont (Value.unmarshal_value valenv (assoc "_k" params))

let parse_client_return (valenv, _, _) program cgi_args =
  let fixup_cont =
    (* At some point, '+' gets replaced with ' ' in our base64-encoded
       string. Here we put it back as it was. *)
    Str.global_replace (Str.regexp " ") "+"
  in
  let cont =
    Value.unmarshal_continuation
      valenv
      (fixup_cont (assoc "__continuation" cgi_args))
  in
  (* Debug.print("continuation: " ^ Value.Show_continuation.show continuation); *)
  let arg = Json.parse_json_b64 (assoc "__result" cgi_args) in
  (* Debug.print ("arg: "^Value.Show_t.show arg); *)
    ClientReturn(cont, arg)

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
    map (fun (name, {Cgi.value=value}) -> (name, value))
      (Cgi.parse_multipart_args())
  else
    Cgi.parse_args()

(* jcheney: lifted from serve_request, to de-clutter *)
let parse_request env (globals, (locals, main)) cgi_args  =
  if      (is_remote_call cgi_args)
  then parse_remote_call env cgi_args
  else if (is_client_return cgi_args)
  then parse_client_return env (globals@locals, main) cgi_args
  else if (is_server_cont cgi_args)
  then parse_server_cont env (globals@locals, main) cgi_args
  else EvalMain (globals, (locals, main))

(** In web mode, we wrap the continuation of the whole program in a
    call to renderPage. We also return the resulting continuation so
    that we can use it elsewhere (i.e. in processing ServerCont).
*)
let wrap_with_render_page (nenv, {Types.tycon_env=tycon_env; Types.var_env=_})
                          (bs, body) =
  let xb, x = Var.fresh_global_var_of_type (Instantiate.alias "Page" [] tycon_env) in
  let render_page = Env.String.lookup nenv "renderPage" in
  let tail = `Apply (`Variable render_page, [`Variable x]) in
  let cont = [(`Global, x, Value.empty_env, ([], tail))] in
    (bs @ [`Let (xb, ([], body))], tail), cont

let perform_request cgi_args (valenv, nenv, tyenv) render_cont =
  function
    | ServerCont t ->
      Debug.print("Doing ServerCont");
      let v = Evalir.apply_with_cont render_cont valenv (t, []) in
      ("text/html",
       Value.string_of_value v)
    | ClientReturn(cont, arg) ->
      Debug.print("Doing ClientReturn ");
      let result = Evalir.apply_cont_toplevel cont valenv arg in
      let result_json = Json.jsonize_value result in
      ("text/plain",
       Utility.base64encode result_json)
    | RemoteCall(func, env, args) ->
      Debug.print("Doing RemoteCall for " ^ Value.string_of_value func);
      (* Debug.print ("func: " ^ Value.Show_t.show func); *)
      (* Debug.print ("args: " ^ mapstrcat ", " Value.Show_t.show args); *)
      let result = Evalir.apply_toplevel env (func, args) in
      (* Debug.print ("result: "^Value.Show_t.show result); *)
      if not(Proc.singlethreaded()) then
        (prerr_endline "Remaining procs on server after remote call!";
         assert(false));
      ("text/plain",
       (* TODO: we should package up the result with event handlers,
          client processes, and client messages *)
       Utility.base64encode (Json.jsonize_value result))
    | EvalMain (globals, (locals, main))->
        Debug.print("Doing EvalMain");
        ("text/html",
         if is_client_program (globals @ locals, main) then
           if Settings.get_value realpages then
             begin
               Debug.print "Running client program from server";
               let valenv, v = Evalir.run_program valenv (locals, main) in
  (* Debug.print ("valenv" ^ Value.Show_env.show valenv); *)
               Irtojs.generate_real_client_page
                 ~cgi_env:cgi_args
                 (Lib.nenv, Lib.typing_env)
                 (globals @ locals)
                 (valenv, v)
             end
           else
             let program = (globals @ locals, main) in
             Debug.print "Running client program.";
             lazy (Irtojs.generate_program_page
                     ~cgi_env:cgi_args
                     (Lib.nenv, Lib.typing_env)
                     program)
             <|measure_as|> "irtojs"
         else
           let program = locals, main in
           Debug.print "Running server program";
           let _env, v = Evalir.run_program valenv program in
           Value.string_of_value v)

let serve_request_program env (globals, (locals, main), render_cont) cgi_args =
  try
    let request = parse_request env (globals, (locals, main)) cgi_args in
    let (content_type, content) =
      perform_request cgi_args env render_cont request
    in
    Lib.print_http_response [("Content-type", content_type)] content
  with
      (* FIXME: errors need to be handled differently between
         user-facing (text/html) and remote-call (text/plain) modes. *)
    Failure msg as e ->
    prerr_endline msg;
    Lib.print_http_response [("Content-type", "text/html; charset=utf-8")]
      (error_page (Errors.format_exception_html e))
  | exc -> Lib.print_http_response[("Content-type","text/html; charset=utf-8")]
             (error_page (Errors.format_exception_html exc))


(* does the preprocessing to turn prelude+filename into a program *)
(* result can be cached *)

let make_program (_, nenv, tyenv) prelude filename =
  (* Warning: cache call nested inside another cache call *)
  let (nenv', tyenv'), (globals, (locals, main), t) =
    Errors.display_fatal (Loader.load_file (nenv, tyenv)) filename
  in

  begin
    try
      Unify.datatypes (t, Instantiate.alias "Page" [] tyenv.Types.tycon_env)
    with
      Unify.Failure error ->
      begin match error with
        | `Msg s -> Debug.print ("Unification error: " ^ s)
        | _ -> ()
      end;
      failwith("Web programs must have type Page but this one has type "
               ^ Types.string_of_datatype t)
  end;

  (* Debug.print ("un-closure-converted IR: " ^ Ir.Show_program.show (prelude@globals@locals, main)); *)

  let nenv'' = Env.String.extend nenv nenv' in
  let tyenv'' = Types.extend_typing_environment tyenv tyenv' in

  (* let module Show_IntStringEnv = Env.Int.Show_t(Deriving_Show.Show_string) in *)
  (* let module Show_StringIntEnv = Env.String.Show_t(Deriving_Show.Show_int) in *)

  (* Debug.print ("nenv''" ^ Show_StringIntEnv.show nenv''); *)

  let tenv0 = Var.varify_env (nenv, tyenv.Types.var_env) in
  let gs0 = Env.String.fold (fun _name var vars -> IntSet.add var vars) nenv IntSet.empty in
  (* Debug.print("gs0: "^Show_intset.show gs0); *)
  let globals = Closures.bindings tenv0 gs0 globals in

  let tenv1 = Var.varify_env (nenv'', tyenv''.Types.var_env) in
  let gs1 = Env.String.fold (fun _name var vars -> IntSet.add var vars) nenv'' IntSet.empty in
  let (locals, main) = Closures.program tenv1 gs1 (locals, main) in

  (* Debug.print ("closure-converted locals: " ^ Ir.Show_program.show (locals, main)); *)

  let (locals, main), render_cont = wrap_with_render_page (nenv, tyenv) (locals, main) in
  let globals = prelude@globals in
  (* Debug.print ("closure-converted IR: " ^ Ir.Show_program.show (globals@locals, main)); *)

  BuildTables.program tenv0 Lib.primitive_vars ((globals @ locals), main);
  (render_cont, (nenv'', tyenv''), (globals, (locals, main)))

(* wrapper for ordinary uses of serve_request_program *)
let serve_request ((valenv, nenv, tyenv) as envs) prelude filename =

  let cgi_args = get_cgi_args() in
  Debug.print ("cgi_args: " ^ mapstrcat "," (fun (k, v) -> k ^ "="  ^ v) cgi_args);
  Lib.cgi_parameters := cgi_args;

  (* Compute cacheable stuff in one call *)
  let (render_cont, (nenv,tyenv), (globals, (locals, main))) =
    Loader.wpcache "program" (fun () ->
      make_program envs prelude filename
   )
  in

  (* We can evaluate the definitions here because we know they are pure. *)
  let valenv = Evalir.run_defs valenv globals in

  Errors.display (lazy (serve_request_program
			  (valenv, nenv, tyenv)
			  (globals, (locals, main), render_cont)
                          cgi_args))
