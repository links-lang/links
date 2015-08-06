(*pp deriving *)

open Notfound
open List

open Performance
open Utility

type query_params = (string * Value.t) list deriving (Show)

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

type program = Ir.binding list * Ir.computation * Value.continuation;;

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

let serialize_call_to_client (continuation, name, arg) =
  Json.jsonize_call continuation name arg

let empty valenv = Value.empty_env (Value.get_closures valenv)

let resolve_function f env = Evalir.eval_fun env f

let parse_remote_call (valenv, nenv, tyenv) cgi_args =
  let fname = Utility.base64decode (assoc "__name" cgi_args) in
  let args = Utility.base64decode (assoc "__args" cgi_args) in
  (* Debug.print ("args: " ^ Value.Show_t.show (Json.parse_json args)); *)
  let args = Value.untuple (Json.parse_json args) in
  let r = Json.parse_json_b64 (assoc "__env" cgi_args) in
  let local_env =
    (* Unpack the record to an alist *)
    match Value.intmap_of_record r with
        Some x -> x
      | None -> failwith "Decoding remote-call request, __env was not a record."
  in
  let local_env = IntMap.map (fun x -> (x, `Local)) local_env in
  let env = Value.extend valenv local_env in
  (* Debug.print ("env: " ^ Value.Show_env.show env); *)
  Debug.print("Resolving server call to " ^ fname);

  (* FIXME *)
  (* ridiculousness: fname is sometimes an integer and sometimes a
     real name! *)

  let func =
    let var =
      try int_of_string fname with
        _ ->
        if not (Env.String.has nenv fname) then
          failwith ("fname: " ^ fname ^ " isn't an integer and isn't in the nenv environment!")
        else
          Env.String.lookup nenv fname
    in
    try resolve_function var valenv with
      _ ->
      begin
        match Value.lookup var valenv with
        | Some v -> v
        (* Try the primitives. *)
        | None ->
          Debug.print ("fname: " ^ fname ^ " not in value environment");
          Lib.primitive_stub fname
      end
  in
  RemoteCall(func, env, args)

let decode_continuation envs program (cont : string) : Value.continuation =
  let fixup_cont =
  (* At some point, '+' gets replaced with ' ' in our base64-encoded
     string.  Here we put it back as it was. *)
    Str.global_replace (Str.regexp " ") "+"
  in
  let envs = Value.build_unmarshal_envs envs program in
    Value.unmarshal_continuation envs (fixup_cont cont)

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
let parse_server_cont (valenv, nenv, tyenv) program params =
  let unmarshal_envs = Value.build_unmarshal_envs (valenv, nenv, tyenv) program in
  ServerCont (Value.unmarshal_value unmarshal_envs (assoc "_k" params))

let parse_client_return envs program cgi_args =
  (* Debug.print("parsing client return"); *)
  let continuation =
    decode_continuation envs program (assoc "__continuation" cgi_args) in
  (* Debug.print("continuation: " ^ Value.Show_continuation.show continuation); *)
  let arg = Json.parse_json_b64 (assoc "__result" cgi_args) in
  (* Debug.print ("arg: "^Value.Show_t.show arg); *)
  let (valenv, _, _) = envs in
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
    that we can use it elsewhere (i.e. in processing ExprEval).
*)
let wrap_with_render_page (nenv, {Types.tycon_env=tycon_env; Types.var_env=_})
                          (bs, body) =
  let xb, x = Var.fresh_var_of_type (Instantiate.alias "Page" [] tycon_env) in
  let tail = Ir.var_appln nenv "renderPage" [`Variable x] in
  let cont = fun env -> [(`Local, x, env, ([], tail))] in
    (bs @ [`Let (xb, ([], body))], tail), cont

let perform_request cgi_args (valenv, nenv, tyenv) cont0 =
  function
    | ServerCont t ->
      Debug.print("Doing ServerCont");
      let v = Evalir.apply_with_cont cont0 valenv (t, []) in
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
       Utility.base64encode (Json.jsonize_value result))
    | EvalMain (globals, (locals, main))->
        Debug.print("Doing EvalMain");
        ("text/html",
         if is_client_program (globals @ locals, main) then
           let program = (globals @ locals, main) in
           Debug.print "Running client program.";
	   let closures = Value.get_closures valenv in
             lazy (Irtojs.generate_program_page
                     ~cgi_env:cgi_args
                     (closures, Lib.nenv, Lib.typing_env)
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

let make_program (_,nenv,tyenv) prelude filename =
  (* Warning: cache call nested inside another cache call *)
  let (nenv', tyenv'), (globals,(locals,main),t) =
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

  let module Show_IntEnv = Env.Int.Show_t(Deriving_Show.Show_string) in
  let module Show_StringEnv = Env.String.Show_t(Deriving_Show.Show_int) in

  (* Debug.print ("nenv''" ^ Show_StringEnv.show nenv''); *)

  let tenv0 = Var.varify_env (nenv, tyenv.Types.var_env) in
  let gs0 = Env.String.fold (fun _name var vars -> IntSet.add var vars) nenv IntSet.empty in
  (* Debug.print("gs0: "^Show_intset.show gs0); *)
  let fenv0 = Closures.ClosureVars.bindings tenv0 gs0 globals in
  (* Debug.print ("fenv0: " ^ Closures.Show_fenv.show fenv0); *)
  let globals = Closures.ClosureConvert.bindings tenv0 gs0 fenv0 globals in

  let tenv1 = Var.varify_env (nenv'', tyenv''.Types.var_env) in
  let gs1 = Env.String.fold (fun _name var vars -> IntSet.add var vars) nenv'' IntSet.empty in
  let fenv1 = Closures.ClosureVars.program tenv1 gs1 (locals, main) in
  let (locals, main) = Closures.ClosureConvert.program tenv1 gs1 fenv1 (locals, main) in

  (* Debug.print ("closure-converted locals: " ^ Ir.Show_program.show (locals, main)); *)

  Ir.FunMap.bindings FunMap.fun_map (globals @ locals);

  let (locals,main), render_cont =
    wrap_with_render_page (nenv, tyenv) (locals,main) in

  let globals = prelude@globals in

  (* Debug.print ("closure-converted IR: " ^ Ir.Show_program.show (globals@locals, main)); *)

  let closures =
    Ir.ClosureTable.program
      (Var.varify_env (nenv, tyenv.Types.var_env))
      Lib.primitive_vars
      (globals @ locals, main)
  in
  let cont0 = render_cont (Value.empty_env closures) in
  (closures, cont0, (nenv'', tyenv''), (globals, (locals, main)))

(* wrapper for ordinary uses of serve_request_program *)
let serve_request ((valenv,nenv,tyenv) as envs) prelude filename =

  let cgi_args = get_cgi_args() in
  Debug.print ("cgi_args: " ^ mapstrcat "," (fun (k, v) -> k ^ "="  ^ v) cgi_args);
  Lib.cgi_parameters := cgi_args;

  (* Compute cacheable stuff in one call *)
  let (closures,cont0,(nenv,tyenv), (globals,(locals,main))) =
    Loader.wpcache "program" (fun () ->
      make_program envs prelude filename
   )
  in

  let valenv = Value.with_closures valenv closures in
  (* We can evaluate the definitions here because we know they are pure. *)
  let valenv = Evalir.run_defs valenv globals in

  Errors.display (lazy (serve_request_program
			  (valenv,nenv,tyenv)
			  (globals,(locals,main),cont0)
                          cgi_args))
