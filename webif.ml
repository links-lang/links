(*pp deriving *)

open Notfound
open List

open Performance
open Utility

type query_params = (string * Value.t) list deriving (Show)

type web_request =
  | ContApply of Value.continuation * query_params
  | ExprEval of Ir.tail_computation * Value.env
  | ClientReturn of Value.continuation * (* continuation *)
      Value.t                            (* argument *)
  | RemoteCall of Value.t *            (* function *)
      Value.env *                      (* closure environment *)
      Value.t list                     (* argument *)
  | EvalMain
      deriving (Show)

type program = Ir.binding list * Ir.computation * Value.continuation;;

(** Does at least one of the functions have to run on the client? *)
let is_client_program : Ir.program -> bool =
  fun (bs, main) ->
    exists
      (function
         | `Fun (_, _, `Client)
         | `Alien (_, "javascript") -> true
         | `Rec defs ->
             exists
               (fun (_, _, location) -> location = `Client)
               defs
         | _ -> false)
      bs

let serialize_call_to_client (continuation, name, arg) = 
  Json.jsonize_call continuation name arg

let resolve_function funcmap x env =
  let binding = assoc x funcmap in
    Evalir.eval_toplevel env ([binding], `Return(`Variable x))  

let rec resolve_functions closures funcmap = 
  function
    | `FunctionPtr(x, env) -> 
        let env = IntMap.map (fun (x,s) -> 
                                resolve_functions closures funcmap x,s) (Value.get_parameters env) in
          resolve_function funcmap x (Value.extend (Value.empty_env closures) env)
(* Q: incorporate valenv here? *)
    | `List elems -> `List (map (resolve_functions closures funcmap) elems)
    | `Record fields -> `Record(alistmap (resolve_functions closures funcmap) fields)
    | `Variant(l, v) -> `Variant(l, resolve_functions closures funcmap v)
    | `RecFunction(defs, env, f, scope) ->
	let clos = Value.get_closures env in
        let env = Utility.IntMap.map
          (fun (x,s) -> resolve_functions closures funcmap x, s) (Value.get_parameters env)
        in `RecFunction(defs,
                        (Value.extend (Value.empty_env clos) env),
                        f, scope)
    | `ClientFunction _ as x -> x
    | `Continuation _ as x -> assert false      (* Unimplemented. Traverse it? *)
    | `PrimitiveFunction _ as x -> x
    | #Value.primitive_value as x -> x

let parse_remote_call (valenv, nenv, tyenv) (program:Ir.program) cgi_args = 
  let funcmap = Ir.funcmap program in (* FIXME: Quite slow... *)
  let closures = Value.get_closures valenv in
  let fname = Utility.base64decode (assoc "__name" cgi_args) in
  let args = Utility.base64decode (assoc "__args" cgi_args) in
  let args = Value.untuple (Json.parse_json args) in
  let args = map (resolve_functions closures funcmap) args in
  let local_env = Json.parse_json_b64 (assoc "__env" cgi_args) in
  let local_env = 
    (* Unpack the record to an alist *)
    match Value.intmap_of_record local_env with
        Some x -> x
      | None -> failwith "Decoding remote-call request, __env was not a record."
  in
  let local_env = IntMap.map (fun v -> resolve_functions closures funcmap v) 
    local_env in
  let local_env = IntMap.map (fun x -> (x, `Local)) local_env in
  let env = Value.extend valenv local_env in
  Debug.print("Resolving server call to " ^ fname);
    (* [fname] may be in the (int-indexed) funcmap, if it is a nested
       function, or in [nenv] if a global, or in [Lib.primitive_stub].
       NOTE: in fact, the funcmap contains top-level functions, so
       we can ditch the [nenv] lookup. *)
  let func = try (* Try finding it in the funcmap as a nested closure *)
    let f_var = int_of_string fname in
      (* FIXME: use resolve_function here *)
    let binding = assoc f_var funcmap in
      Evalir.eval_toplevel env ([binding], `Return(`Variable f_var))
  with _ -> 
    (* Try the nenv, which maps the global functions *)
    let var = Env.String.lookup nenv fname in
      match Value.lookup var valenv with
        | Some v -> v
            (* Try the primitives. *)
        | None -> Lib.primitive_stub fname
  in
    RemoteCall(func, env, args)

let make_unmarshal_envs (valenv, nenv, tyenv) program = 
  (* jcheney: is this closure table call necessary? 
     maybe can use closures from valenv instead.  *)
  let closures = 
    Ir.ClosureTable.program 
      (Var.varify_env (nenv, tyenv.Types.var_env)) 
      Lib.primitive_vars program 
  in
  let valenv = Value.with_closures valenv closures in
  let unmarshal_envs= Value.build_unmarshal_envs(valenv, nenv, tyenv) program in
    closures, unmarshal_envs

let decode_continuation envs program (cont : string) : Value.continuation =
  let fixup_cont = 
  (* At some point, '+' gets replaced with ' ' in our base64-encoded
     string.  Here we put it back as it was. *)
    Str.global_replace (Str.regexp " ") "+" 
  in
  let _, envs = make_unmarshal_envs envs program in
    Value.unmarshal_continuation envs (fixup_cont cont)




(** Boolean tests for cgi parameters *)

let is_cont_apply_param (key, _) = key == "_cont"

let is_remote_call params =
  mem_assoc "__name" params && mem_assoc "__args" params

let is_client_return params = 
  mem_assoc "__continuation" params && mem_assoc "__result" params

let is_cont_apply params = 
  mem_assoc "_cont" params

let is_expr_eval args =
  mem_assoc "_k" args
  

(** NOTE: The invocation mode [ContApply] is used by the
    [freshResource] function defined in the prelude, which creates an
    explicit link using a [_cont] parameter.
*)

(** Extract continuation from the parameters passed in over CGI.*)
let parse_cont_apply (valenv, nenv, tyenv) program params =
  let pickled_continuation = assoc "_cont" params in
  let params = filter (not -<- is_cont_apply_param) params in
  let params = alistmap Value.box_string params in
  let _, unmarshal_envs = make_unmarshal_envs (valenv, nenv, tyenv) program in
    (* TBD: create a debug setting for printing webif modes. *)
    ContApply(Value.unmarshal_continuation unmarshal_envs pickled_continuation,
              params)

(** Extract expression/environment pair from the CGI parameters.*)
let parse_expr_eval (valenv, nenv, tyenv) program params =
  let string_pair (l, r) =
    `Extend
      (StringMap.from_alist [("1", `Constant (`String l));
                             ("2", `Constant (`String r))],
       None) in
  let closures, unmarshal_envs = make_unmarshal_envs (valenv, nenv, tyenv) 
    program in
    (* FIXME: "_k" is a misnomer; it should be "_expr" *)
    match Value.unmarshal_value unmarshal_envs (assoc "_k" params) with
        | `RecFunction ([(f, (_xs, _body))], locals, _, _) as v ->
          let json_env =
            if mem_assoc "_jsonArgs" params then
              match Json.parse_json_b64 (assoc "_jsonArgs" params) with
                | `Record fields ->
                       fold_left
                         (fun env (name, v) ->
                            Value.bind (int_of_string name) (v, `Local) env)
                         (Value.empty_env closures)
                         fields
                | _ -> assert false
            else
              Value.empty_env closures in

          (* we don't need to pass the args in here as they are read using the environment
             function *)

          (*           let params = filter (not -<- is_cont_apply_param) params in *)            
          (*           let args = *)
          (*             fold_right *)
          (*               (fun pair env -> *)
          (*                  `ApplyPure (`Variable (Env.String.lookup nenv "Cons"), [string_pair pair; env])) *)
          (*               params *)
          (*               (`Variable (Env.String.lookup nenv "Nil")) in *)

          let env = Value.shadow(Value.bind f (v, `Local) locals) ~by:json_env in
            ExprEval (`Apply (`Variable f, []), env)
      | _ -> assert false

let parse_client_return envs program cgi_args = 
  let continuation =
    decode_continuation envs program (assoc "__continuation" cgi_args) in
  let arg = Json.parse_json_b64 (assoc "__result" cgi_args) in
    (* FIXME: refactor *)
  let funcmap = Ir.funcmap program in (* FIXME: Quite slow... *)
  let (valenv, _, _) = envs in
  let closures = Value.get_closures valenv in
  let arg = resolve_functions closures funcmap arg in
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
let parse_request env comp cgi_args = 
  if      (is_remote_call cgi_args)   
  then parse_remote_call env comp cgi_args 
  else if (is_client_return cgi_args) 
  then parse_client_return env comp cgi_args 
  else if (is_cont_apply cgi_args)   
  then parse_cont_apply env comp cgi_args 
  else if (is_expr_eval cgi_args)     
  then parse_expr_eval env comp cgi_args 
  else EvalMain
;;


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

(* jcheney: lowered type of cont0 *)

let perform_request cgi_args (valenv, nenv, tyenv) (globals, locals, main) cont0 =
  function
    | ContApply(cont, params) ->
        Debug.print("Doing ContApply");
        ("text/html",
         (Value.string_of_value
            (Evalir.apply_cont_toplevel cont valenv (`Record params))))
    | ExprEval(expr, expr_locals) ->        
        Debug.print("Doing ExprEval");
        let env = Value.shadow valenv ~by:expr_locals in
        let v = snd (Evalir.run_program_with_cont 
                       (cont0)
                       env ([], expr)) in
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
        let result = Evalir.apply_toplevel env (func, args) in
          if not(Proc.singlethreaded()) then
            (prerr_endline "Remaining procs on server after remote call!";
             assert(false));
          ("text/plain",
           Utility.base64encode (Json.jsonize_value result))
    | EvalMain -> 
        Debug.print("Doing EvalMain");
        ("text/html",
         if is_client_program (globals @ locals, main) then
           let program = (globals @ locals, main) in
             Debug.print "Running client program.";
(* jcheney:redundant? *)
(*
             let tenv = Var.varify_env (nenv, tyenv.Types.var_env) in
             let closures = lazy (
	       Ir.ClosureTable.program tenv Lib.primitive_vars program
	      ) <|measure_as|> "closures" 
	     in
*)
	   let closures = Value.get_closures valenv in
	     lazy (
	     Irtojs.generate_program_page
               ~cgi_env:cgi_args
               (closures, Lib.nenv, Lib.typing_env)
               program 
	     ) <|measure_as|> "irtojs"
         else
           let program = locals, main in 
             (* wrap_with_render_page (nenv, tyenv) (locals, main) in*)
             Debug.print "Running server program";
(* jcheney: redundant? *)
(*
             let tenv = Var.varify_env (nenv, tyenv.Types.var_env) in
             let closures = Ir.ClosureTable.program tenv Lib.primitive_vars 
               (globals @ locals, main) in
             let valenv = Value.with_closures valenv (closures) in
*)
             let _env, v = Evalir.run_program valenv program in
               Value.string_of_value v)

let serve_request_program env (globals, (locals, main), render_cont) cgi_args
    =
  try 
    let request = parse_request env (globals@locals,main) cgi_args
    in
    let (content_type, content) =
      perform_request cgi_args env (globals, locals, main) 
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
    | exc -> Lib.print_http_response[("Content-type","text/html; charset=utf-8")]
        (error_page (Errors.format_exception_html exc))


(* does the preprocessing to turn prelude+filename into a program *)
(* result can be cached *)

let make_program (_,nenv,tyenv) prelude filename = 

  (* Warning: cache call nested inside another cache call *)
  let (nenv', tyenv'), (globals,(locals,main),t) = 
    Errors.display_fatal (Loader.load_file (nenv, tyenv)) filename
  in
  
  begin try
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
  let nenv'' = Env.String.extend nenv nenv' in
  let tyenv'' = Types.extend_typing_environment tyenv tyenv' in
  
  let (locals,main), render_cont = 
    wrap_with_render_page (nenv, tyenv) (locals,main) in
  
  let globals = prelude@globals in
  
  let closures = 
    Ir.ClosureTable.program
      (Var.varify_env (nenv, tyenv.Types.var_env)) 
      Lib.primitive_vars
      (globals @ locals, main)
  
  in
  let cont0  = render_cont (Value.empty_env closures) in

  (closures,cont0,(nenv'', tyenv''), (globals, (locals, main)))
;;

(* wrapper for ordinary uses of serve_request_program *)
let serve_request ((valenv,nenv,tyenv) as envs) prelude filename =
  
  let cgi_args = get_cgi_args() in
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
;;

