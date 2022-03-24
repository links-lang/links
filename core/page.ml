(* Page generation *)

open Irtojs
module Make_RealPage (C : JS_PAGE_COMPILER) (G : JS_CODEGEN) = struct
  open Utility

  let jslib_url = Webserver_types.jslib_url
  let external_base = Webserver_types.external_base_url
  let hide_database_info = Js.hide_database_info
  let session_exceptions_enabled = Settings.get (Basicsettings.Sessions.exceptions_enabled)

  let get_js_lib_url () =
    let js_url = from_option "" (opt_map strip_slashes (Settings.get jslib_url)) in
    match opt_map strip_slashes (Settings.get external_base) with
    | None ->
       "/" ^ js_url ^ "/"
    | Some base_url ->
      "/" ^ base_url ^ "/" ^ js_url ^ "/"

  let ext_script_tag ?(base=get_js_lib_url()) file =
    "  <script type='text/javascript' src=\""^base^file^"\"></script>"

  let script_tag body =
    "<script type='text/javascript'>\n'use strict';\n" ^ body ^ "\n</script>\n"

  let make_boiler_page ?(cgi_env=[]) ?(onload="") ?(body="") ?(html="") ?(head="") ?(external_files=[]) () =
    let in_tag tag str = "<" ^ tag ^ ">\n" ^ str ^ "\n</" ^ tag ^ ">" in
    let custom_ext_script_tag str = "<script type='text/javascript' src='" ^ str ^ "'></script>" in
    let ffiLibs = String.concat "\n" (List.map custom_ext_script_tag external_files) in
    let debug_flag onoff = "\n    <script type='text/javascript'>var DEBUGGING=" ^
      string_of_bool onoff ^ ";</script>"
    in
    let db_config_script =
      if Settings.get hide_database_info then
        script_tag("    function _getDatabaseConfig() {
     return {}
    }
    var getDatabaseConfig = _$Links.kify(_getDatabaseConfig);\n")
      else
        script_tag("    function _getDatabaseConfig() {
      return {driver:'" ^ from_option "" (Settings.get DatabaseDriver.driver) ^
                      "', args:'" ^ from_option "" (Settings.get Database.connection_info) ^"'}
    }
    var getDatabaseConfig = _$Links.kify(_getDatabaseConfig);\n") in
    let env =
      script_tag("  var cgiEnv = {" ^
                    mapstrcat "," (fun (name, value) -> "'" ^ name ^ "':'" ^ value ^ "'") cgi_env ^
                    "};\n  _makeCgiEnvironment();\n") in
    "<!DOCTYPE html>\n" ^
    in_tag "html" (in_tag "head"
                     (  debug_flag (Settings.get Debug.enabled)
                        ^ ext_script_tag "jslib.js" ^ "\n"
                        ^ ffiLibs ^ "\n"
                        ^ db_config_script
                        ^ env
                        ^ "<meta charset=\"utf-8\">"
                        ^ head
                        ^ "<script type=\"text/javascript\">
                             'use strict';
                             function _isRuntimeReady() {
                                if (_$Links === void 0) {
                                   const msg = \"<h1>Startup error: Runtime dependency `jslib.js' is not loaded.</h1>\";
                                   document.body.innerHTML = msg;
                                   document.head.innerHTML = \"\";
                                   return false;
                                }
                                return true;
                             }
                           </script>"
                     )
                   ^ "<body onload=\'" ^ onload ^ "\'>
  <script type='text/javascript'>
  'use strict';
  _debug(\"Continuation: \" + _$K.kind);
  _startTimer();" ^ body ^ ";
  </script>" ^ html ^ "</body>")


  (* generate code to resolve JSONized toplevel let-bound values *)
  let resolve_toplevel_values : string list -> string =
    fun names ->
      String.concat "" (List.map (fun name -> "    " ^ name ^ " = _$Links.resolveServerValue(state, " ^ name ^ ");\n") names)

  let page : ?cgi_env:(string * string) list ->
             wsconn_url:(Webserver_types.websocket_url option) ->
             (Var.var Env.String.t * Types.typing_environment) ->
             Ir.binding list -> (Value.env * Value.t) -> string list -> string
    = fun ?(cgi_env=[]) ~wsconn_url (nenv, _tyenv) defs (valenv, v) deps ->
    let open Json in
    let req_data = Value.Env.request_data valenv in
    let client_id = RequestData.get_client_id req_data in
    let json_state = JsonState.empty client_id wsconn_url in

    (* Add the event handlers for the final value to be sent *)
    let json_state = ResolveJsonState.add_value_information v json_state in
    (* Json.jsonize_state req_data v in *)

    (* divide HTML into head and body secitions (as we need to augment the head) *)
    let hs, bs = Value.split_html (List.map Value.unbox_xml (Value.unbox_list v)) in
    let venv =
      Env.String.fold
        (fun name v venv -> VEnv.bind v name venv)
        nenv
        VEnv.empty
    in

    let json_state, venv, let_names, f = C.generate_toplevel_bindings valenv json_state venv defs in
    let init_vars = "  function _initVars(state) {\n" ^ resolve_toplevel_values let_names ^ "  }" in

    (* Add AP information; mark APs as delivered *)
    let json_state = ResolveJsonState.add_ap_information client_id json_state in

    (* Add process information to the JSON state; mark all processes as active *)
    let json_state = ResolveJsonState.add_process_information client_id json_state in

    (* Add channel information to the JSON state; mark all as residing on client *)
    let json_state = ResolveJsonState.add_channel_information client_id json_state in

    let state_string = JsonState.to_json json_state |> Json.json_to_string in
    let escaped_state_string = `String state_string |> Json.json_to_string in

    let printed_code =
      let _venv, code = C.generate_program venv ([], Ir.Return (Ir.Extend (StringMap.empty, None))) in
      let code = f code in
      let code =
        code |> (C.generate_stubs valenv defs) |> C.wrap_with_server_lib_stubs
      in
      G.string_of_js code
    in
    let welcome_msg =
      "_debug(\"Links version " ^ (val_of (Settings.get Basicsettings.version)) ^ "\");"
    in
    make_boiler_page
      ~cgi_env
      ~body:printed_code
      ~html:(Value.string_of_xml ~close_tags:true bs)
      ~head:(script_tag welcome_msg ^ "\n"
              ^ script_tag (G.string_of_js C.primitive_bindings) ^ "\n"
              ^ script_tag ({|
// Deprecated continuation operations.
const _applyCont = _$K.apply;
const _yieldCont = _$K.yield;
const _idy = _$K.idy;
|})
              ^ script_tag("  var _jsonState = " ^ escaped_state_string ^ "\n" ^ init_vars)
              ^ Value.string_of_xml ~close_tags:true hs)
      ~onload:"_isRuntimeReady() && _startRealPage()"
      ~external_files:deps
      ()
end

module RealPage = Make_RealPage(Compiler)(Js_CodeGen)
