open Utility
open ProcessTypes

let show_json = Settings.add_bool("show_json", false, `User)

(*
  SL:
    Having implemented jsonisation of database values, I'm now
    unsure if this is what we really want. From a security point
    of view it certainly isn't a very good idea to pass this kind of
    information to the client.
*)
let json_of_db (db, params) =
  let driver = db#driver_name() in
  let (name, args) = Value.parse_db_string params in
    "{_db:{driver:\"" ^ driver ^ "\",name:\"" ^ name ^ "\", args:\"" ^ args ^ "\"}}"

(*
WARNING:
  May need to be careful about free type variables / aliases in row
*)
let json_of_table ((db, params), name, keys, row) =
  let json_of_key k = "[" ^ (mapstrcat ", " (fun x -> x) k) ^ "]" in
  let json_of_keylist ks = "[" ^ (mapstrcat ", " json_of_key ks) ^ "]" in
    "{_table:{db:'" ^ json_of_db (db, params) ^ "',name:\"" ^ name ^
      "\",row:\"" ^ Types.string_of_datatype (`Record row) ^
      "\",keys:\"" ^ json_of_keylist keys ^ "\"}}"

let js_dq_escape_string str =
  (* escape for placement in double-quoted string *)
  Str.global_replace (Str.regexp_string "\"") "\\\""
    (Str.global_replace (Str.regexp_string "\n") "\\n"
       (Str.global_replace (Str.regexp_string "\\") "\\\\"
          str))

(** Escape the argument for inclusion in a double-quoted string. *)
let js_dq_escape_char =
  function
    '"' -> "\\\""
  | '\\' -> "\\\\"
  | ch -> String.make 1 ch

let jsonize_location : Ir.location -> string = function
  | `Client  -> "client"
  | `Server  -> "server"
  | `Native  -> "native"
  | `Unknown -> "unknown"

(* Presumably we need only send an event handler to the client if it
   occurs in a value being sent to the client.

   On the other hand, we might expect to send all newly spawned client
   processes to the client whenever we visit the client.

   Similarly, we might transmit all client messages to the client
   whenever we visit the client.
*)

(* sets of client processes and event handlers that need to be serialized *)
type json_state = {processes: pid_set; handlers: IntSet.t}
let empty_state = {processes = PidSet.empty; handlers = IntSet.empty}
let add_pid : process_id -> json_state -> json_state =
  fun pid state -> {state with processes = PidSet.add pid state.processes}
let pid_state : process_id -> json_state =
  fun pid -> add_pid pid empty_state
let add_event_handler : int -> json_state -> json_state =
  fun h state -> {state with handlers = IntSet.add h state.handlers}
let merge_states s1 s2 = {processes = PidSet.union s1.processes s2.processes;
                          handlers =  IntSet.union s1.handlers s2.handlers}
let diff_state s1 s2 = {processes = PidSet.diff s1.processes s2.processes;
                        handlers = IntSet.diff s1.handlers s2.handlers}


type auxiliary = [`Process of process_id | `Handler of int]
let auxiliaries_of_state : json_state -> auxiliary list =
  fun state -> List.map (fun i -> `Handler i) (IntSet.elements state.handlers)
             @ List.map (fun i -> `Process i) (PidSet.elements state.processes)

(* Check if the client ID of a PID matches the current client ID as per the request data *)
let client_id_matches req_data pid_client_id =
  let client_id = RequestData.get_client_id req_data in
  ClientID.equal client_id pid_client_id

let rec jsonize_value req_data : Value.t -> string * json_state =
  function
  | `PrimitiveFunction _
  | `Continuation _
      as r ->
      failwith ("Can't yet jsonize " ^ Value.string_of_value r);
  | `FunctionPtr (f, fvs) ->
    let (_, _, _, location) = Tables.find Tables.fun_defs f in
    let location = jsonize_location location in
    let env_string, state =
      match fvs with
      | None     -> "", empty_state
      | Some fvs ->
        let s, state = jsonize_value req_data fvs in
        ", \"environment\":" ^ s, state in
    "{\"func\":\"" ^ Js.var_name_var f ^ "\"," ^
    " \"location\":\"" ^ location ^ "\"" ^ env_string ^ "}", state
  | `ClientFunction name -> "{\"func\":\"" ^ name ^ "\"}", empty_state
  | #Value.primitive_value as p -> jsonize_primitive p
  | `Variant (label, value) ->
    let s, state = jsonize_value req_data value in
    "{\"_label\":\"" ^ label ^ "\",\"_value\":" ^ s ^ "}", state
  | `Record fields ->
    let ls, vs = List.split fields in
    let ss, state = jsonize_values req_data vs in
      "{" ^
        mapstrcat "," (fun (kj, s) -> "\"" ^ kj ^ "\":" ^ s) (List.combine ls ss)
      ^ "}", state
  | `List [] -> "[]", empty_state
  | `List (elems) ->
    let ss, state = jsonize_values req_data elems in
      "[" ^ String.concat "," ss ^ "]", state
  | `Pid (`ClientPid (client_id, process_id)) ->
      "{\"_clientPid\":\"" ^ (ProcessID.to_string process_id) ^
      "\", \"_clientId\":\"" ^ (ClientID.to_string client_id) ^ "\"}",
      if client_id_matches req_data client_id then pid_state process_id else empty_state
  | `Pid (`ServerPid (process_id)) ->
      "{\"_serverPid\":\"" ^ (ProcessID.to_string process_id) ^ "\"}", empty_state
  | `Socket _ -> failwith "Cannot jsonize sockets"
  | `SpawnLocation (`ClientSpawnLoc client_id) ->
      "{\"_clientSpawnLoc\":" ^ (ClientID.to_string client_id) ^ "}", empty_state
  | `SpawnLocation (`ServerSpawnLoc) ->
      "{\"_serverSpawnLoc\": [] }", empty_state
and jsonize_primitive : Value.primitive_value -> string * json_state = function
  | `Bool value -> string_of_bool value, empty_state
  | `Int value -> string_of_int value, empty_state
  | `Float value -> string_of_float' value, empty_state
  | `Char c -> "{\"_c\":\"" ^ (js_dq_escape_char c) ^"\"}", empty_state
  | `Database db -> json_of_db db, empty_state
  | `Table t -> json_of_table t, empty_state
  | `XML xmlitem -> json_of_xmlitem xmlitem
  | `String s -> "\"" ^ js_dq_escape_string s ^ "\"", empty_state
and json_of_xmlitem = function
  | Value.Text s ->
      "[\"TEXT\",\"" ^ js_dq_escape_string (s) ^ "\"]", empty_state
  (* TODO: check that we don't run into problems when HTML containing
     an event handler is copied *)
  | Value.Node (tag, xml) ->
      let attrs, body, state =
        List.fold_right (fun xmlitem (attrs, body, state) ->
            match xmlitem with
            | Value.Attr (label, value) ->
              if label = "key" then
                begin
                  let key = int_of_string value in
                  let state' = add_event_handler key state in
                  ("\"" ^label ^ "\" : " ^ "\"" ^ js_dq_escape_string value ^ "\"") :: attrs, body, state'
                end
              else
                ("\"" ^label ^ "\" : " ^ "\"" ^ js_dq_escape_string value ^ "\"") :: attrs, body, state
            | _ ->
              let s, state' = json_of_xmlitem xmlitem in
              attrs, s :: body, merge_states state state') xml ([], [], empty_state)
      in
        "[\"ELEMENT\",\"" ^ tag ^
            "\",{" ^ String.concat "," attrs ^"},[" ^ String.concat "," body ^ "]]", state
  | Value.Attr _ -> assert false
and jsonize_values : RequestData.request_data -> Value.t list -> string list * json_state =
  fun req_data vs ->
    let ss, state =
      List.fold_left
        (fun (ss, state) v ->
           let s, state' = jsonize_value req_data v in
           s::ss, merge_states state state') ([], empty_state) vs in
    List.rev ss, state

let jsonize_value_with : RequestData.request_data -> json_state -> Value.t -> string * json_state =
  fun req_data state v ->
    let s, state' = jsonize_value req_data v in
    s, merge_states state state'

let encode_continuation (cont : Value.continuation) : string =
  Value.marshal_continuation cont

let rec resolve_auxiliaries :
  RequestData.request_data ->
  json_state ->
  auxiliary list ->
  (string PidMap.t * string IntMap.t) ->
  (string PidMap.t * string IntMap.t) =
  fun req_data state pids ((pmap, hmap) as maps) ->
    match pids with
    | [] -> maps
    | `Process pid :: auxs ->
      begin
        let client_id = RequestData.get_client_id req_data in
        match Proc.Proc.lookup_client_process client_id pid with
        | Some process ->
          let messages = Proc.Mailbox.pop_all_messages_for client_id pid in

          (* jsonizing these values may yield further state *)
          let ps, pstate = jsonize_value req_data process in
          let ms, mstate = jsonize_value req_data (`List messages) in

          (* These are processes which should be running on this client *)
          let s =
            "{\"pid\":\"" ^ (ProcessID.to_string pid) ^ "\"," ^
            " \"process\":" ^ ps ^ "," ^
            " \"messages\":" ^ ms ^ "}" in

          let new_state = merge_states pstate mstate in

          resolve_auxiliaries
            req_data
            (merge_states state new_state)
            (auxs @ auxiliaries_of_state (diff_state new_state state))
            (PidMap.add pid s pmap, hmap)
        | None -> (* this process is already active on the client *)
          resolve_auxiliaries req_data state auxs maps
      end
    | `Handler key :: auxs ->
      let hs = EventHandlers.find key in
      let hs_string, new_state = jsonize_value req_data hs in

      let s = "{\"key\": " ^ string_of_int key ^ "," ^
              " \"eventHandlers\":" ^ hs_string ^ "}" in

      resolve_auxiliaries
        req_data
        (merge_states state new_state)
        (auxs @ auxiliaries_of_state (diff_state new_state state))
        (pmap, IntMap.add key s hmap)

(* serialise the json_state as a string *)
let resolve_state : RequestData.request_data -> json_state -> string =
  fun req_data state ->
    let pmap, hmap = resolve_auxiliaries req_data state (auxiliaries_of_state state) (PidMap.empty, IntMap.empty) in
    let ws_url_data =
      (match RequestData.get_websocket_connection_url req_data with
         | Some ws_conn_url -> "\"ws_conn_url\":\"" ^ ws_conn_url ^ "\","
         | None -> "") in
    "{\"client_id\":\"" ^ ClientID.to_string (RequestData.get_client_id req_data) ^ "\"," ^
    ws_url_data ^
     "\"processes\":" ^ "[" ^ String.concat "," (PidMap.to_list (fun _ s -> s) pmap) ^ "]" ^ "," ^
     "\"handlers\":" ^ "[" ^ String.concat "," (IntMap.to_list (fun _ s -> s) hmap) ^ "]}"

(* FIXME: Currently we only send inactive client processes if they
   appear in the serialised value. We might consider sending *all*
   inactive client processes even if they don't appear in the
   serialised value.

   However, the whole thing will become problematic if we start
   connecting multiple clients to the same server.
*)

let jsonize_state req_data value =
  let _v, state = jsonize_value req_data value in
  state

  (* let s = resolve_state state in *)
  (* Debug.if_set show_json (fun () -> "json state: " ^ s); *)
  (* s *)

let value_with_state v s =
  "{\"value\":" ^ v ^ ",\"state\":" ^ s ^ "}"

let jsonize_value_with_state req_data value =
  let v, state = jsonize_value req_data value in
  value_with_state v (resolve_state req_data state)

(** [jsonize_call] creates the JSON object representing a client call,
    its server-side continuation, and the complete state of the
    scheduler. Note that [continuation], [name] and [args] arguments are
    all [Value]-style objects, while the last argument,
    [sched_state_json], is already JSONized--a nonuniform interface. This
    is because the [proc.ml] file keeps the scheduler state as an abstract
    type so we can't inspect it here. Consider changing this.
*)
let jsonize_call req_data continuation name args =
  let vs, state = jsonize_values req_data args in
  let v =
    "{\"__continuation\":\"" ^ (encode_continuation continuation) ^"\"," ^
    "\"__name\":\"" ^ name ^ "\"," ^
    "\"__args\":[" ^ String.concat ", " vs ^ "]}" in
  value_with_state v (resolve_state req_data state)

let jsonize_value req_data value =
  Debug.if_set show_json
    (fun () -> "jsonize_value => " ^ Value.string_of_value value);
  let rv = jsonize_value_with_state req_data value in
    Debug.if_set show_json
      (fun () -> "jsonize_value <= " ^ rv);
    rv

let parse_json str =
  Jsonparse.parse_json Jsonlex.jsonlex (Lexing.from_string str)

let parse_json_b64 str = parse_json(Utility.base64decode str)
