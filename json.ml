(* Side-effect-free JSON operations. *)
open ProcessTypes
open Utility

(* Setting *)
let show_json = Basicsettings.Json.show_json

(* Type synonyms *)
type handler_id = int
type websocket_url = string

(* Types *)
type json_string = string

let parse_json str =
  Jsonparse.parse_json Jsonlex.jsonlex (Lexing.from_string str)

let parse_json_b64 str = parse_json(Utility.base64decode str)


(* Helper functions for jsonization *)
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

let rec jsonize_value' : Value.t -> json_string =
  function
  | `PrimitiveFunction _
  | `ReifiedContinuation _
  | `Continuation _
  | `Socket _
      as r ->
      failwith ("Can't jsonize " ^ Value.string_of_value r);
  | `FunctionPtr (f, fvs) ->
    let (_, _, _, location) = Tables.find Tables.fun_defs f in
    let location = jsonize_location location in
    let env_string =
      match fvs with
      | None     -> ""
      | Some fvs ->
        let s = jsonize_value' fvs in
        ", \"environment\":" ^ s in
    "{\"func\":\"" ^ Js.var_name_var f ^ "\"," ^
    " \"location\":\"" ^ location ^ "\"" ^ env_string ^ "}"
  | `ClientDomRef i -> "{\"_domRefKey\":" ^ (string_of_int i) ^ "}"
  | `ClientFunction name -> "{\"func\":\"" ^ name ^ "\"}"
  | #Value.primitive_value as p -> jsonize_primitive p
  | `Variant (label, value) ->
    let s = jsonize_value' value in
    "{\"_label\":\"" ^ label ^ "\",\"_value\":" ^ s ^ "}"
  | `Record fields ->
    let ls, vs = List.split fields in
    let ss = jsonize_values vs in
      "{" ^
        mapstrcat "," (fun (kj, s) -> "\"" ^ kj ^ "\":" ^ s) (List.combine ls ss)
      ^ "}"
  | `List [] -> "[]"
  | `List (elems) ->
    let ss = jsonize_values elems in
      "[" ^ String.concat "," ss ^ "]"
  | `AccessPointID (`ClientAccessPoint (cid, apid)) ->
      "{\"_clientAPID\": " ^ (AccessPointID.to_json apid) ^
      ", \"_clientId\":" ^ (ClientID.to_json cid) ^  "}"
  | `AccessPointID (`ServerAccessPoint (apid)) ->
      "{\"_serverAPID\": " ^ (AccessPointID.to_json apid) ^ "}"
  | `Pid (`ClientPid (client_id, process_id)) ->
      "{\"_clientPid\":" ^ (ProcessID.to_json process_id) ^
      ", \"_clientId\":" ^ (ClientID.to_json client_id) ^ "}"
  | `Pid (`ServerPid (process_id)) ->
      "{\"_serverPid\":" ^ (ProcessID.to_json process_id) ^ "}"
  | `SessionChannel (ep1, ep2) ->
      "{\"_sessEP1\":" ^ ChannelID.to_json ep1 ^
      ",\"_sessEP2\":" ^ ChannelID.to_json ep2 ^ "}"
  | `SpawnLocation (`ClientSpawnLoc client_id) ->
      "{\"_clientSpawnLoc\":" ^ (ClientID.to_json client_id) ^ "}"
  | `SpawnLocation (`ServerSpawnLoc) ->
      "{\"_serverSpawnLoc\": [] }"
and jsonize_primitive : Value.primitive_value -> string  = function
  | `Bool value -> string_of_bool value
  | `Int value -> string_of_int value
  | `Float value -> string_of_float' value
  | `Char c -> "{\"_c\":\"" ^ (js_dq_escape_char c) ^"\"}"
  | `Database db -> json_of_db db
  | `Table t -> json_of_table t
  | `XML xmlitem -> json_of_xmlitem xmlitem
  | `String s -> "\"" ^ js_dq_escape_string s ^ "\""
and json_of_xmlitem = function
  | Value.Text s ->
      "{ \"type\": \"TEXT\", \"text\": \"" ^ js_dq_escape_string (s) ^ "\"}"
  (* TODO: check that we don't run into problems when HTML containing
     an event handler is copied *)
  | Value.NsNode (ns, tag, xml) ->
      let attrs, body =
        List.fold_right (fun xmlitem (attrs, body) ->
            match xmlitem with
            | Value.Attr (label, value) ->
                ("\"" ^label ^ "\" : " ^ "\"" ^ js_dq_escape_string value ^ "\"") :: attrs, body
            | Value.NsAttr (ns, label, value) ->
                ("\"" ^ ns ^ ":" ^ label ^ "\" : " ^ "\"" ^ js_dq_escape_string value ^ "\"") :: attrs, body
            | _ ->
              let s = json_of_xmlitem xmlitem in
              attrs, s :: body) xml ([], [])
      in
        "{ \"type\": \"ELEMENT\"," ^
          "\"tagName\": \"" ^ tag ^ "\"," ^
          (if (String.length(ns) > 0) then "\"namespace\": \"" ^ ns ^ "\"," else "") ^
          "\"attrs\": {" ^ String.concat "," attrs ^ "}," ^
          "\"children\": [" ^ String.concat "," body ^ "]" ^
        "}"
  | Value.Node (name, children) -> json_of_xmlitem (Value.NsNode ("", name, children))
  | _ -> failwith "Cannot jsonize a detached attribute."

and jsonize_values : Value.t list -> string list  =
  fun vs ->
    let ss =
      List.fold_left
        (fun ss v ->
           let s = jsonize_value' v in
           s::ss) [] vs in
    List.rev ss


let value_with_state v s =
  "{\"value\":" ^ v ^ ",\"state\":" ^ s ^ "}"


let show_processes procs =
  (* Show the JSON for a prcess, including the PID, process to be run, and mailbox *)
  let show_process (pid, (proc, msgs)) =
    let ps = jsonize_value' proc in
    let ms = jsonize_value' (`List msgs) in
    "{\"pid\":" ^ (ProcessID.to_json pid) ^ "," ^
    " \"process\":" ^ ps ^ "," ^
    " \"messages\":" ^ ms ^ "}" in
  let bnds = PidMap.bindings procs in
  String.concat "," (List.map show_process bnds)

let show_handlers evt_handlers =
  (* Show the JSON for an event handler: the evt handler key, and the associated process *)
  let show_evt_handler (key, proc) =
    let h_proc_json = jsonize_value' proc in
    "{\"key\": " ^ string_of_int key ^ "," ^
    " \"eventHandlers\":" ^ h_proc_json ^ "}" in
  let bnds = IntMap.bindings evt_handlers in
  String.concat "," (List.map show_evt_handler bnds)

let show_aps aps =
  let ap_list = AccessPointIDSet.elements aps in
  String.concat "," (List.map (AccessPointID.to_json) ap_list)

let print_json_state client_id conn_url procs handlers aps =
    let ws_url_data =
    (match conn_url with
       | Some ws_conn_url -> "\"ws_conn_url\":\"" ^ ws_conn_url ^ "\","
       | None -> "") in
  "{\"client_id\":" ^ (ClientID.to_json client_id) ^ "," ^
   ws_url_data ^
   "\"access_points\":" ^ "[" ^ (show_aps aps) ^ "]" ^ "," ^
   "\"processes\":" ^ "[" ^ (show_processes procs) ^ "]" ^ "," ^
   "\"handlers\":" ^ "[" ^ (show_handlers handlers) ^ "]}"


(* JSON state definition *)
module JsonState = struct
  type t = {
    client_id : client_id;
    ws_conn_url : websocket_url option;
    processes: (Value.t * Value.t list) pid_map;
    handlers: Value.t intmap;
    aps: apid_set
  }

  (** Creates an empty JSON state *)
  let empty cid url = {
    client_id = cid;
    ws_conn_url = url;
    processes = PidMap.empty;
    handlers = IntMap.empty;
    aps = AccessPointIDSet.empty
  }

  (** Adds a process and its mailbox to the state. *)
  let add_process pid proc mb state =
    { state with processes = PidMap.add pid (proc, mb) state.processes }

  (** Adds an event handler to the state *)
  let add_event_handler handler_id handler_val state =
    { state with handlers = IntMap.add handler_id handler_val state.handlers }

  (** Adds an access point ID to the state *)
  let add_ap_id apid state =
    { state with aps = AccessPointIDSet.add apid state.aps }

  (** Serialises the state as a JSON string *)
  let to_string s = print_json_state s.client_id s.ws_conn_url s.processes s.handlers s.aps
end

type json_state = JsonState.t

(* External interface *)
let jsonize_value_with_state value state =
  Debug.if_set show_json
      (fun () -> "jsonize_value_with_state => " ^ Value.string_of_value value);
  let jv = jsonize_value' value in
  let jv_s = value_with_state jv (JsonState.to_string state) in
  Debug.if_set show_json (fun () -> "jsonize_value_with_state <= " ^ jv_s);
  jv_s

let jsonize_value v =
  Debug.if_set show_json
      (fun () -> "jsonize_value => " ^ Value.string_of_value v);
  let jv = jsonize_value' v in
  Debug.if_set show_json (fun () -> "jsonize_value <= " ^ jv);
  jv


let encode_continuation (cont : Value.continuation) : string =
  Value.marshal_continuation cont

let jsonize_call s cont name args =
  let vs = jsonize_values args in
  let v =
    "{\"__continuation\":\"" ^ (encode_continuation cont) ^"\"," ^
    "\"__name\":\"" ^ name ^ "\"," ^
    "\"__args\":[" ^ String.concat ", " vs ^ "]}" in
  value_with_state v (JsonState.to_string s)
