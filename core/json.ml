(* Side-effect-free JSON operations. *)
open ProcessTypes
open CommonTypes
open Utility

(* Setting *)
let show_json
  = Settings.(flag "show_json"
              |> convert parse_bool
              |> sync)


(* Type synonyms *)
type handler_id = int
type websocket_url = string

(* Types *)
type json_string = string

let nil_literal = `Null

let lit : ?tag:string -> (string * Yojson.Basic.t) list -> Yojson.Basic.t
  = fun ?tag fields ->
  match tag with
  | None -> `Assoc fields
  | Some tag -> `Assoc (("_tag", `String tag) :: fields)

(* Helper functions for jsonization *)
(*
  SL:
    Having implemented jsonisation of database values, I'm now
    unsure if this is what we really want. From a security point
    of view it certainly isn't a very good idea to pass this kind of
    information to the client.
*)
let json_of_db (db, params) : Yojson.Basic.t =
  let driver = db#driver_name() in
  let (name, args) = Value.parse_db_string params in
  `Assoc [("_db",
      `Assoc [("driver", `String driver);
              ("name", `String name);
              ("args", `String args)])]

(*
WARNING:
  May need to be careful about free type variables / aliases in row
*)
let json_of_table Value.{
  Table.database = (db, params); name; keys; temporality;
  temporal_fields; row } : Yojson.Basic.t =

  let json_of_key k = `List (List.map (fun x -> `String x) k) in
  let json_of_keylist ks = `List (List.map json_of_key ks)  in
  let tmp =
    match temporality with
      | Temporality.Current -> "current"
      | Temporality.Transaction -> "current"
      | Temporality.Valid -> "current"
  in
  let tmp_fields =
    match temporal_fields with
      | Some (from_field, to_field) ->
          [("temporal_fields",
              `Assoc [
                ("from_field", `String from_field);
                ("to_field",   `String to_field)
              ])]
      | None -> []
  in
  let table_assoc =
    [
        ("db", json_of_db (db, params));
        ("name", `String name);
        ("temporality", `String tmp);
        ("row", `String (Types.(string_of_datatype (Record (Row row)))));
        ("keys", json_of_keylist keys)
    ] @ tmp_fields
  in
  `Assoc [("_table", `Assoc table_assoc)]

let json_of_lens (db, lens) : Yojson.Basic.t =
  let db =
    let open Lens.Database in
    db.serialize () in
  let l = Lens.Value.serialize lens in
  lit ~tag:"Lens" [ ("_lens", lit [ ("db", `String db);
                                    ("lens", `String l) ]) ]

let jsonize_location loc = `String (Location.to_string loc)

let rec cons_listify : Yojson.Basic.t list -> Yojson.Basic.t = function
  | [] -> lit ~tag:"List" []
  | x::xs -> lit ~tag:"List" [("_head", x); ("_tail", cons_listify xs)]

let rec jsonize_value' : Value.t -> Yojson.Basic.t =
  function
  | `Lens dl -> json_of_lens dl
  | `PrimitiveFunction _
  | `Resumption _
  | `Continuation _
  | `Socket _
      as r ->
      raise (Errors.runtime_error ("Can't jsonize " ^ Value.string_of_value r));

  | `FunctionPtr (f, fvs) ->
    let (_, _, _, location) = Tables.find Tables.fun_defs f in
    let location = jsonize_location location in
    let fields = [ ("func", `String (Js.var_name_var f));
                   ("location", location) ]
    in
    let fields' =
      match fvs with
      | None     -> fields
      | Some fvs -> ("environment", jsonize_value' fvs) :: fields in
    lit ~tag:"FunctionPtr" fields'
  | `ClientDomRef i ->
     lit ~tag:"ClientDomRef" [("_domRefKey", `String (string_of_int i))]
  | `ClientFunction name -> lit ~tag:"ClientFunction" [("func", `String name)]
  | `ClientClosure index -> lit ~tag:"ClientClosure" [("index", `Int index)]
  | #Value.primitive_value as p -> jsonize_primitive p
  | `Variant (label, value) ->
     lit ~tag:"Variant" [("_label", `String label); ("_value", jsonize_value' value)]
  | `Record fields ->
     lit ~tag:"Record" (List.map (fun (k, v) -> (k, jsonize_value' v )) fields)
  | `List l ->  cons_listify (List.map jsonize_value' l)
  | `AccessPointID (`ClientAccessPoint (cid, apid)) ->
     lit ~tag:"ClientAccessPoint" [ ("_clientAPID", AccessPointID.to_json apid);
                                     ("_clientId", ClientID.to_json cid) ]
  | `AccessPointID (`ServerAccessPoint (apid)) ->
     lit ~tag:"ServerAccessPoint" [ ("_serverAPID", AccessPointID.to_json apid) ]
  | `Pid (`ClientPid (client_id, process_id)) ->
     lit ~tag:"ClientPid" [ ("_clientPid", ProcessID.to_json process_id);
                            ("_clientId", ClientID.to_json client_id) ]
  | `Pid (`ServerPid (process_id)) ->
     lit ~tag:"ServerPid"[ ("_serverPid", ProcessID.to_json process_id) ]
  | `SessionChannel (ep1, ep2) ->
     lit ~tag:"SessionChannel" [ ("_sessEP1", ChannelID.to_json ep1);
                                 ("_sessEP2", ChannelID.to_json ep2) ]
  | `SpawnLocation (`ClientSpawnLoc client_id) ->
     lit ~tag:"ClientSpawnLoc" [ ("_clientSpawnLoc", ClientID.to_json client_id) ]
  | `SpawnLocation (`ServerSpawnLoc) ->
     lit ~tag:"ServerSpawnLoc" [ ("_serverSpawnLoc", `List []) ]
  | `Alien -> raise (Errors.runtime_error "Can't jsonize alien")
and jsonize_primitive : Value.primitive_value -> Yojson.Basic.t  = function
  | `Bool value -> lit ~tag:"Bool" [ ("_value", `Bool value) ]
  | `Int value -> lit ~tag:"Int"  [ ("_value", `Int value) ]
  | `Float value -> lit ~tag:"Float" [ ("_value", `Float value) ]
  | `Char c -> lit ~tag:"Char" [ ("_c", `String (String.make 1 c)) ]
  | `Database db -> lit ~tag:"Database" [ ("_value", json_of_db db) ]
  | `Table t -> lit ~tag:"Table" [ ("_value", json_of_table t) ]
  | `XML xmlitem -> lit ~tag:"XML" [ ("_value", json_of_xmlitem xmlitem) ]
  | `String s -> lit ~tag:"String" [ ("_value", `String s) ]
  | `DateTime (Timestamp.Infinity) ->
          lit ~tag:"DateTime" [ ("_type", `String "infinity") ]
  | `DateTime (Timestamp.MinusInfinity) ->
          lit ~tag:"DateTime" [ ("_type", `String "-infinity") ]
  (* NOTE: An important invariant that it's only ever the *UTC* timestamp
     that is transferred between client and server. *)
  | `DateTime (Timestamp.Timestamp ts) ->
      let utc_timestamp =
        int_of_float (UnixTimestamp.of_calendar ts) in
      lit ~tag:"DateTime"
        [ ("_type", `String "timestamp");
          ("_value", `Int utc_timestamp) ]
and json_of_xmlitem = function
  | Value.Text s -> lit ~tag:"Text" [("type", `String "TEXT"); ("text", `String s)]
  (* TODO: check that we don't run into problems when HTML containing
     an event handler is copied *)
  | Value.NsNode (ns, tag, xml) ->
      let attrs, body =
        List.fold_right (fun xmlitem (attrs, body) ->
            match xmlitem with
            | Value.Attr (label, value) ->
                (label, `String value) :: attrs, body
            | Value.NsAttr (ns, label, value) ->
                (ns ^ ":" ^ label, `String value) :: attrs, body
            | _ ->
              let s = json_of_xmlitem xmlitem in
              attrs, s :: body) xml ([], [])
      in
      let assocKeys = [
          ("type", `String "ELEMENT");
          ("tagName", `String tag);
          ("attrs", `Assoc attrs);
          ("children", cons_listify body)]
      in
      lit ~tag:"NsNode" (if (String.length ns > 0)
                         then ("namespace", `String ns) :: assocKeys
                         else assocKeys)
  | Value.Node (name, children) -> json_of_xmlitem (Value.NsNode ("", name, children))
  | _ -> raise (Errors.runtime_error "Cannot jsonize a detached attribute.")

and jsonize_values : Value.t list -> Yojson.Basic.t list  =
  fun vs ->
    let ss =
      List.fold_left
        (fun ss v ->
           let s = jsonize_value' v in
           s::ss) [] vs in
    List.rev ss

let show_processes procs =
  (* Show the JSON for a prcess, including the PID, process to be run, and mailbox *)
  let show_process (pid, (proc, msgs)) =
    let ms = `List (List.map jsonize_value' msgs) in
    lit ~tag:"Process"
      [ ("pid", ProcessID.to_json pid);
        ("process", jsonize_value' proc);
        ("messages", ms) ]
  in
  let bnds = PidMap.bindings procs in
  `List (List.map show_process bnds)

let show_handlers evt_handlers =
  (* Show the JSON for an event handler: the evt handler key, and the associated process(es) *)
  let show_evt_handler (key, proc) =
    (* If the list of processes handling each key is represented by a 'List term, we translate it to a
       JS Array. This Array is supposed to be processes  by jslib code only*)
    let jsonize_handler_list = function
      | `List elems -> cons_listify (List.map jsonize_value' elems)
      | _ ->  jsonize_value' proc
    in
    (* TODO(dhil): We ought to tag the collection of event
       handlers. Currently, this structure is handled specially by the
       server value resolution algorithm in jslib. *)
    lit [ ("key", `Int key); ("eventHandlers", jsonize_handler_list proc) ]
  in
  let bnds = IntMap.bindings evt_handlers in
  `List (List.map show_evt_handler bnds)

let show_aps aps =
  let aps_json =
    List.map AccessPointID.to_json (AccessPointIDSet.elements aps) in
  `List aps_json

let show_buffers bufs =
  let bufs =
    List.map (fun (endpoint_id, values) ->
        let json_values = `List (List.rev (List.map jsonize_value' values)) in
        (* TODO(dhil): Currently unclear whether we need to tag
           buffers. *)
        lit [ ("buf_id", ChannelID.to_json endpoint_id);
              ("values", json_values) ])
      (ChannelIDMap.bindings bufs)
  in
  `List bufs

let serialise_json_state client_id conn_url procs handlers aps bufs =
  let assoc_keys = [
    ("client_id", ClientID.to_json client_id);
    ("access_points", show_aps aps);
    ("buffers", show_buffers bufs);
    ("processes", show_processes procs);
    ("handlers", show_handlers handlers) ]
  in
  lit (match conn_url with
       | None -> assoc_keys
       | Some url -> ("ws_conn_url", `String url) :: assoc_keys)

(* JSON state definition *)
module JsonState = struct
  type t = {
    client_id : client_id;
    ws_conn_url : websocket_url option;
    processes: (Value.t * Value.t list) pid_map;
    buffers : Value.t list channel_id_map;
    channels : Value.chan list;
    handlers: Value.t intmap;
    aps: apid_set
  }

  (** Creates an empty JSON state *)
  let empty cid url = {
    client_id = cid;
    ws_conn_url = url;
    processes = PidMap.empty;
    buffers = ChannelIDMap.empty;
    channels = [];
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

  (** Adds a buffer to the state *)
  let add_buffer chan_id buf state =
    { state with buffers = ChannelIDMap.add chan_id buf state.buffers }

  let add_carried_channel chan state =
    { state with channels = chan :: state.channels }

  let get_carried_channels state = state.channels

  (** Serialises the state as a JSON string *)
  let to_json s = serialise_json_state s.client_id s.ws_conn_url s.processes s.handlers s.aps s.buffers

  let _merge s s' =
    let select_left _ x _ = Some x in
    let processes = PidMap.union select_left s.processes s'.processes in
    let buffers   = ChannelIDMap.union select_left s.buffers s'.buffers in
    let channels  =
      List.fold_left
        (fun acc chan ->
          (* make sure each channel only appears once *)
          chan :: List.filter (fun chan' -> chan <> chan') acc)
        s.channels s'.channels
    in
    let handlers = IntMap.union select_left s.handlers s'.handlers in
    (* TODO: access points *)
    let aps = AccessPointIDSet.union s.aps s'.aps in
    { s with processes = processes; buffers = buffers; channels = channels; handlers = handlers; aps = aps }
end

type json_state = JsonState.t

let value_with_state v s =
  lit [ ("value", v); ("state", JsonState.to_json s) ]

(* External interface *)
let jsonize_value_with_state value state =
  Debug.if_set show_json
      (fun () -> "jsonize_value_with_state => " ^ Value.string_of_value value);
  let jv = jsonize_value' value in
  let jv_s = value_with_state jv state in
  let jv_str = Yojson.Basic.to_string jv_s in
  Debug.if_set show_json (fun () -> "jsonize_value_with_state <= " ^ jv_str);
  jv_s

let jsonize_value v =
  Debug.if_set show_json
      (fun () -> "jsonize_value => " ^ Value.string_of_value v);
  let jv = jsonize_value' v in
  let jv_str = Yojson.Basic.to_string jv in
  Debug.if_set show_json (fun () -> "jsonize_value <= " ^ jv_str);
  jv

let jsonize_call s cont name args =
  let arg_vs = jsonize_values args in
  let v =
    `Assoc [
      ("__continuation", `String cont);
      ("__name", `String name);
      ("__args", `List arg_vs)] in
  value_with_state v s

(* Eta expansion needed to suppress optional arguments *)
let json_to_string json = Yojson.Basic.to_string json
