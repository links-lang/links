open Proc
open Json
open Utility

type handler_id_set = IntSet.t
let empty_state = (IntSet.empty, [])

(* Given a value, extracts the event handlers that need to be sent to the client *)
let rec extract_json_values : Value.t -> (handler_id_set * (Value.chan list)) =
  function
  (* Can't-dos *)
  | `PrimitiveFunction _ | `Socket _
  | `Resumption _ | `Continuation _ as r ->
      raise (Errors.runtime_error
        ("Can't create json state for " ^ Value.string_of_value r))

  (* Empties *)
  | `List [] | `SpawnLocation _ | `Pid _
  | `AccessPointID _ | `ClientDomRef _
  | `ClientFunction _ | `ClientClosure _
  |  `Alien -> empty_state

  (* Session channels *)
  | `SessionChannel c -> (IntSet.empty, [c])

  (* Homomorphisms *)
  | `FunctionPtr (_f, fvs) ->
    begin
      match fvs with
        | None     -> empty_state
        | Some fvs -> extract_json_values fvs
    end

  (* Handle lenses similar to primitive values *)
  | `Lens _ -> empty_state

  | #Value.primitive_value as p -> (extract_from_primitive p, [])
  | `Variant (_label, value) -> extract_json_values value
  | `Record fields ->
    let _ls, vs = List.split fields in
    extract_from_values vs
  | `List (elems) -> extract_from_values elems
  and extract_from_primitive : Value.primitive_value -> handler_id_set = function
  (* Everything is empty except XML items *)
  | `XML xmlitem -> extract_from_xml xmlitem
  | _ -> IntSet.empty
and extract_from_xml = function
  | Value.Text _ -> IntSet.empty
  | Value.Node (_tag, xml) ->
      (* Any attribute with the "key" label is an event handler; add to state. *)
      List.fold_right (fun xmlitem state ->
          match xmlitem with
          | Value.Attr (label, value) ->
            if label = "key" then
                let key = int_of_string value in
                IntSet.add key state
            else state
          | _ ->
            let state' = extract_from_xml xmlitem in
            IntSet.union state state') xml IntSet.empty
  | Value.Attr _ -> assert false
  | Value.NsAttr _ -> assert false
  (* Namespace of a tag is not relevant for event handlers, so we can just drop that here *)
  | Value.NsNode (_, name, children) -> extract_from_xml (Value.Node (name, children))

and extract_from_values (vs : Value.t list) : (handler_id_set * Value.chan list) =
    List.fold_left
        (fun (set_acc, chan_acc) v ->
           let (v_handlers, v_chans) = extract_json_values v in
           (IntSet.union set_acc v_handlers, v_chans @ chan_acc)) empty_state vs


(* External interface *)
let add_value_information v json_state =
  let (handler_ids, chans) = extract_json_values v in
  let handler_id_list = IntSet.elements handler_ids in
  let json_state =
    List.fold_left (fun state_acc h_id ->
      JsonState.add_event_handler h_id (EventHandlers.find h_id) state_acc
    ) json_state handler_id_list in
  List.fold_left (fun state_acc chan ->
    JsonState.add_carried_channel chan state_acc) json_state chans



let add_ap_information cid json_state =
  let pending_aps = Session.get_and_mark_pending_aps cid in
  List.fold_left
    (fun state_acc ap_id -> JsonState.add_ap_id ap_id state_acc)
    json_state pending_aps

let add_process_information cid json_state =
  let pending_processes = Proc.get_and_mark_pending_processes cid in
  List.fold_left (fun state_acc (pid, proc) ->
    (* Pop all messages *)
    let msgs = Mailbox.pop_all_messages_for cid pid in
    (* Add any channels from each process / any mailbox messages into the state *)
    let state_acc =
      List.fold_left
      (fun state_acc v -> add_value_information v state_acc) state_acc msgs in
    let state_acc = add_value_information proc state_acc in
    JsonState.add_process pid proc msgs state_acc
  ) json_state pending_processes


let add_channel_information client_id json_state =
  List.fold_left (fun state_acc chan ->
    let (_, local) = chan in
    Session.register_client_channel client_id chan;
    match Session.get_buffer local with
      | Some (buf) -> JsonState.add_buffer local buf state_acc
      | None -> JsonState.add_buffer local [] state_acc
  ) json_state (JsonState.get_carried_channels json_state)

