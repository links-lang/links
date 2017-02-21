open Proc
open Json
open Utility

type handler_id_set = IntSet.t
let empty_state = IntSet.empty

(* Given a value, extracts the event handlers that need to be sent to the client *)
let rec event_handlers_from_value : Value.t -> handler_id_set =
  function
  (* Can't-dos *)
  | `PrimitiveFunction _ | `Socket _ | `Continuation _ as r ->
      failwith ("Can't create json state for " ^ Value.string_of_value r);

  (* Empties *)
  | `List [] | `SpawnLocation _ | `Pid _
  | `AccessPointID _ | `ClientFunction _ | `SessionChannel _ -> empty_state

  (* Homomorphisms *)
  | `FunctionPtr (_f, fvs) ->
    begin
      match fvs with
        | None     -> empty_state
        | Some fvs -> event_handlers_from_value fvs
    end
  | #Value.primitive_value as p -> event_handlers_from_primitive p
  | `Variant (_label, value) -> event_handlers_from_value value
  | `Record fields ->
    let _ls, vs = List.split fields in
    event_handlers_from_values vs
  | `List (elems) -> event_handlers_from_values elems
  and event_handlers_from_primitive : Value.primitive_value -> handler_id_set = function
  (* Everything is empty except XML items *)
  | `XML xmlitem -> event_handlers_from_xml xmlitem
  | _ -> empty_state
and event_handlers_from_xml = function
  | Value.Text _ -> empty_state
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
            let state' = event_handlers_from_xml xmlitem in
            IntSet.union state state') xml empty_state
  | Value.Attr _ -> assert false
and event_handlers_from_values (vs : Value.t list) : handler_id_set =
    List.fold_left
        (fun set_acc v ->
           let v_handlers = event_handlers_from_value v in
           IntSet.union set_acc v_handlers) IntSet.empty vs

(* Derp, I don't think I need this.
let empty_ap_state = AccessPointIDSet.empty
let aps_from_value : client_id -> Value.t -> apid_set = fun cid v ->

  let rec aps_from_value_inner : Value.t -> apid_set = function
    (* Only interesting one *)
    | `AccessPointID (`ClientAccessPoint (ap_cid, apid))
        when (ClientID.equal cid ap_cid) -> AccessPointIDSet.singleton apid
    | `AccessPointID _ -> empty_ap_state
    (* Can't-dos *)
    | `PrimitiveFunction _ | `Socket _ | `Continuation _ as r ->
        failwith ("Can't create json state for " ^ Value.string_of_value r);

    (* Empties *)
    | `List [] | `SpawnLocation _ | `Pid _
    | `ClientFunction _ | `SessionChannel _ -> empty_ap_state
    | #Value.primitive_value -> empty_ap_state

    (* Homomorphisms *)
    | `FunctionPtr (_f, fvs) ->
      begin
        match fvs with
          | None     -> empty_ap_state
          | Some fvs -> aps_from_value_inner fvs
      end
    | `Variant (_label, value) -> aps_from_value_inner value
    | `Record fields ->
      let _ls, vs = List.split fields in
      aps_from_values vs
    | `List (elems) -> aps_from_values elems
  and aps_from_values (vs : Value.t list) : apid_set =
      List.fold_left
          (fun set_acc v ->
             let v_aps = aps_from_value_inner v in
             AccessPointIDSet.union set_acc v_aps) (AccessPointIDSet.empty) vs in
  (* toplevel *)
  aps_from_value_inner v
*)

(* External interface *)
let add_val_event_handlers v json_state =
  let handler_id_list = IntSet.elements @@ event_handlers_from_value v in
  List.fold_left (fun state_acc h_id ->
    JsonState.add_event_handler h_id (EventHandlers.find h_id) state_acc
  ) json_state handler_id_list


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
    JsonState.add_process pid proc msgs state_acc
  ) json_state pending_processes
