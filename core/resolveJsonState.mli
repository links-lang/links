(** Utilities for resolving process-based state into a JSON state *)
open ProcessTypes
open Json

(* Adds the event handlers found in the given Value.t. Pure. *)
val add_value_information : Value.t -> json_state -> json_state

(* Adds access point IDs found in the given Value.t.
 * SIDE-EFFECTING! Marks all pending APs as being delivered. *)
val add_ap_information : client_id -> json_state -> json_state

(* Adds process information for processes that have been marked to be spawned
 * by the server (i.e. created by spawnClient) but not yet spawned, and event
 * handlers which have been created, but not yet activated on the client.
 * SIDE-EFFECTING! Pops from the client mailbox. *)
val add_process_information : client_id -> json_state -> json_state

val add_channel_information : client_id -> json_state -> json_state
