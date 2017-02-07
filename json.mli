(* Exported types. It would be nice to abstract these away. *)
type json_state = {
  processes : ProcessTypes.pid_set;
  handlers : Utility.IntSet.t
}

(* Parsing *)
val parse_json : string -> Value.t
val parse_json_b64 : string -> Value.t

(* Jsonization *)
val jsonize_value : RequestData.request_data -> Value.t -> string
val jsonize_call :
  RequestData.request_data -> Value.continuation -> string -> Value.t list -> string
val jsonize_state : RequestData.request_data -> Value.t -> json_state
val jsonize_value_with : RequestData.request_data -> json_state -> Value.t -> string * json_state
val resolve_state : RequestData.request_data -> json_state -> string
