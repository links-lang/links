open Result

(* Process management *)
type pid
type proc_state = continuation * result
val main_process_pid : pid
val suspended_processes : (proc_state * pid) Queue.t
val blocked_processes : (pid, proc_state * pid) Hashtbl.t
val messages : (pid, result Queue.t) Hashtbl.t
val current_pid : pid ref
val debug_process_status : unit -> unit

val http_response_headers : (string * string) list ref
val http_response_code : int ref

val equal : Result.result -> Result.result -> bool
val less : Result.result -> Result.result -> bool
val less_or_equal : Result.result -> Result.result -> bool

(* Primitive functions and values *)
type continuationized_val = [
  result
| `PFun of (continuation -> result -> result) * continuation * result -> continuationized_val
]
val value_env : (string * continuationized_val) list ref
val type_env : Inferencetypes.environment
val alias_env : Inferencetypes.alias_environment
val typing_env : Inferencetypes.environment * Inferencetypes.alias_environment
val apply_pfun : (continuation -> result -> result) -> continuation -> string -> result list -> result
val primitive_stub : string -> result

val primitive_location : string -> Syntax.location
val cgi_parameters : (string * string) list ref 
