open Result

(* database config *)
val database_driver : string Settings.setting
val database_args : string Settings.setting

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
(*type continuationized_val = [
  result
| `PFun of (continuation -> result -> result) * continuation * result list -> continuationized_val
]*)

type primitive = [
  result
| `PFun of result list -> result
]
val primitive_names : string list
val is_primitive : string -> bool
val is_pure_primitive : string -> bool
val value_env : primitive option Utility.StringMap.t ref
val type_env : Types.environment
val alias_env : Types.alias_environment
val typing_env : Types.typing_environment
val apply_pfun : string -> result list -> result
val primitive_stub : string -> result

val primitive_location : string -> Syntax.location
val primitive_arity : string -> int option
val cgi_parameters : (string * string) list ref 

val print_http_response : (string * string) list -> string -> unit
