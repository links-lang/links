(* Process management *)
type pid
type proc_state = Value.continuation * Value.t
val main_process_pid : pid
val suspended_processes : (proc_state * pid) Queue.t
val blocked_processes : (pid, proc_state * pid) Hashtbl.t
val messages : (pid, Value.t Queue.t) Hashtbl.t
val current_pid : pid ref
val debug_process_status : unit -> unit

val http_response_headers : (string * string) list ref
val http_response_code : int ref

val equal : Value.t -> Value.t -> bool
val less : Value.t -> Value.t -> bool
val less_or_equal : Value.t -> Value.t -> bool

type primitive

val primitive_names : string list
val is_primitive : string -> bool
val is_pure_primitive : string -> bool
val value_env : primitive option Env.Int.t ref
val type_env : Types.environment
val typing_env : Types.typing_environment
val nenv : Var.var Env.String.t
val prelude_env : Types.typing_environment option ref
val apply_pfun : string -> Value.t list -> Value.t
val primitive_stub : Var.var -> Value.t

val primitive_name : Var.var -> string
val primitive_location : string -> Syntax.location
val primitive_arity : string -> int option
val cgi_parameters : (string * string) list ref 

val print_http_response : (string * string) list -> string -> unit

val primitive_names : string list
