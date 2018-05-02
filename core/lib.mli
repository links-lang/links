
val equal : Value.t -> Value.t -> bool
val less : Value.t -> Value.t -> bool
val less_or_equal : Value.t -> Value.t -> bool

type primitive

val primitive_names : string list
val is_primitive : string -> bool
val is_primitive_var : Var.var -> bool
val is_pure_primitive : string -> bool
val value_env : primitive option Env.Int.t
val type_env : Types.environment
val typing_env : Types.typing_environment
val nenv : Var.var Env.String.t
val prelude_tyenv : Types.typing_environment option ref
val prelude_nenv : Var.var Env.String.t option ref

val primitive_vars : Utility.IntSet.t

val patch_prelude_funs : Types.typing_environment -> Types.typing_environment

val apply_pfun : string -> Value.t list -> RequestData.request_data -> Value.t
val primitive_stub : string -> Value.t

(* jcheney: added to avoid string comparisons at runtime *)
val apply_pfun_by_code : Var.var -> Value.t list -> RequestData.request_data -> Value.t
val primitive_stub_by_code : Var.var -> Value.t

val primitive_name : Var.var -> string
val primitive_location : string -> Sugartypes.location
val primitive_arity : string -> int option

val cohttp_server_response : (string * string) list -> string -> RequestData.request_data -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
val print_http_response : (string * string) list -> string -> RequestData.request_data -> unit

val prim_appln : Env.String.name -> Ir.value list -> Ir.tail_computation

