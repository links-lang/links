(*pp deriving *)

type primitive =
[ Value.t
| `PFun of RequestData.request_data -> Value.t list -> Value.t ]

type pure = PURE | IMPURE

type located_primitive = [ `Client | `Server of primitive | primitive ]

val datatype : string -> Types.datatype

val equal : Value.t -> Value.t -> bool
val less : Value.t -> Value.t -> bool
val less_or_equal : Value.t -> Value.t -> bool

val p1 : ('a -> 'b) -> [> `PFun of 'c -> 'a list -> 'b ]
val p2 : ('a -> 'a -> 'b) -> [> `PFun of 'c -> 'a list -> 'b ]
val p3 : ('a -> 'a -> 'a -> 'b) -> [> `PFun of 'c -> 'a list -> 'b ]

val env : (string * (located_primitive * Types.datatype * pure)) list ref

val primitive_names : string list ref
val is_primitive : string -> bool
val is_primitive_var : Var.var -> bool
val is_pure_primitive : string -> bool
val value_env : primitive option Env.Int.t ref
val type_env : Types.environment ref
val typing_env : Types.typing_environment ref
val nenv : Var.var Env.String.t ref
val prelude_tyenv : Types.typing_environment option ref
val prelude_nenv : Var.var Env.String.t option ref

val primitive_vars : Utility.IntSet.t ref

val patch_prelude_funs : Types.typing_environment -> Types.typing_environment

val apply_pfun : string -> Value.t list -> RequestData.request_data -> Value.t
val primitive_stub : string -> Value.t

(* jcheney: added to avoid string comparisons at runtime *)
val apply_pfun_by_code : Var.var -> Value.t list -> RequestData.request_data -> Value.t
val primitive_stub_by_code : Var.var -> Value.t

val primitive_name : (Var.var -> string) ref
val primitive_location : string -> Sugartypes.location
val primitive_arity : string -> int option

val cohttp_server_response : (string * string) list -> string -> RequestData.request_data -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
val print_http_response : (string * string) list -> string -> RequestData.request_data -> unit

val prim_appln : Env.String.name -> Ir.value list -> Ir.tail_computation

val re_cal_env : unit -> unit

