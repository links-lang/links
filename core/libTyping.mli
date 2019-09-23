(* The semantics of these functions is equivalent
   to their counterpart in the Lib module *)



val primitive_name : Var.var -> string

val is_primitive_var : Var.var -> bool

val is_pure_primitive : string -> bool



(* Only to be called from Lib *)
val set_fun_primitive_name : (Var.var -> string) -> unit
val set_fun_is_primitive_var : (Var.var -> bool) -> unit
val set_fun_is_pure_primitive : (string -> bool) -> unit
