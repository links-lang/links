module type LIB_TYPING_INFO =
sig

(* The semantics of these functions is equivalent
   to their counterpart in the Lib module *)

  val primitive_name : Var.var -> string

  val is_primitive_var : Var.var -> bool

  val is_pure_primitive : string -> bool

end
