(* Renaming stack. We either have a variable binding or an "open" statement.
 * If we have a variable binding after an "open" statement, then this should
 * take priority over the variables contained within the opened module.
 * Stack frames persist for a single scope (i.e. `Block) *)
type binding_stack_node = [
  | `OpenStatement of string
  | `LocalVarBinding of string
]

val module_sep : string
val print_stack_node : binding_stack_node -> string
val print_module_stack : binding_stack_node list -> string
val moduleInScope : Utility.StringSet.t -> binding_stack_node list -> string -> string option
val prefixWith : string -> string -> string

