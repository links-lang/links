
open TypesBase
open CommonTypes


  type flavour = [`Rigid | `Flexible | `Recursive]
  type kind    = PrimaryKind.t
  type scope   = [`Free | `Bound]
  type vars_list = (int * (flavour * kind * scope)) list
  type spec    = flavour * kind * int

  type names  = (int, string * spec) Hashtbl.t

  val find : int -> names -> string
  val find_spec : int -> names -> string * spec

val make_names : vars_list -> names

val tyvar_name_counter : int ref

(** get type variables *)
val free_type_vars : datatype -> TypeVarSet.t
val free_row_type_vars : row -> TypeVarSet.t
val free_tyarg_vars : type_arg -> TypeVarSet.t
val free_bound_type_vars          : typ      -> vars_list
val free_bound_row_type_vars      : row      -> vars_list
val free_bound_type_arg_type_vars : type_arg -> vars_list
