(* Detection of free variables in types *)
(* Do not use this module directly, but its export via FreeTypeVars.*)

open Types
open CommonTypes


  type flavour = [`Rigid | `Flexible | `Recursive]
  type kind    = PrimaryKind.t
  type scope   = [`Free | `Bound]
  type vars_list = (int * (flavour * kind * scope)) list
  type spec    = flavour * kind * int

  type names  = (int, string * spec) Hashtbl.t

  val find : int -> names -> string
  val find_spec : int -> names -> string * spec


(** get type variables *)
val free_type_vars : datatype -> TypeVarSet.t
val free_row_type_vars : row -> TypeVarSet.t
val free_tyarg_vars : type_arg -> TypeVarSet.t
val free_bound_type_vars          : typ      -> vars_list
val free_bound_row_type_vars      : row      -> vars_list
val free_bound_type_arg_type_vars : type_arg -> vars_list
val free_bound_field_spec_type_vars : field_spec -> vars_list
val free_bound_row_var_vars         : row_var -> vars_list
val free_bound_tycon_type_vars      : tycon_spec -> vars_list
val free_bound_quantifier_vars      : Quantifier.t -> vars_list
