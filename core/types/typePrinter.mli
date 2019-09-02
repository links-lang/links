open TypesBase
open CommonTypes

module Vars : sig
  type flavour = [`Rigid | `Flexible | `Recursive]
  type kind    = PrimaryKind.t
  type scope   = [`Free | `Bound]
  type vars_list = (int * (flavour * kind * scope)) list
  type spec    = flavour * kind * int

end


(** get type variables *)
val free_type_vars : datatype -> TypeVarSet.t
val free_row_type_vars : row -> TypeVarSet.t
val free_tyarg_vars : type_arg -> TypeVarSet.t
val free_bound_type_vars          : typ      -> Vars.vars_list
val free_bound_row_type_vars      : row      -> Vars.vars_list
val free_bound_type_arg_type_vars : type_arg -> Vars.vars_list

module Print : sig
  type policy = {quantifiers:bool; flavours:bool; hide_fresh:bool; kinds:string; effect_sugar:bool}

  val default_policy : unit -> policy
end

(** generating type variable names *)
val build_tyvar_names : refresh_tyvar_names:bool
                     -> ('a -> Vars.vars_list)
                     -> ('a list)
                     -> unit
val add_tyvar_names : ('a -> Vars.vars_list)
                   -> ('a list)
                   -> unit


(** pretty printing *)
val string_of_datatype   : ?policy:(unit -> Print.policy)
                        -> ?refresh_tyvar_names:bool -> datatype   -> string
val string_of_row        : ?policy:(unit -> Print.policy)
                        -> ?refresh_tyvar_names:bool -> row        -> string
val string_of_presence   : ?policy:(unit -> Print.policy)
                        -> ?refresh_tyvar_names:bool -> field_spec -> string
val string_of_type_arg   : ?policy:(unit -> Print.policy)
                        -> ?refresh_tyvar_names:bool -> type_arg   -> string
val string_of_row_var    : ?policy:(unit -> Print.policy)
                        -> ?refresh_tyvar_names:bool -> row_var    -> string
val string_of_tycon_spec : ?policy:(unit -> Print.policy)
                        -> ?refresh_tyvar_names:bool -> tycon_spec -> string
val string_of_environment        : environment -> string
val string_of_typing_environment : typing_environment -> string

val string_of_quantifier : ?refresh_tyvar_names:bool -> quantifier   -> string
