(** Type printers *)
(* Do not use this module directly, but its export via Types.Print *)

open TypesBase
open CommonTypes

module Vars : sig
  type flavour = [`Rigid | `Flexible | `Recursive]
  type kind    = PrimaryKind.t
  type scope   = [`Free | `Bound]
  type vars_list = (int * (flavour * kind * scope)) list
  type spec    = flavour * kind * int

end


  type policy = {quantifiers:bool; flavours:bool; hide_fresh:bool; kinds:string; effect_sugar:bool}

  val default_policy : unit -> policy

(** generating type variable names *)
val build_tyvar_names : refresh_tyvar_names:bool
                     -> ('a -> Vars.vars_list)
                     -> ('a list)
                     -> unit
val add_tyvar_names : ('a -> Vars.vars_list)
                   -> ('a list)
                   -> unit


(** pretty printing *)
val string_of_datatype   : ?policy:(unit -> policy)
                        -> ?refresh_tyvar_names:bool -> datatype   -> string
val string_of_row        : ?policy:(unit -> policy)
                        -> ?refresh_tyvar_names:bool -> row        -> string
val string_of_presence   : ?policy:(unit -> policy)
                        -> ?refresh_tyvar_names:bool -> field_spec -> string
val string_of_type_arg   : ?policy:(unit -> policy)
                        -> ?refresh_tyvar_names:bool -> type_arg   -> string
val string_of_row_var    : ?policy:(unit -> policy)
                        -> ?refresh_tyvar_names:bool -> row_var    -> string
val string_of_tycon_spec : ?policy:(unit -> policy)
                        -> ?refresh_tyvar_names:bool -> tycon_spec -> string

val string_of_quantifier : ?policy:(unit -> policy)
                        -> ?refresh_tyvar_names:bool -> quantifier -> string
