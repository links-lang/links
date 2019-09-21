
open Types

module type TYPE_PRINTER =
sig

  val pp_datatype : Format.formatter -> datatype -> unit
  val pp_quantifier : Format.formatter -> quantifier -> unit
  val pp_type_arg : Format.formatter -> type_arg -> unit
  val pp_row : Format.formatter -> row -> unit

  val string_of_datatype : Types.datatype -> string
  val string_of_field_spec: Types.field_spec -> string
  val string_of_type_arg : Types.type_arg -> string
  val string_of_row : Types.row -> string
  val string_of_quantifier : Types.quantifier -> string
  val string_of_tycon_spec : Types.tycon_spec -> string

end



(* A type printer that uses auto-generated functions *)
module Raw       : TYPE_PRINTER

(* A type printer that uses our own, hand-crafted printing functions *)
module Pretty    : TYPE_PRINTER

(* A printer behaving like one of the two above,
   based on the setting print_types_pretty *)
module BySetting : TYPE_PRINTER




(* A printer similar to Pretty, but exposes more fine-grained control
   over its behavior *)
module PrettyWithPolicy :
sig

type policy = {quantifiers:bool; flavours:bool; hide_fresh:bool; kinds:string; effect_sugar:bool}

val default_policy : unit -> policy

val build_tyvar_names : refresh_tyvar_names:bool
                     -> ('a -> FreeTypeVars.vars_list)
                     -> ('a list)
                     -> unit
val add_tyvar_names : ('a -> FreeTypeVars.vars_list)
                   -> ('a list)
                   -> unit


val string_of_datatype   : ?policy:(unit -> policy)
                        -> ?refresh_tyvar_names:bool -> datatype   -> string
val string_of_row        : ?policy:(unit -> policy)
                        -> ?refresh_tyvar_names:bool -> row        -> string
val string_of_field_spec : ?policy:(unit -> policy)
                        -> ?refresh_tyvar_names:bool -> field_spec -> string
val string_of_type_arg   : ?policy:(unit -> policy)
                        -> ?refresh_tyvar_names:bool -> type_arg   -> string
val string_of_row_var    : ?policy:(unit -> policy)
                        -> ?refresh_tyvar_names:bool -> row_var    -> string
val string_of_tycon_spec : ?policy:(unit -> policy)
                        -> ?refresh_tyvar_names:bool -> tycon_spec -> string
end





(* The modules below provide modules equivalent to the vanilla
  Types module, but with added printing functions.
  You can use them if you want to define a dataype that uses
  types from Types and you want printing functions derived for
  your new types.
  Just shadow Types with one of the following modules then.
*)

module type PRINTABLE_TYPES =
sig

include (module type of Types)
include TYPE_PRINTER

end

module RawPrintableTypes : PRINTABLE_TYPES
module PrettyPrintableTypes : PRINTABLE_TYPES
module BySettingPrintableTypes : PRINTABLE_TYPES
