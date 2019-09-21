
open Types

module type TYPE_PRINTER_T =
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




module Raw       : TYPE_PRINTER_T
module Pretty    : TYPE_PRINTER_T
module BySetting : TYPE_PRINTER_T





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





module type PRINTABLE_TYPES =
sig

include (module type of Types)
include TYPE_PRINTER_T

end

module RawPrintableTypes : PRINTABLE_TYPES
module PrettyPrintableTypes : PRINTABLE_TYPES
module BySettingPrintableTypes : PRINTABLE_TYPES
