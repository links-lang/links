include TypesBase

module Vars = FreeTypeVars
module Print = TypePrinter


(* We replace some of the generated printing functions here such that
   they may use our own printing functions instead. If the generated functions are
   to be used, we remove potential cycles arising from recursive types/rows first.
   They are here because they are needed
   by the generated code for printing the IR, do not call them yourself.
   Use string_of_* instead *)

module DecycleTypes  =
struct
  let elim_recursive_type_cycles_visitor = new ElimRecursiveTypeCyclesTransform.visitor

  let datatype t = fst (elim_recursive_type_cycles_visitor#typ t)
  let row r = fst (elim_recursive_type_cycles_visitor#row r)
  let field_spec p = fst (elim_recursive_type_cycles_visitor#field_spec p)
  let type_arg ta = fst (elim_recursive_type_cycles_visitor#type_arg ta)
  let row_var rv = fst (elim_recursive_type_cycles_visitor#row_var rv)
  let quantifier q = fst (elim_recursive_type_cycles_visitor#quantifier q)

end

let pp_datatype : Format.formatter -> datatype -> unit = fun fmt t ->
  if Settings.get_value Basicsettings.print_types_pretty then
    Format.pp_print_string fmt (Print.string_of_datatype t)
  else
    raw_pp_datatype fmt (DecycleTypes.datatype t)
let pp_quantifier : Format.formatter -> quantifier -> unit = fun fmt t ->
  if Settings.get_value Basicsettings.print_types_pretty then
    Format.pp_print_string fmt (Print.string_of_quantifier t)
  else
    pp_quantifier fmt (DecycleTypes.quantifier t)
let show_quantifier : quantifier -> string = (fun x -> Format.asprintf "%a" pp_quantifier x)
let pp_type_arg : Format.formatter -> type_arg -> unit = fun fmt t ->
  if Settings.get_value Basicsettings.print_types_pretty then
    Format.pp_print_string fmt (Print.string_of_type_arg t)
  else
    pp_type_arg fmt (DecycleTypes.type_arg t)
let pp_tycon_spec : Format.formatter -> tycon_spec -> unit = fun fmt t ->
  let decycle_tycon_spec = function
    | `Alias (qlist, ty) -> `Alias (List.map DecycleTypes.quantifier qlist, DecycleTypes.datatype ty)
    | other -> other in

  if Settings.get_value Basicsettings.print_types_pretty then
    Format.pp_print_string fmt (Print.string_of_tycon_spec t)
  else
    raw_pp_tycon_spec fmt (decycle_tycon_spec t)
let pp_row : Format.formatter -> row -> unit = fun fmt t ->
  if Settings.get_value Basicsettings.print_types_pretty then
    Format.pp_print_string fmt (Print.string_of_row t)
  else
    raw_pp_row fmt (DecycleTypes.row t)
