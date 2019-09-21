

open Types
open CommonTypes



module Raw =
struct


  type 'a point = 'a Unionfind.point [@@deriving show]

  type 't meta_type_var_non_rec_basis =
    [ `Var of (int * subkind * freedom)
    | `Body of 't ]
      [@@deriving show]

  type 't meta_type_var_basis =
    [ 't meta_type_var_non_rec_basis
    | `Recursive of (int * 't) ]
      [@@deriving show]

  type 'r meta_row_var_basis =
    [ 'r meta_type_var_basis | `Closed ]
      [@@deriving show]

  type 't meta_presence_var_basis = 't meta_type_var_non_rec_basis
                                      [@@deriving show]






  type typ               =  [%import: Types.typ]
  and  field_spec        =  [%import: Types.field_spec]
  and  field_spec_map    =  [%import: Types.field_spec_map]
  and  row_var           =  [%import: Types.row_var]
  and  row               =  [%import: Types.row]
  and  meta_type_var     =  [%import: Types.meta_type_var]
  and  meta_row_var      =  [%import: Types.meta_row_var]
  and  meta_presence_var =  [%import: Types.meta_presence_var]
  and  meta_var          =  [%import: Types.meta_var]
  and  quantifier        =  [%import: Types.quantifier]
  and  type_arg          =  [%import: Types.type_arg]
                              [@@deriving show]

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
    pp_datatype fmt (DecycleTypes.datatype t)

  let pp_quantifier : Format.formatter -> quantifier -> unit = fun fmt t ->
    pp_quantifier fmt (DecycleTypes.quantifier t)

  let show_quantifier : quantifier -> string = (fun x -> Format.asprintf "%a" pp_quantifier x)

  let pp_type_arg : Format.formatter -> type_arg -> unit = fun fmt t ->
    pp_type_arg fmt (DecycleTypes.type_arg t)

  let pp_tycon_spec : Format.formatter -> tycon_spec -> unit = fun fmt t ->
    let decycle_tycon_spec = function
      | `Alias (qlist, ty) -> `Alias (List.map DecycleTypes.quantifier qlist, DecycleTypes.datatype ty)
      | other -> other in


    pp_tycon_spec fmt (decycle_tycon_spec t)
  let pp_row : Format.formatter -> row -> unit = fun fmt t ->
    pp_row fmt (DecycleTypes.row t)

end




module Pretty =
struct
end
