open Utility

type module_info = {
    simple_name : string; (* Note: not fully-qualified *)
    inner_modules : string list;
    type_names : string list;
    decl_names : string list
  }

type term_shadow_table = string list stringmap
type type_shadow_table = string list stringmap
type shadow_table = string list stringmap

val module_sep : string
val try_parse_file : string -> (Sugartypes.program * Scanner.position_context)
val contains_modules : Sugartypes.program -> bool
val separate_modules : Sugartypes.binding list -> (Sugartypes.binding list * Sugartypes.binding list)
val get_ffi_files : Sugartypes.program -> string list
val shadow_open : string -> string -> module_info stringmap -> term_shadow_table -> type_shadow_table -> (term_shadow_table * type_shadow_table)
val shadow_binding : string -> string -> (string list) stringmap -> string list stringmap
val create_module_info_map : Sugartypes.program -> module_info stringmap
val lst_to_path : string list -> string
val make_path_string : string list -> string -> string
