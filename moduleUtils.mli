open Utility

type module_info = {
    simple_name : string; (* Note: not fully-qualified *)
    inner_modules : string list;
    type_names : string list;
    decl_names : string list
  }


val module_sep : string
val try_parse_file : string -> (Sugartypes.program * Parse.position_context)
val contains_modules : Sugartypes.program -> bool
val separate_modules : Sugartypes.binding list -> (Sugartypes.binding list * Sugartypes.binding list)
val shadow_open_types : string -> string -> module_info stringmap -> (string list stringmap) -> (string list stringmap)
val shadow_open_terms : string -> string -> module_info stringmap -> (string list stringmap) -> (string list stringmap)
val shadow_binding : string -> string -> (string list) stringmap -> (string list stringmap)
val create_module_info_map : Sugartypes.program -> module_info stringmap
val lst_to_path : string list -> string
val make_path_string : string list -> string -> string
