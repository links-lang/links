val module_sep : string
val try_parse_file : string -> (Sugartypes.program * Parse.position_context)
val contains_modules : Sugartypes.program -> bool
val separate_modules : Sugartypes.binding list -> (Sugartypes.binding list * Sugartypes.binding list)
