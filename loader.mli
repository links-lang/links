val load_file : Types.typing_environment -> string -> (Types.typing_environment * Syntax.program)
val print_cache : string -> unit
val precompile : Types.typing_environment -> string -> string -> unit
val precompile_cache : Types.typing_environment -> string -> unit
