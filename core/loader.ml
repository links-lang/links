type 'a result =
  { program_ : 'a;
    context: Context.t }

let load : Context.t -> string -> Sugartypes.program result
  = fun context filename ->
  let program, pos_context =
    ModuleUtils.try_parse_file filename
  in
  let context' = Context.{ context with source_code = pos_context } in
  { context   = context';
    program_  = program }



