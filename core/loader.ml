type 'a result =
  { program_ : 'a;
    context: Context.t }

let load : Context.t -> string -> Sugartypes.program result
  = fun context filename ->
  let root = Filename.dirname filename in
  let program, pos_context =
    ModuleUtils.try_parse_file ~root:"" filename
  in
  let program = (ResolvePositions.resolve_positions pos_context)#program program in
  let program = Chaser.add_dependencies root program in
  let context' = Context.{ context with source_code = pos_context } in
  { context   = context';
    program_  = program }
