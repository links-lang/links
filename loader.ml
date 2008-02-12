open Utility
open Performance

let `T (pos, dt, _) = Syntax.no_expr_data

let identity x = x

let expunge_source_pos =
  (Syntax.Functor_expression'.map
     (fun (`T (_,_,l)) -> `T (pos, dt, l)))

let expunge_all_source_pos =
  Syntax.transform_program expunge_source_pos

let newer f1 f2 = 
   ((Unix.stat f1).Unix.st_mtime > (Unix.stat f2).Unix.st_mtime) 
  
let make_cache = true

let read_file_cache : string -> (Types.typing_environment * Syntax.program) = fun filename ->
  let cachename = filename ^ ".cache" in
    try
      if make_cache && newer cachename filename then
        call_with_open_infile cachename ~binary:true
          (fun cachefile ->
             (Marshal.from_channel cachefile 
                : (Types.typing_environment * Syntax.program)))
      else
        (Debug.print("No precompiled " ^ filename);
         raise (Sys_error "Precompiled source file out of date."))
    with (Sys_error _| Unix.Unix_error _) ->
      let sugar, pos_context = measure "parse" (Parse.parse_file ~pp:(Settings.get_value Basicsettings.pp) Parse.program) filename in
      let program, _, tenv = Frontend.Pipeline.program Library.typing_env pos_context sugar in
      let program = Sugar.desugar_program program in
      let env, program = measure "type" (Inference.type_program Library.typing_env) program in
      let program = measure "optimise" Optimiser.optimise_program (env, program) in
      let program = Syntax.labelize program
      in 
	(try (* try to write to the cache *)
           call_with_open_outfile cachename ~binary:true
             (fun cachefile ->
                Marshal.to_channel cachefile 
                  (* Serialise the typing environment returned from
                     sugar typing (which includes alias bindings), not
                     the typing environment returned from
                     Inference.type_program (which doesn't) *)
                  (tenv, expunge_all_source_pos program
                     : Types.typing_environment * Syntax.program)
                  [])
	 with _ -> ()) (* Ignore errors writing the cache file *);
        tenv, program
  
let dump_cached filename =
   let _env, program = read_file_cache filename in
     print_string (Syntax.labelled_string_of_program program)
