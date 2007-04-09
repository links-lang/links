open Utility
open Performance

let `T (pos, dt, _) = Syntax.no_expr_data

let identity x = x

let expunge_source_pos =
  (Syntax.Functor_expression'.map
     (fun (`T (_,_,l)) -> `T (pos, dt, l)))

let expunge_all_source_pos =
  List.map expunge_source_pos

let newer f1 f2 = 
   ((Unix.stat f1).Unix.st_mtime > (Unix.stat f2).Unix.st_mtime) 
  
let make_cache = true

let read_file_cache filename : (Types.typing_environment * Syntax.expression list) = 
  let cachename = filename ^ ".cache" in
    try
      if make_cache && newer cachename filename then
        call_with_open_infile cachename ~binary:true
          (fun cachefile ->
             (Marshal.from_channel cachefile 
                : (Types.typing_environment *Syntax.expression list)))
          (* (OCaml manual recommends putting a type signature on unmarshal 
             calls; not clear whether this actually helps. It seems we get 
             a segfault if the marhsaled data is not the right type.) *)
      else
        (Debug.print("No precompiled " ^ filename);
         raise (Sys_error "Precompiled source file out of date."))
    with (Sys_error _| Unix.Unix_error _) ->
      let exprs = measure "parse" (Parse.parse_file Parse.program) filename in
      let env, exprs = measure "type" (Inference.type_program Library.typing_env) exprs in
      let exprs = measure "optimise" Optimiser.optimise_program (env, exprs)in
      let env, exprs =
        env, List.map Syntax.labelize exprs 
      in 
	(try (* try to write to the cache *)
           call_with_open_outfile cachename ~binary:true
             (fun cachefile ->
                Marshal.to_channel cachefile 
                  (env, (expunge_all_source_pos exprs))
                  [Marshal.Closures])
	 with _ -> ()) (* Ignore errors writing the cache file *);
        env, exprs
  
let dump_cached filename = 
  let env, exprs = read_file_cache filename in
    print_string (Utility.mapstrcat "\n" Syntax.labelled_string_of_expression exprs)
