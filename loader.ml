open Utility
open Performance

let `T (pos, dt, _) = Syntax.no_expr_data

let home_dir = match getenv "HOME" with
    None -> [] | Some dir -> [dir]

let links_exec_dir = Filename.dirname Sys.executable_name

let std_links_libdir = Filename.concat links_exec_dir "lib"
  (* "/usr/lib/links" (* someday!*) *)

let working_dir = Sys.getcwd()

let std_search_path = 
  ([working_dir; std_links_libdir; links_exec_dir] @
     List.map (fun d -> Filename.concat d "lib/links") home_dir)

exception File_not_found of string

(** Search for [path] in all the standard places, and also in [dirs] *)
let resolve_path dirs path = 
  if Filename.is_relative path then
    try
      let search_path = dirs@std_search_path in
        Debug.print("Searching in " ^ String.concat ":" search_path);
        List.hd(List.filter Sys.file_exists
                  (List.map(fun d -> Filename.concat d path) search_path))
    with Failure _ -> raise(File_not_found path)
  else path

let find_dependencies filenames = 
  let rec find_dependencies filenames deps = 
    match filenames with
        [] -> unduplicate (=) deps
      | (filename::filenames) ->
          let (Syntax.Program(defs, _), _) = Parse.parse_file Parse.program filename in
          let new_deps = concat_map
            (function Syntax.Module((Some path), _, _) -> [path]
               | _ -> []) defs
          in find_dependencies filenames (new_deps :: deps)
  in
    find_dependencies filenames []

let expunge_source_pos =
  (Syntax.Functor_expression'.map
     (fun (`T (_,_,l)) -> `T (pos, dt, l)))

let expunge_all_source_pos =
  Syntax.transform_program expunge_source_pos

let newer f1 f2 = 
   ((Unix.stat f1).Unix.st_mtime > (Unix.stat f2).Unix.st_mtime) 
  
let use_caches = Settings.add_bool ("use_caches", true, `User)
let make_caches = Settings.add_bool ("make_caches", true, `User)
let correct_optimised_types = Settings.add_bool ("correct_optimised_types",
                                                 false, `User)

(* let links_bin_timestamp = Sys.executable_name *)

exception No_caching

(** [resolve_includes loaded filename program] replaces each [include]
    directive in [program] with the syntax tree of the corresponding file.
    It refuses to load the file if it is already in the [loaded] list of 
    files (thus preventing circular dependencies--this could be relaxed)
    and uses [filename] as the name of the program [program] in error messages.
*)
let rec resolve_includes loaded filename (program : 'a Syntax.program') = 
  let file_dir = Filename.dirname filename in
  let Syntax.Program(defs, expr) = program in
  let defs = List.map
    (function Syntax.Module(Some path, None, data) ->
       begin try
         let abs_path = resolve_path [file_dir] path in
           if List.mem abs_path loaded then
             failwith("File " ^ abs_path ^ " included from " ^
                        filename ^" is already loaded.");
           let Syntax.Program(defs, expr), _sugar =
             parse_file (abs_path::loaded) abs_path
           in
             if Syntax.is_unit_expr expr then ()
             else failwith ("Included file " ^ path ^
                              " (included from " ^ filename ^
                              ") had a non-trivial body expression.");
             
             Syntax.Module(Some path, Some defs, data)
               (* TBD: include the resolved absolute path in this result. *)
       with File_not_found incl_name ->
            failwith("Couldn't find file "^incl_name^" included from " ^ filename)
         | Sys_error _ ->
            failwith("Couldn't open file "^filename^" included from " ^ filename)
       end
       | decl -> decl) defs in
    Syntax.Program(defs, expr)

and parse_file loaded filename =
  let abs_path = resolve_path [] filename in
  let program, sugar = 
    Parse.parse_file ~pp:(Settings.get_value Basicsettings.pp) 
                     Parse.program abs_path in
    resolve_includes loaded filename program, sugar

(** Loads, typechecks, optimises a file--or reads it from the cache if
    it can.  Effectively, [load_file ty_env also_load filename] loads
    [filename], all files it includes and [also_load]; with the
    resulting code it types, optimises, and labelizes it, all in the
    context of typing environment [ty_env].
*)
let load_file : Types.typing_environment -> string list -> string
  -> (Types.typing_environment * Syntax.program)
  = fun tyenv also_load filename ->
    Debug.print("Loading " ^ filename);
    let cachename = filename ^ ".cache" in
      try
        if not(Settings.get_value use_caches) then
          raise No_caching
        else if Settings.get_value make_caches && newer cachename filename then
          call_with_open_infile cachename ~binary:true
            (fun cachefile ->
               (Marshal.from_channel cachefile 
                  : (Types.typing_environment * Syntax.program)))
            (* (OCaml manual recommends putting a type signature on unmarshal 
               calls; not clear whether this actually helps. It seems we get 
               a segfault if the marshaled data is not the right type.) *)
      else
         raise(Sys_error("Precompiled source file out of date ("^filename^")."))
    with (Sys_error _| Unix.Unix_error _ | No_caching) ->

      try
      let Syntax.Program(defs, expr) as program, sugar =
                         lazy(parse_file [filename] filename)
                           <|measure_as|> "parsing" in
      let defs = List.map (fun filename ->
                             Syntax.Module(Some filename, None, 
                                           `U Syntax.dummy_position)) also_load
                 @ defs in
      let program = Syntax.Program(defs, expr) in
      let program = resolve_includes [] filename program in
      let () = lazy(TypeSugar.Check.file tyenv sugar)
                           <|measure_as|> "typing sugar" in
      let env, program = lazy(Inference.type_program tyenv program) 
                           <|measure_as|> "type" in
      let program = lazy(Optimiser.optimise_program (env, program))
                           <|measure_as|> "optimise" in
      let env, program = if Settings.get_value correct_optimised_types then
        lazy(Inference.type_program tyenv (Syntax.erase program))
        <|measure_as|> "type again"
      else
        env, program
      in
      let program = Syntax.labelize program in 
	(try (* try to write to the cache *)
           call_with_open_outfile cachename ~binary:true
             (fun cachefile ->
                Marshal.to_channel cachefile 
                  (env, expunge_all_source_pos program)
                  [Marshal.Closures])
	 with _ -> ()) (* Ignore errors writing the cache file *);
        env, program
      with File_not_found filename -> failwith("Couldn't find file " ^ filename)
        | Sys_error _ -> failwith("Couldn't open file " ^ filename)
  
let dump_cached tyenv filename =
   let env, program = load_file tyenv [] filename in
     print_string (Syntax.labelled_string_of_program program)
