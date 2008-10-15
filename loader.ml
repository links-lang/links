open Utility
open Performance

let `T (pos, dt, _) = Syntax.no_expr_data

let use_cache = true
let make_cache = true

let write_program (filename : string)
    (tenv : Types.typing_environment) (program : Syntax.program) : unit =
  call_with_open_outfile filename ~binary:true
    (fun file ->
       (* strip out positions and types *)
       let program = Syntax.Functor_program'.map (fun (`T (_,_,l)) -> l) program in
         Marshal.to_channel file
         (* Serialise the typing environment returned from
            sugar typing (which includes alias bindings), not
            the typing environment returned from
            Inference.type_program (which doesn't) *)
           ((tenv, program) : Types.typing_environment * (Syntax.label option) Syntax.program')
           [])
  
let read_file_source (filename:string) tyenv =
  let sugar, pos_context = 
      lazy (Parse.parse_file Parse.program filename) <|measure_as|> "parse" in
  let program, _, tenv = 
      Frontend.Pipeline.program tyenv pos_context sugar in
  let program = Sugar.desugar_program program in
  let env, program = 
    lazy (Inference.type_program tyenv program) <|measure_as|> "type" in
  let program = lazy (Optimiser.optimise_program (env, program)) 
                <|measure_as|> "optimise" in
  let program = Syntax.labelize program in
    tenv, env, program

let unmarshal_cache_file cachefile : (Types.typing_environment * Syntax.program) = 
  let tenv, program = Marshal.from_channel cachefile in
    (* fill in dummy positions and dummy types *)
  let program = Syntax.Functor_program'.map (fun l -> `T (SourceCode.dummy_pos, `Not_typed, l)) program in
    tenv, program

let read_cache_file cachename = 
  call_with_open_infile cachename ~binary:true unmarshal_cache_file

let cache_directory_setting = 
  Settings.add_string ("cache_directory", "", `User)

let cachefile_path filename = 
  let cachedir = Settings.get_value cache_directory_setting in
    match cachedir with
      | "" -> filename  ^ ".cache" (* Use current dir, no fancy filename  *)
      | cachedir ->                (* Use given dir, put hash in filename *)
          let path_hash = base64encode(Digest.string (absolute_path filename)) in
          let cache_filename = (Filename.basename filename) ^ "-" ^ 
                                 path_hash ^ ".cache" in
            Filename.concat cachedir cache_filename
    
exception No_cache

(** Load a file, considering and maintaining the cache. Includes 
    parsing, desugaring, typechecking, optimising and labelizing. *)
let load_file : _ -> string -> (Types.typing_environment * Syntax.program) = 
  fun env infile ->
  let cachename = cachefile_path infile in
  let result = 
    try
      if not use_cache then None else
      if getuid_owns cachename && newer cachename infile then
        Some (read_cache_file cachename)
      else
        raise No_cache
    with (No_cache | Sys_error _| Unix.Unix_error _) ->
      Debug.print("No valid cache for " ^ infile);
      None
  in
    match result with
        Some (tenv, program) -> tenv, program
      | None -> 
          (* Read & process the source *)
          let tenv, env, program = read_file_source infile env in
            if make_cache then
            (try  (* to write to the cache *)
               write_program cachename tenv program
             with _ -> ()) (* Ignore errors writing the cache file *);
            tenv, program

(** Loads a named file and prints it as syntax; may use the cache or
    the original file, as per the caching policy. *)
let print_cache filename =
   let _tenv, program = read_cache_file filename in
     print_string (Syntax.labelled_string_of_program program)

let precompile env infile outfile : unit =
  let tenv, env, program = read_file_source infile env in
    write_program outfile tenv program

let precompile_cache env infile : unit =
  let outfile = infile ^ ".cache" in
    precompile env infile outfile
