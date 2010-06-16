open Utility
open Performance

type envs = Var.var Env.String.t * Types.typing_environment
type program = Ir.binding list * Ir.computation * Types.datatype

(** Marshal an IR program to a file along with naming and typing
    environments and the fresh variable counters.
*)
let write_program filename envs program : unit =
  let var_counters = (!Types.type_variable_counter, !Var.variable_counter) in
    call_with_open_outfile filename ~binary:true
      (fun filename ->
         Marshal.to_channel filename
           ((envs, program, var_counters) : envs * program * (int * int))
           [])

(** Unmarshal an IR program from a file along with naming and typing
    environments and the fresh variable counters.
*)
let read_program filename : (envs * program) = 
  let envs, program, (tc, vc) =
    call_with_open_infile filename ~binary:true Marshal.from_channel
  in
    Types.type_variable_counter := tc;
    Var.variable_counter := vc;
    envs, program

(** Read source code from a file, parse, infer types and desugar to
    the IR *)
let read_file_source (filename:string) (nenv, tyenv) =
  let sugar, pos_context =
    lazy (Parse.parse_file Parse.program filename) <|measure_as|> "parse" in
  let program, t, tenv = Frontend.Pipeline.program tyenv pos_context sugar in
  let globals, main, nenv = Sugartoir.desugar_program (nenv, Var.varify_env (nenv, tyenv.Types.var_env), tyenv.Types.effect_row) program in
    (nenv, tenv), (globals, main, t)

let cachefile_path filename = 
  let cachedir = Settings.get_value Basicsettings.cache_directory in
    match cachedir with
      | "" -> filename  ^ ".cache" (* Use current dir, no fancy filename  *)
      | cachedir ->                (* Use given dir, put hash in filename *)
          let path_hash = base64encode(Digest.string (absolute_path filename)) in
          let cache_filename = (Filename.basename filename) ^ "-" ^ 
                                 path_hash ^ ".cache" in
            Filename.concat cachedir cache_filename
    
exception No_cache

(** Load a source file, parse, infer types and desugar to the IR. If
    the result has already been cached, then just use that instead.
    If not, then cache the result.
*)
let load_file : envs -> string -> (envs * program) = 
  fun envs infile ->
    let cachename = cachefile_path infile in
    let result = 
      try
        if not (Settings.get_value Basicsettings.use_cache) then None else
          if newer cachename infile &&
            (Settings.get_value Basicsettings.allow_stale_cache || newer cachename (Sys.argv.(0))) then
            Some (read_program cachename)
          else
            raise No_cache
      with (No_cache | Sys_error _| Unix.Unix_error _) ->
        Debug.print("No valid cache for " ^ infile);
        None
    in
      match result with
          Some (envs, program) -> envs, program
        | None ->
            (* Read & process the source *)
            let envs, program = read_file_source infile envs in
              if (Settings.get_value Basicsettings.make_cache) then
                (try  (* to write to the cache *)
                   write_program cachename envs program
                 with _ -> ()) (* Ignore errors writing the cache file *);
              envs, program

(** Loads a named file and prints it as syntax; may use the cache or
    the original file, as per the caching policy. *)
let print_cache filename =
   let _envs, (globals, (locals, main), _t) = read_program filename in
     print_string (Show.show Ir.show_program (globals @ locals, main))

(** precompile a cache file *)
let precompile_cache envs infile : unit =
  let outfile = infile ^ ".cache" in
  let envs, program = read_file_source infile envs in
    write_program outfile envs program
