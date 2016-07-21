open Utility
open Performance
open Printf

type envs = Var.var Env.String.t * Types.typing_environment
type program = Ir.binding list * Ir.computation * Types.datatype

(** Marshal an IR program to a file along with naming and typing
    environments and the fresh variable counters.
*)
let write_a filename x : unit =
  let counters = (!Utility.gensym_counter, !Types.type_variable_counter, !Var.variable_counter) in
    call_with_open_outfile filename ~binary:true
      (fun fh ->
         Marshal.to_channel fh
           ((x, counters) : 'a * (int * int * int))
           [])

let write_program filename envs program : unit =
  write_a filename (envs,program)

(** Unmarshal an IR program from a file along with naming and typing
    environments and the fresh variable counters.
*)
let read_a filename : ('a) =
  let x, (gc, tc, vc) =
    call_with_open_infile filename ~binary:true Marshal.from_channel
  in
    Utility.gensym_counter := gc;
    Types.type_variable_counter := tc;
    Var.variable_counter := vc;
    x

let read_program filename : (envs * program) = read_a filename

(* measuring only *)
let read_program filename : (envs * program) =
  measure ("read_program "^filename) read_program filename



(** Read source code from a file, parse, infer types and desugar to
    the IR *)
let read_file_source (nenv, tyenv) (filename:string) =
  let sugar, pos_context =
    Parse.parse_file Parse.program filename  in
  (* printf "Parsed AST: \n%s \n\n" (Sugartypes.Show_program.show sugar); *)
  let program, t, tenv = Frontend.Pipeline.program tyenv pos_context sugar in
  let globals, main, nenv =
    Sugartoir.desugar_program
      (nenv,
       Var.varify_env (nenv, tyenv.Types.var_env),
       tyenv.Types.effect_row) program
  in
  (nenv, tenv), (globals, main, t)

let cachefile_path_tag filename tag =
  let suffix = if tag = "" then ".cache" else "."^tag^".cache" in
  let cachedir = Settings.get_value Basicsettings.cache_directory in
    match cachedir with
      | "" -> filename  ^ suffix (* Use current dir, no fancy filename  *)
      | cachedir ->                 (* Use given dir, put hash in filename *)
          let path_hash = base64encode(Digest.string (absolute_path filename)) in
          let cache_filename = (Filename.basename filename) ^ "-" ^
                                 path_hash ^ suffix in
            Filename.concat cachedir cache_filename

let cachefile_path filename = cachefile_path_tag filename ""

exception No_cache

let cache : string -> string -> (unit -> 'a) -> 'a =
  fun infile tag f ->
    let cachename = cachefile_path_tag infile tag in
    let result =
      try
	if not (Settings.get_value Basicsettings.use_cache) then None else
          if newer cachename infile &&
             (Settings.get_value Basicsettings.allow_stale_cache
	      || newer cachename (Sys.argv.(0))) then
            Some (read_a cachename)
          else (raise No_cache)
      with (No_cache | Sys_error _ | Unix.Unix_error _ | End_of_file) ->
        Debug.print("No valid "^tag^" cache for " ^ infile);
        None

    in
    match result with
      Some x ->  (* Return the cached value *)
	Debug.print ("Reusing "^cachename);
	x
    | None -> (* Read & process the source *)
        let x = f () in
        let _ =
	  if (Settings.get_value Basicsettings.make_cache) then
	    try  (* to write to the cache *)
	      Debug.print("Caching "^cachename);
	      write_a cachename x
	    with exn -> (Debug.print("Caching failed"))
		(* Ignore errors writing the cache file *)
	in x
;;



(** Load a source file, parse, infer types and desugar to the IR. If
    the result has already been cached, then just use that instead.
    If not, then cache the result.
*)
(*let load_file : envs -> string -> (envs * program) =
  fun envs infile ->
    let cachename = cachefile_path infile in
    let result =
      try
	if not (Settings.get_value Basicsettings.use_cache) then None else
          if newer cachename infile &&
            (Settings.get_value Basicsettings.allow_stale_cache || newer cachename (Sys.argv.(0))) then
            Some (read_program cachename)
          else
            (raise No_cache)
      with (No_cache | Sys_error _ | Unix.Unix_error _) ->
        Debug.print("No valid cache for " ^ infile);
        None

    in
      match result with
          Some (envs, program) -> envs, program
        | None ->
            (* Read & process the source *)
            let envs, program = read_file_source envs infile in
              if (Settings.get_value Basicsettings.make_cache) then
                (try  (* to write to the cache *)
                   write_program cachename envs program
                 with _ -> ()) (* Ignore errors writing the cache file *);
              envs, program
*)

let load_file envs filename =
  cache filename "" (fun () -> read_file_source envs filename);;


(** Loads a named file and prints it as syntax; may use the cache or
    the original file, as per the caching policy. *)
let print_cache filename =
   let _envs, (globals, (locals, main), _t) = read_program filename in
     print_string (Ir.Show_program.show (globals @ locals, main))

(** precompile a cache file *)
let precompile_cache envs infile : unit =
  let outfile = infile ^ ".cache" in
  let envs, program = read_file_source envs infile in
    write_program outfile envs program


let wpcachefilename = ref ""

let activate_wpcache str =
   if Settings.get_value Basicsettings.cache_whole_program
      && !wpcachefilename = ""
   then wpcachefilename := str
   else failwith "Whole program cache activated more than once"

let wpcache tag f =
  if Settings.get_value Basicsettings.cache_whole_program
      && !wpcachefilename <> ""
  then cache (!wpcachefilename) tag f
  else f()
