(**
 Whether to run the interactive loop
 (default is true)
*)
let interacting = Settings.add_bool ("interacting", true, `System)

(** [true] if we're in web mode *)
let web_mode = Settings.add_bool ("web_mode", false, `System)

(** If [true], then wait for all child processes to finish before
    terminating *)
let wait_for_child_processes = Settings.add_bool ("wait_for_child_processes", false, `User)


(** If [true], then enable concurrency on the server:

    - Child processes are abandoned if the main process ends.

    - A run-time error results if the server tries to call the client
    with child processes still running.
*)
let concurrent_server = Settings.add_bool ("concurrent_server", true, `System)

(** Set this to [true] to print types when printing results. *)
let printing_types = Settings.add_bool ("printing_types", true, `User)

(** Name of the file containing the prelude code. *)
let prelude_file =
  let prelude_dir = match Utility.getenv "LINKS_LIB" with
    (* If user defined LINKS_LIB then it takes the highest priority *)
    | Some path -> path
    | None ->
        (* If LINKS_LIB is not defined then we search in current directory *)
        let executable_dir = Filename.dirname Sys.executable_name in
        if Sys.file_exists (Filename.concat executable_dir "prelude.links") then
          executable_dir
        else
          try
            (* If all else failed we search for OPAM installation of Links and
               use a prelude that it provides *)
            let opam_links_lib =
              input_line (Unix.open_process_in "opam config var links:lib 2>/dev/null") in
            if Sys.file_exists (Filename.concat opam_links_lib "prelude.links") then
              opam_links_lib
            else
              (* But if no OPAM installation exists we fall back to current
                 directory so that user gets a reasonable error message *)
              executable_dir
          with End_of_file ->
            (* User probably does not have OPAM, so fall back to current directory *)
            executable_dir
  in Settings.add_string ("prelude", Filename.concat prelude_dir "prelude.links", `System)

(** Path to config file *)
let config_file_path = match Utility.getenv "LINKS_CONFIG" with
    (* If user defined LINKS_CONFIG then it takes the highest priority *)
    | Some path -> Some (Filename.concat path "config")
    | None ->
        (* If LINKS_CONFIG is not defined then we search in current directory *)
        let executable_dir = Filename.dirname Sys.executable_name in
        if Sys.file_exists (Filename.concat executable_dir "config") then
          Some (Filename.concat executable_dir "config")
        else
          try
            (* If all else failed we search for OPAM installation of Links and
               use a config that it provides *)
            let opam_links_etc =
              input_line (Unix.open_process_in "opam config var links:etc 2>/dev/null") in
            if Sys.file_exists (Filename.concat opam_links_etc "config") then
              Some (Filename.concat opam_links_etc "config")
            else
              None
          with End_of_file ->
            None

(** The banner *)
let welcome_note = Settings.add_string ("welcome_note",
" _     _ __   _ _  __  ___\n\
 / |   | |  \\ | | |/ / / ._\\\n\
 | |   | | , \\| |   /  \\  \\\n\
 | |___| | |\\ \\ | |\\ \\ _\\  \\\n\
 |_____|_|_| \\__|_| \\_|____/\n\
Welcome to Links version 0.6.1 (Gorgie)", `System)

(** Allow impure top-level definitions *)
let allow_impure_defs = Settings.add_bool("allow_impure_defs", false, `User)

(** JS stuff *)
(* needs to go here as long as we have two different JS compilers *)
module Js =
struct
  let optimise = Settings.add_bool("optimise_javascript", true, `User)
  let elim_dead_defs = Settings.add_bool("elim_dead_defs", false, `User)
  let lib_url = Settings.add_string("jsliburl", "lib/", `User)
  let pp = Settings.add_bool("js_pretty_print", true, `User)

  let hide_database_info = Settings.add_bool("js_hide_database_info", true, `System)
end

module Shredding = struct
  let relax_query_type_constraint = Settings.add_bool("relax_query_type_constraint", false, `User)
  let shredding = Settings.add_bool("shredding", false, `User)
end


(** Caveat: don't [Open basicsettings] because the above module
   conflicts with the Js module from js.ml*)

(** Installed preprocessor *)
let pp = Settings.add_string("preprocessor", "", `System)

(** Default database settings *)
let database_driver = Settings.add_string("database_driver", "", `User)
let database_args = Settings.add_string("database_args", "", `User)

(** Set this to [true] to print the body and environment of a
    function. When [false], functions are simply printed as [fun] *)
let printing_functions = Settings.add_bool ("printing_functions", false, `User)

(** Caching *)
let cache_directory =
  Settings.add_string ("cache_directory", "", `User)
let use_cache = Settings.add_bool("use_cache", true, `System)
let make_cache = Settings.add_bool("make_cache", true, `System)

(* if set to true, then Links will not check that the
   cache is newer than the Links binary
*)
let allow_stale_cache = Settings.add_bool("allow_stale_cache", false, `System)

(* Optimization pass? *)
let optimise = Settings.add_bool("optimise", false, `User)

(* Allow modules? *)
let modules = Settings.add_bool("modules", false, `User)

(* Compile & cache whole program, closures, and HTML *)
let cache_whole_program = Settings.add_bool("cache_whole_program", false, `User)

(* Use keys in shredding *)
let use_keys_in_shredding = Settings.add_bool("use_keys_in_shredding", true, `User)

(* Paths to look for .links files in chasing pass *)
let links_file_paths = Settings.add_string("links_file_paths", "", `User)
