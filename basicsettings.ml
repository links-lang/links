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
    match Utility.getenv "LINKS_LIB" with 
      None -> Settings.add_string ("prelude", "prelude.links", `System)
    | Some path -> Settings.add_string ("prelude", path ^ "/" ^ "prelude.links", `System)

(** The banner *)
let welcome_note = Settings.add_string ("welcome_note", 
"  _     _ __   _ _  __  ___\n\
 / |   | |  \\ | | |/ / / ._\\\n\
 | |   | | , \\| |   /  \\  \\\n\
 | |___| | |\\ \\ | |\\ \\ _\\  \\\n\
 |_____|_|_| \\__|_| \\_|____/\n\
Welcome to Links with session types", `System)

(* (* Alternative banner designs *)
"  _     _ __    _ _  __  ___
 / |   | |  \\  |   |/ / / ._\\
 | |   | | , \\ | |   /  \\  \\
 | |___| | |\\ \\  | |\\ \\__\\  \\
 |_____._._| \\___|_| \\______/",
"                                         _
  _     _ __    _ _  _ ___        <>    |_>
 / |   | |  \\  | | |'// ._\\      //     |  
 | |   | | , \\ | |  / \\  \\      //      |
 | |___| | |\\ \\  | | \\_\\  \\  ,-//      _|_
 |_____._._| \\___|_|\\_____/  \\_/      <_|_>"
*)

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

(* Compile & cache whole program, closures, and HTML *)
let cache_whole_program = Settings.add_bool("cache_whole_program", false, `User)
