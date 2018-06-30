(** Whether to turn on debug printing *)
let debugging_enabled = Settings.add_bool ("debug", false, `User)

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

(* Looks for a given file, either in the current directory or in the Links opam path *)
let locate_file filename =
  (* If LINKS_LIB is not defined then we search in current directory *)
  let executable_dir = Filename.dirname Sys.executable_name in
  if Sys.file_exists (Filename.concat executable_dir filename) then
    executable_dir
  else
    try
      (* If all else failed we search for OPAM installation of Links and
         use a prelude that it provides *)
      let opam_links_lib =
        input_line (Unix.open_process_in "opam config var links:lib 2>/dev/null") in
      if Sys.file_exists (Filename.concat opam_links_lib filename) then
        opam_links_lib
      else
        (* But if no OPAM installation exists we fall back to current
           directory so that user gets a reasonable error message *)
        executable_dir
    with End_of_file ->
      (* User probably does not have OPAM, so fall back to current directory *)
      executable_dir

(** Name of the file containing the prelude code. *)
let prelude_file =
  let prelude_dir =
    match Utility.getenv "LINKS_LIB" with
    (* If user defined LINKS_LIB then it takes the highest priority *)
    | Some path -> path
    | None -> locate_file "prelude.links" in
  Settings.add_string ("prelude", Filename.concat prelude_dir "prelude.links", `System)

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


let default_db_driver_search_folders =
  let links_executable_folder = Filename.dirname Sys.argv.(0) in
  let remove_build_subdirs = Str.regexp "_build/default/bin/?$" in
  let links_base_folder =  Str.global_replace remove_build_subdirs "" links_executable_folder in
  let install_path = Filename.concat links_base_folder "_build/install/default" in
  let start_folders = List.map (Filename.concat install_path)  ["lib"; "share"] in
  let existing_start_folders = List.filter (Sys.file_exists) start_folders in
  let potential_search_folders = List.map (fun folder ->
                                 List.map (Filename.concat folder) (Array.to_list (Sys.readdir folder) ))
                               existing_start_folders in
  let links_db_driver_folder_regexp = Str.regexp ".+/links-[^/]+/?" in
  List.filter (fun s -> Sys.is_directory s &&  Str.string_match links_db_driver_folder_regexp s 0) (List.flatten potential_search_folders)

(** List of directories where to look for database drivers, split by ':'
    Initialized to point to where the drivers are compiled to if building in the current directory **)
let db_driver_path = Settings.add_string ("db_driver_path", String.concat ":" default_db_driver_search_folders, `System)

(** The banner *)
let version = "0.8 (Merchiston)"
let welcome_note = Settings.add_string ("welcome_note",
" _     _ __   _ _  __  ___\n\
 / |   | |  \\ | | |/ / / ._\\\n\
 | |   | | , \\| |   /  \\  \\\n\
 | |___| | |\\ \\ | |\\ \\ _\\  \\\n\
 |_____|_|_| \\__|_| \\_|____/\n\
Welcome to Links version " ^ version, `System)

(** Allow impure top-level definitions *)
let allow_impure_defs = Settings.add_bool("allow_impure_defs", false, `User)

(** JS stuff *)
(* needs to go here as long as we have two different JS compilers *)
module Js =
struct
  let optimise = Settings.add_bool("optimise_javascript", true, `User)
  let elim_dead_defs = Settings.add_bool("elim_dead_defs", false, `User)
  let lib_url = Settings.add_string("jsliburl", "lib/", `User)
  let lib_dir = Settings.add_string("jslibdir", "", `User)

  let hide_database_info = Settings.add_bool("js_hide_database_info", true, `System)
  let backend = Settings.add_string("js_compiler", "cps", `System)
end

module Shredding = struct
  let relax_query_type_constraint = Settings.add_bool("relax_query_type_constraint", false, `User)
  let shredding = Settings.add_bool("shredding", false, `User)
end


(** App server stuff *)
module Appserver = struct
  let hostname = Settings.add_string ("host", "0.0.0.0", `User)
  let port = Settings.add_int ("port", 8080, `User)
  let external_base_url = Settings.add_string("external_base_url", "", `User)
  let internal_base_url = Settings.add_string("internal_base_url", "", `User)
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

(* Optimization pass? *)
let optimise = Settings.add_bool("optimise", false, `User)

(* Use keys in shredding *)
let use_keys_in_shredding = Settings.add_bool("use_keys_in_shredding", true, `User)

(* Paths to look for .links files in chasing pass *)
let links_file_paths = Settings.add_string("links_file_paths", "", `User)

(* Pretty print values (outside web mode) *)
let print_pretty = Settings.add_bool ("print_pretty", false, `User)
let print_colors = Settings.add_bool ("print_colors", false, `User)

(* Base URL for websocket connections *)
let websocket_url = Settings.add_string("websocket_url", "/ws/", `User)

(* For testing only. If this is set, programs are not executed, but
   Links terminates after type-checking and compiling to the IR. *)
let typecheck_only = Settings.add_bool ("typecheck_only", false, `User)

(* Handlers stuff *)
module Handlers = struct
  let enabled = Settings.add_bool("enable_handlers", false, `System)
end

(* Performance settings *)
module Performance = struct
  let measuring = Settings.add_bool("measure_performance", false, `User)
  let noisy_gc = Settings.add_bool("noisy_garbage_collection", false, `User)
end

(* Serialisation stuff *)
module Serialisation = struct
  let serialiser = Settings.add_string ("serialiser", "Yojson", `User)
end

(* Typing stuff *)
module TypeSugar = struct
  let endbang_antiquotes = Settings.add_bool ("endbang_antiquotes", false, `User)
(*  let constrain_absence_types = Settings.add_bool ("constrain_absence_types", false, `User)*)
  let check_top_level_purity = Settings.add_bool ("check_top_level_purity", false, `User)
  let show_pre_sugar_typing = Settings.add_bool("show_pre_sugar_typing", false, `User)
  let dodgey_type_isomorphism = Settings.add_bool("dodgey_type_isomorphism", false, `User)
end

(* Types stuff *)
module Types = struct
  let show_mailbox_annotations = Settings.add_bool("show_mailbox_annotations", true, `User)
  let show_raw_type_vars = Settings.add_bool("show_raw_type_vars", false, `User)
  module Print = struct
    let show_quantifiers     = Settings.add_bool   ("show_quantifiers"    , false    , `User)
    let show_flavours        = Settings.add_bool   ("show_flavours"       , false    , `User)
    let show_kinds           = Settings.add_string ("show_kinds"          , "default", `User)
    let hide_fresh_type_vars = Settings.add_bool   ("hide_fresh_type_vars", true     , `User)
  end
end

(* Compile patterns stuff *)
module CompilePatterns = struct
  let show_pattern_compilation = Settings.add_bool("show_pattern_compilation2", false, `User)
end

(* Ir stuff *)
module Ir = struct
  let show_rec_uses = Settings.add_bool("show_rec_uses", false, `User)
end

(* Generalise stuff *)
module Generalise = struct
  let show_generalisation = Settings.add_bool("show_generalisation", false, `User)
end

(* Webif stuff *)
module Webif = struct
  let realpages = Settings.add_bool ("realpages", false, `System)
end

(* Json stuff *)
module Json = struct
  let show_json = Settings.add_bool("show_json", false, `User)
end

(* Webserver types stuff *)
module Webserver_types = struct
  let webs_running = Settings.add_bool ("webs_running", false, `System)
end

(* Polymorphic types instantiation stuff *)
module Instantiate = struct
  let show_recursion = Settings.add_bool("show_recursion", false, `User)
  let show_instantiation = Settings.add_bool("show_instantiation", false, `User)
  let quantified_instantiation = Settings.add_bool("quantified_instantiation", true, `User)
end

(* Evaluation stuff *)
module Evalir = struct
  let dynamic_static_routes = Settings.add_bool ("dynamic_static_routes", false, `User)
end

(* Sugar to ir stuff *)
module Sugartoir = struct
  let show_compiled_ir = Settings.add_bool ("show_compiled_ir", false, `User)
end

(* Unification stuff *)
module Unify = struct
  let show_unification = Settings.add_bool("show_unification", false, `User)
  let show_row_unification = Settings.add_bool("show_row_unification", false, `User)
  let infer_recursive_types = Settings.add_string("infer_recursive_types", "guarded", `User)
end

(* Standard Library stuff *)
module StdLib = struct
  (* Should we use the extra standard library definitions? *)
  let use_stdlib = Settings.add_bool ("use_stdlib", true, `User)

  (* Standard library path *)
  let stdlib_path =
    let dir =
      match Utility.getenv "LINKS_LIB" with
      | Some path -> Filename.concat path "stdlib"
      | None -> ""
    in
    Settings.add_string ("stdlib_path", dir, `User)
end

module Readline = struct
  (* Path for readline history file *)
  let readline_history_setting = Settings.add_string("readline_history_path", "", `User)

  let readline_history_path () =
    let settings_path = Settings.get_value readline_history_setting in
    if settings_path = "" then
      Filename.concat (Unix.getenv "HOME") ".links_history"
    else
      settings_path

  (* Enable native readline? *)
  let native_readline = Settings.add_bool("native_readline", true, `User)
end

module Sessions = struct
  let exceptions_enabled = Settings.add_bool ("session_exceptions", false, `User)
end
