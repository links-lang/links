open Utility
open Getopt
module BS = Basicsettings

let to_evaluate : string list ref = ref []
let to_precompile : string list ref = ref []
let file_list : string list ref = ref []

let set_web_mode() = (
    (* When forcing web mode using the command-line argument, default
     the CGI environment variables to a GET request with no params--
     i.e. start running with the main expression. *)
  if not(is_some(getenv "REQUEST_METHOD")) then
    Unix.putenv "REQUEST_METHOD" "GET";
  if not(is_some(getenv "QUERY_STRING")) then
    Unix.putenv "QUERY_STRING" "";
  Settings.set_value BS.web_mode true
  )

let print_keywords = ref false
let print_cache : (bool * string option) ref = ref (false, None)

let config_file   : string option ref = ref BS.config_file_path
let options : opt list =
  let set setting value = Some (fun () -> Settings.set_value setting value) in
  [
    ('d',     "debug",               set BS.debugging_enabled true, None);
    ('w',     "web_mode",            Some set_web_mode,                None);
    (noshort, "optimise",            set BS.optimise true,             None);
    (noshort, "measure-performance", set BS.Performance.measuring true,               None);
    ('n',     "no-types",            set BS.printing_types false,      None);
    ('e',     "evaluate",            None,                             Some (fun str -> push_back str to_evaluate));
    ('m',     "modules",             set BS.modules true,              None);
    (noshort, "dump",                None,
     Some (fun filename -> print_cache := (true, Some filename)));
    (noshort, "precompile",          None,                             Some (fun file -> push_back file to_precompile));
    (noshort, "print-keywords",      Some (fun () -> print_keywords := true), None);
    (noshort, "pp",                  None,                             Some (Settings.set_value BS.pp));
    (noshort, "path",                None,                             Some (fun str -> Settings.set_value BS.links_file_paths str));
    (noshort, "config",              None,                             Some (fun name -> config_file := Some name));
    (noshort, "enable-handlers",     set BS.Handlers.enabled true,     None);
    ('r',     "rlwrap",              set BS.Readline.native_readline false, None);
    (noshort, "session-exceptions",  set BS.Sessions.exceptions_enabled true, None);
    ]


let _ =
  try
    parse_cmdline options (fun i -> push_back i file_list);
    (match !config_file with
    | None -> ()
    | Some file -> Settings.load_file false file);
  with Error msg -> Printf.fprintf stderr "error: %s\n" msg; flush stderr; exit 1
