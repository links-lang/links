open Getopt

module BS = Basicsettings

let (config_file : string option ref) = ref BS.config_file_path

let options : opt list =
  let set setting value = Some (fun () -> Settings.set_value setting value) in
  [
    (noshort, "config",              None,                             Some (fun name -> config_file := Some name));
    (noshort, "enable-handlers",     set BS.Handlers.enabled true,     None);
  ]

let _ =
  try
    parse_cmdline options (fun _ -> ());
    match !config_file with
    | None -> ()
    | Some file -> Settings.load_file true file
  with _ -> ()
