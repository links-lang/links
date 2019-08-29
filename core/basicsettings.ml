(** [true] if we're in web mode *)
let web_mode =
  Settings.(flag "web_mode"
            |> synopsis "Start Links in web mode"
            |> privilege `System
            |> convert parse_bool
            |> CLI.(add (short 'w' <&> long "web-mode"))
            |> sync)

(** [true] if we're in interactive mode *)
let interactive_mode =
  Settings.(flag "interactive_mode"
            |> synopsis "Enter the REPL on startup"
            |> privilege `System
            |> convert parse_bool
            |> sync)

(** The banner *)
let version = "0.9.1 (Burghmuirhead)"
let version = Settings.(option ~default:(Some version) ~readonly:true "version"
                        |> privilege `System
                        |> synopsis "Print version and exit"
                        |> to_string from_string_option
                        |> action (fun _ -> Printf.printf "Links version %s\n%!" version; exit 0)
                        |> CLI.(add (fun arg -> short 'v' (long "version" arg)))
                        |> sync)

let welcome_note =
" _     _ __   _ _  __  ___\n\
 / |   | |  \\ | | |/ / / ._\\\n\
 | |   | | , \\| |   /  \\  \\\n\
 | |___| | |\\ \\ | |\\ \\ _\\  \\\n\
 |_____|_|_| \\__|_| \\_|____/\n\
Welcome to Links version " ^ (Utility.val_of (Settings.get version)) ^ "\n"
let welcome_note = Settings.(option ~default:(Some welcome_note) ~readonly:true "welcome_note"
                             |> privilege `System
                             |> to_string from_string_option
                             |> sync)

module Shredding = struct
  let relax_query_type_constraint =
    Settings.(flag "relax_query_type_constraint"
              |> convert parse_bool
              |> sync)

  let shredding =
    Settings.(flag "shredding"
              |> synopsis "Enables database query shredding"
              |> convert parse_bool
              |> sync)
end

module RelationalLenses = struct
  let nonincremental : bool Settings.setting option ref = ref None
  let incremental : bool Settings.setting option ref = ref None
  let relational_lenses =
    let setting =
      Settings.(flag "relational_lenses"
                |> synopsis "Enables the incremental relational lenses extenion"
                |> action (fun _ -> Settings.set (Utility.val_of !nonincremental) false)
                |> convert parse_bool)
    in incremental := Some setting; setting
  let classic_lenses =   (* Use naive/non-incremental relational lenses instead of incremental ones *)
    let setting =
      Settings.(flag "relational_lenses_classic"
                |> synopsis "Enables non-incremental relational lenses extension"
                |> action (fun _ -> Settings.set (Utility.val_of !incremental) false)
                |> convert parse_bool)
    in nonincremental := Some setting; setting

  let relational_lenses = Settings.sync relational_lenses
  let classic_lenses = Settings.sync classic_lenses

  let debug =
    Settings.(flag "relational_lenses_debug"
              |> synopsis "Enables debug mode for relational lenses extensions (development)"
              |> convert parse_bool
              |> sync)
end


(** App server stuff *)
module Appserver = struct
  let hostname =
    Settings.(option ~default:(Some "0.0.0.0") "host"
              |> synopsis "The host address of the app-server"
              |> to_string from_string_option
              |> convert Utility.some
              |> sync)

  let port =
    Settings.(option ~default:(Some 8080) "port"
              |> synopsis "The port for listening to incoming requests for the app-server"
              |> to_string (function Some i -> string_of_int i | None -> "<none>")
              |> convert (fun n -> Some (int_of_string n))
              |> sync)

  let external_base_url
    = Settings.(option "external_base_url"
                |> to_string from_string_option
                |> convert Utility.some
                |> sync)

  let internal_base_url
    = Settings.(option "internal_base_url"
                |> to_string from_string_option
                |> convert Utility.some
                |> sync)
end

(* Use keys in shredding *)
let use_keys_in_shredding
  = Settings.(flag ~default:true "use_keys_in_shredding"
              |> convert parse_bool
              |> sync)

(* Handlers stuff *)
module Handlers = struct
  let enabled
    = Settings.(flag "enable_handlers"
                |> privilege `System
                |> synopsis "Enables the effect handlers extension"
                |> convert parse_bool
                |> CLI.(add (long "enable-handlers"))
                |> sync)
end

module Sessions = struct
  let exceptions_enabled
    = Settings.(flag "session_exceptions"
                |> synopsis "Enables the session exceptions extension"
                |> depends Handlers.enabled
                |> privilege `System
                |> convert parse_bool
                |> CLI.(add (long "session-exceptions"))
                |> sync)
end
