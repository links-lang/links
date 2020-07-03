(** [true] if we're in interactive mode *)
let interactive_mode =
  Settings.(flag "interactive_mode"
            |> synopsis "Signifies whether Links is running in REPL mode"
            |> privilege `System
            |> convert parse_bool
            |> sync)

(** The banner *)
let version = "0.9.2 (Burghmuirhead)"
let version = Settings.(option ~default:(Some version) ~readonly:true "version"
                        |> privilege `System
                        |> synopsis "Print version and exit"
                        |> to_string from_string_option
                        |> action (fun _ -> Printf.printf "Links version %s\n%!" version; exit 0)
                        |> show_default false
                        |> CLI.(add (fun arg -> short 'v' (long "version" arg)))
                        |> sync)

let show_stages =
  Settings.(flag "show_stages"
            |> convert parse_bool
            |> sync)

module Types = struct
  let show_recursion =
    Settings.(flag "show_recursion"
              |> depends Debug.enabled
              |> convert parse_bool
              |> sync)
end

module Handlers = struct
  let enabled =
    Settings.(flag "enable_handlers"
              |> privilege `System
              |> synopsis "Enables the effect handlers extension"
              |> convert parse_bool
              |> CLI.(add (long "enable-handlers"))
              |> sync)
end

module Sessions = struct
  let exceptions_enabled =
    Settings.(flag "session_exceptions"
              |> synopsis "Enables the session exceptions extension"
              |> depends Handlers.enabled
              |> privilege `System
              |> convert parse_bool
              |> CLI.(add (long "session-exceptions"))
              |> sync)
end
