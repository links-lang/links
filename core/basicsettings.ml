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
