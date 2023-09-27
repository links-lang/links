(** The banner *)
let version = "0.9.8 (Burghmuirhead)"
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

(* control-flow linearity *)
module CTLinearity = struct
  let enabled =
    Settings.(flag "control_flow_linearity"
              |> privilege `System
              |> synopsis "Enables the control-flow linearity extension"
              |> depends Handlers.enabled
              |> convert parse_bool
              |> CLI.(add (long "control-flow-linearity"))
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

  let expose_session_fail =
    Settings.(flag "expose_session_fail" ~default:true
              |> synopsis "Exposes the SessionFail effect"
              |> depends Handlers.enabled
              |> depends exceptions_enabled
              |> privilege `System
              |> convert parse_bool
              |> CLI.(add (long "expose-session-fail"))
              |> sync)
end

module System = struct
  type mode = Interactive
            | Compile
            | Web
  let mode =
    let parse_mode s =
      match String.lowercase_ascii s with
      | "interact"    -> Some Interactive
      | "compile"     -> Some Compile
      | "web"         -> Some Web
      | _ -> raise (Invalid_argument (Printf.sprintf "Unrecognised mode '%s'" s))
    in
    let string_of_mode = function
      | Some Interactive -> "interact"
      | Some Compile -> "compile"
      | Some Web -> "web"
      | None -> "<none>"
    in
    Settings.(option "mode"
              |> privilege `System
              |> synopsis "Instructs Links to run in interactive, compilation, or web mode"
              |> convert parse_mode
              |> hidden
              |> to_string string_of_mode
              |> hint "<compile|interact|web>"
              |> CLI.(add (long "mode"))
              |> sync)

  let compile_mode =
    Settings.(flag "compile" ~default:false
              |> privilege `System
              |> synopsis "Toggles compilation mode"
              |> action (fun _ -> set mode (Some Compile))
              |> hidden
              |> CLI.(add (long "compile" <&> short 'c'))
              |> sync)

  (* TODO(dhil): The notion of output file might need to be
     generalised as we add more backends to Links or decide to add
     support for dumping other compilation artefacts. *)
  let output_file =
    Settings.(option "output_file" ~default:(Some "a.js")
              |> privilege `User
              |> synopsis "Set output file name to <file>"
              |> hint "<file>"
              |> to_string from_string_option
              |> convert (fun s -> Some s)
              |> CLI.(add (long "output" <&> short 'o'))
              |> sync)

  let interactive_mode =
    Settings.(flag "interactive_mode"
              |> synopsis "Toggles interactive mode"
              |> privilege `System
              |> action (fun _ -> set mode (Some Interactive))
              |> hidden
              |> sync)

  let is_interacting () =
    match Settings.get mode with
    | Some Interactive -> true
    | _ -> false

  let link_js_runtime =
    Settings.(flag "link_js_runtime" ~default:true
              |> privilege `User
              |> synopsis "In compile mode, this flag toggles whether the JS compiler statically links the JS runtime"
              |> convert parse_bool
              |> hidden
              |> CLI.(add (long "Ljs-runtime"))
              |> sync)

  let custom_js_runtime =
    Settings.(multi_option "custom_js_runtime"
              |> privilege `User
              |> synopsis "If link_js_runtime is set to true, then the JS compiler will link the provided file(s) rather than the standard Links JS runtime"
              |> to_string string_of_paths
              |> convert parse_paths
              |> hidden
              |> CLI.(add (long "Xcustom-js-runtime"))
              |> sync)
end
