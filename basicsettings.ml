(**
 Whether to run the interactive loop
 (default is true)
*)
let interacting = Settings.add_bool ("interacting", true, `System)

(** [true] if we're in web mode *)
let web_mode = Settings.add_bool ("web_mode", false, `System)

(** Set this to [true] to print types when printing results. *)
let printing_types = Settings.add_bool ("printing_types", true, `User)

(** Name of the file containing the prelude code. *)
let prelude_file = Settings.add_string ("prelude", "prelude.links", `System)

(** The banner *)
let welcome_note = Settings.add_string ("welcome_note", 
"  _     _ __    _ _  __  ___
 / |   | |  \\  | | |/ / / ._\\
 | |   | | , \\ | |   /  \\  \\
 | |___| | |\\ \\  | |\\ \\ _\\  \\
 |_____|_|_| \\___|_| \\_|____/
Welcome to Links version 0.4 (Crostorfyn)", `System)
