(*** Debugging ***)
let debugging_enabled = Settings.add_bool ("debug", false, true)

(* print a debug message if debugging is enabled *)
let debug message = 
  (if Settings.get_value(debugging_enabled) then prerr_endline message; flush stderr)

let debugf fmt = Printf.kprintf debug fmt

let debug_if_set setting message =
  (if Settings.get_value(setting) then debug (message ()))

