(*** Debugging ***)
let debuggingEnabled = Settings.add_bool false "debug"

(* print a debug message if debugging is enabled *)
let debug msg = 
  (if Settings.get_value(debuggingEnabled) then prerr_endline msg)
