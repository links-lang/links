(*** Debugging ***)
let debugging_enabled = Settings.add_bool ("debug", false, `User)

(* print a debug message if debugging is enabled *)
let print message = 
  (if Settings.get_value(debugging_enabled) then prerr_endline message; flush stderr)

(* print a formatted debugging message if debugging is enabled *)
let f fmt = Printf.kprintf print fmt

(* print a debugging message if debugging is enabled and setting is on *)
let if_set setting message =
  (if Settings.get_value(setting) then print (message ()))
