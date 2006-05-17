(*** debugging ***)

val debugging_enabled : bool Settings.setting

(* print a debug message if debugging is enabled *)
val debug : string -> unit

(*
  debug_if_set setting message
    print message() if setting is set to true
 *)
val debug_if_set : bool Settings.setting -> (unit -> string) -> unit
