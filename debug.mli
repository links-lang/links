(** Facilities for optional debugging messages. *)

val debugging_enabled : bool Settings.setting

(* print a debug message if debugging is enabled *)
val debug : string -> unit

(* printf-style debugging *)
val debugf :  ('a, unit, string, unit) format4 -> 'a

(*
  debug_if_set setting message
    print message() if setting is set to true
 *)
val debug_if_set : bool Settings.setting -> (unit -> string) -> unit
