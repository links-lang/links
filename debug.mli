(** Facilities for optional debugging messages. *)

val debugging_enabled : bool Settings.setting

(* print a debug message if debugging is enabled *)
val print : string -> unit

(* printf-style debugging *)
val f :  ('a, unit, string, unit) format4 -> 'a

(*
  debug_if_set setting message
    print message() if debugging is enabled
    and setting is on
 *)
val if_set : bool Settings.setting -> (unit -> string) -> unit
