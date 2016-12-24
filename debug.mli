(** Facilities for optional debugging messages. *)

val debugging_enabled : bool Settings.setting

(** print a debug message if debugging is enabled *)
val print : string -> unit

(** print a debug message if debugging is enabled *)
val print_l : string lazy_t -> unit

(** printf-style debugging *)
val f :  ('a, unit, string, unit) format4 -> 'a

(**
  [if_set setting message]:
    print [message()] if debugging is enabled
    and [setting] is on.
 *)
val if_set : bool Settings.setting -> (unit -> string) -> unit

(**
  [if_set_l setting message]:
    print [message] (a lazy expr) if debugging is enabled
    and [setting] is on.
 *)
val if_set_l : bool Settings.setting -> string lazy_t -> unit


(** [debug_time msg f]:
      run [f()] and measure running time; print [msg] with time and return result
*)
val debug_time : string -> (unit -> 'a) -> 'a
