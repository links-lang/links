(** This module is used for benchmarking relational lenses. It keeps track of time spent querying database
    servers. *)

(** Increment the query counte and add the time it took to execute the argument function to the query time counter. *)
val time_query : (unit -> 'a) -> 'a

(** Reset all counters and timers to zero. *)
val reset : unit -> unit

val get_query_time : unit -> int

val get_query_count : unit -> int

val debug_time_out : (unit -> 'a) -> (float -> unit) -> 'a
