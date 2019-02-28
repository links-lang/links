type t = Value.t

(** Get the value of column [key] from the specified record. *)
val get : t -> key:string -> Value.t

(** Change the records value of column [key] to [value]. The record is required to have a column
    with the specified key, or this function will not do anything. *)
val set : t -> key:string -> value:Value.t -> t

(** Determine if the two records have the same value for all columns [on]. *)
val match_on : t -> t -> on:string list -> bool
