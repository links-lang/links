(*
(* This module provides a minimal and unsafe support for
   saving/restoring the global state of the program.

   It assumes that the global state is fully described by
   Marshal'able references. 
*)

val ref: string -> 'a -> 'a ref
  (* Replacement for Pervasives.ref. Creates a persistant reference.
     Two runs of the programs must yield the same calls to this function,
     in the correct order. The arbitrary string argument is used to 
     check this order (give a different string for different calls).
  *)

val close: unit -> unit
  (* Close registration for the global state. When this function
     has been called, ref becomes illegal, and get/set become
     legal *)

val get: unit -> 'a
  (* Get a marshal'able value representing the global state *)

val set: 'a   -> unit
  (* Set the global state *)
*)
