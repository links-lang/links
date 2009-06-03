(* Finite map : dynamic |-> t *)

open Hash
open Typeable

type 'a t
val empty : 'a t
val add  : 'k hash -> 'k typeable -> 'k -> 'v -> 'v t -> 'v t
val find : 'k hash -> 'k typeable -> 'k -> 'v t -> 'v option
