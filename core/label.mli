open CommonTypes

type local = Int.t
type global = Name.t

type t =
        | Lcl of local
        | Gbl of global
      [@@deriving show]

val mk_fresh : unit -> t

val mk_global : Name.t -> t
val make : Name.t -> t

val mk_int : Int.t -> t

val get_int : t -> Int.t

val compare : t -> t -> int

val is_local : t -> bool
val is_global : t -> bool

val one : t
