type 'a point
  [@@deriving show]

val fresh      : 'a -> 'a point
val find       : 'a point -> 'a
val change     : 'a point -> 'a -> unit
val equivalent : 'a point -> 'a point -> bool
val union      : 'a point -> 'a point -> unit
