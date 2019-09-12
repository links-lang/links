val measure : string -> ('a -> 'b) -> ('a) -> 'b
val measure_l : string -> ('b lazy_t) -> 'b
val measure_as : ('b lazy_t) -> string -> 'b
