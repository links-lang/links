type t
val empty : t
val text : string -> t
val nest : int -> t -> t
val break : t
val break_null : t
val break_with : string -> t
val hgrp : t -> t
val vgrp : t -> t
val agrp : t -> t
val fgrp : t -> t
val default_width : int
val to_string : ?width:int -> t -> string
val to_file : ?width:int -> out_channel -> t -> unit
val list : sep:t -> f:('a -> t) -> 'a list -> t
val commalist : f:('a -> t) -> 'a list -> t

val ( $ ) : t -> t -> t
val ( $/ ) : t -> t -> t
val ( $// ) : t -> t -> t

val vlist : t list -> t
val alist : t list -> t
val hlist : t list -> t
