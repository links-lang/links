type t = Constant.constant

val bool : bool -> t

val int : int -> t

val fmt : Format.formatter -> t -> unit

val of_value : Value.t -> t

val to_value : t -> Value.t
