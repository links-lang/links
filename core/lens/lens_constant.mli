open CommonTypes

type t = Constant.t

val bool : bool -> t

val int : int -> t

val fmt : Format.formatter -> t -> unit

val of_value : Lens_phrase_value.t -> t

val to_value : t -> Lens_phrase_value.t
