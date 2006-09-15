module Definition : sig
  type t

  type regexp

  val epsilon : regexp

  val latin1 : regexp

  val of_string : string -> regexp

  val concat : regexp -> regexp -> regexp

  val question : regexp -> regexp

  val union : regexp -> regexp -> regexp

  val star : regexp -> regexp

  val plus : regexp -> regexp

  val of_name : string -> t

  val of_regexp : regexp -> t

  val element : string -> (string * bool) list -> bool -> t -> regexp

  val define : (string * t) list -> unit
end

module Type : sig
  type t

  val empty : t

  val any : t

  val string : t

  val epsilon : t

  val of_string : string -> t

  val of_definition : string -> t

  val of_regexp : Definition.regexp -> t

  val union : t -> t -> t

  val concat : t -> t -> t

  val element : string -> (string * bool) list -> bool -> t -> t

  val fprintf : Format.formatter -> t -> unit

  val to_buffer : Buffer.t -> t -> unit

  val to_string : t -> string

  val subtype : t -> t -> bool
end
