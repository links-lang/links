(*
module Put : sig
  type t
  type 'a f = t -> 'a -> unit
  val run: 'a f -> 'a -> string

  val bits: int -> int f
  val int: int f
  val string: string f
  val substring: t -> string -> int -> int -> unit
  val magic: t -> string -> unit
  val bool: bool f
  val env: 'a f -> 'b f -> (('a -> 'b -> unit) -> 'c -> unit) -> 'c f

  val list: 'a f -> 'a list f
  val array: 'a f -> 'a array f
  val pair: 'a f -> 'b f -> ('a * 'b) f

  type 'b property
  val mk_property: (t -> 'b) -> 'b property
  val get_property: 'b property -> t -> 'b

  val pos: t -> int
end

module Get : sig
  type t
  type 'a f = t -> 'a
  val run : 'a f -> string -> 'a

  val bits: int -> int f
  val int : int f
  val string: string f
  val bool: bool f

  val list: 'a f -> 'a list f
  val array: 'a f -> 'a array f
  val pair: 'a f -> 'b f -> ('a * 'b) f
  val magic: t -> string -> unit
  val env: 'a f -> 'b f -> ('a -> 'b -> 'c -> 'c) -> 'c -> 'c f


  type 'b property
  val mk_property: (t -> 'b) -> 'b property
  val get_property: 'b property -> t -> 'b

  val pos: t -> int
end
*)
