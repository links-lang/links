(*
module P : sig
  type chunk
  val init: unit -> chunk
  val mk: chunk -> string

  val pm: chunk -> Types.t * Patterns.Node.t list -> int
  val const: chunk -> Types.const -> int
  val label: chunk -> Ident.label -> int
  val typ: chunk -> Types.t -> int
  val label_array: chunk -> Ident.label array -> int
  val tag: chunk -> Atoms.V.t -> int
  val tag_array: chunk -> (Atoms.V.t * int) array -> int
  val typ2: chunk -> Types.t -> Types.t -> int
end

module G : sig
  type chunk
  val mk: string -> chunk

  val pm: chunk -> int -> Value.t -> int * Value.t array
  val const: chunk -> int -> Value.t
  val remove_label: chunk -> int -> Value.t -> Value.t
  val get_field: chunk -> int -> Value.t -> Value.t
  val typ: chunk -> int -> Types.t
  val check: chunk -> int -> Value.t -> Value.t
  val record: chunk -> int -> Value.t array -> Value.t
  val constr: chunk -> int -> Value.t array -> Value.t
  val constr_const: chunk -> int -> Value.t
  val dconstr: chunk -> int -> Value.t -> Obj.t
  val dvariant: chunk -> int -> Value.t -> Obj.t
end
*)

module P : sig
  type chunk

  val init: unit -> chunk
  val mk: chunk -> string
  val put: chunk -> 'a -> int
end

module G : sig
  val mk: string -> Obj.t array
end
