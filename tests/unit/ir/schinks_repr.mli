module ReprState : sig
  type t

  val initial : t
end

module Stage2 : sig
  type t
end

module E : sig
  type t
end

type 'a lookup = ('a, Stage2.t) State.t

type 'a stage1 = ('a, ReprState.t) State.t

type 'a maker = 'a lookup stage1

type 'a t = ('a, E.t) Result.t maker

val make : ?state:ReprState.t -> 'a maker -> ('a, 'b) result

val lookup_tname : tname:string -> int lookup

val lookup_name : name:string -> int lookup

val add_tname : tname:string -> unit stage1

val add_name : name:string -> unit stage1

val add_tid : tid:int -> unit stage1

val add_id : id:int -> unit stage1
