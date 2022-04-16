module ReprState : sig
  type t

  val initial : t
end

module Stage2 : sig
  type t
end

exception SchinksError of string

type 'a lookup = ('a, Stage2.t) State.t

type 'a stage1 = ('a, ReprState.t) State.t

type 'a t = 'a lookup stage1

val make : ?state:ReprState.t -> 'a t -> 'a

val lookup_tname : tname:string -> int lookup

val lookup_name : name:string -> int lookup

val add_tnames : tnames:string list -> unit stage1

val add_names : names:string list -> unit stage1

val add_tids : tids:int list -> unit stage1

val add_ids : ids:int list -> unit stage1

val add_tname : tname:string -> unit stage1

val add_name : name:string -> unit stage1

val add_tid : tid:int -> unit stage1

val add_id : id:int -> unit stage1

val fresh_id : int stage1
