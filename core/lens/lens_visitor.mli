module Visitor : sig
  type ('a, 'typ, 'inst) t =
    { traverse: 'a -> ('inst -> unit) -> unit
    ; traverse_type: ('typ -> unit) -> unit }

  type ('typ, 'inst) packed =
    | Pack : ('a, 'typ, 'inst) t * 'a -> ('typ, 'inst) packed
end

module Visitors : sig
  type ('a, 'typ, 'inst) t

  val of_list :
    ('a -> ('typ, 'inst) Visitor.packed option) list -> ('a, 'typ, 'inst) t

  val get_exn : ('a, 'typ, 'inst) t -> 'a -> ('typ, 'inst) Visitor.packed
end

module O : sig
  val ( -+- ) :
       ('a, 'typ, 'inst) Visitor.t
    -> ('b, 'typ, 'inst) Visitor.t
    -> ('a * 'b, 'typ, 'inst) Visitor.t
end
