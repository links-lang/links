open Lens_utility

module Visitor = struct
  type ('a, 'typ, 'inst) t =
    { traverse: 'a -> ('inst -> unit) -> unit
    ; traverse_type: ('typ -> unit) -> unit }

  type ('typ, 'inst) packed =
    | Pack : ('a, 'typ, 'inst) t * 'a -> ('typ, 'inst) packed
end

module Visitors = struct
  type ('a, 'typ, 'inst) t = ('a -> ('typ, 'inst) Visitor.packed option) list

  let of_list l = l

  let get_exn l v =
    List.find (fun f -> match f v with Some _ -> true | None -> false) l
    |> (fun f -> f v |> Option.value_exn ~exn:(Failure "Missing visitor."))
end

module O = struct
  open Visitor

  let ( -+- ) t t' =
    let traverse (v, v') f = t.traverse v f ; t'.traverse v' f in
    let traverse_type f = t.traverse_type f ; t'.traverse_type f in
    {traverse; traverse_type}
end
