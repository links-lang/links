(* Bidirectional map {t -> t} *)

module type S =
sig
  type item
  type t
  val empty : t
  val add : item -> item -> t -> t
  val find : item -> t -> item
  val mem : item -> t -> bool
  val rmem : item -> t -> bool
end

module type OrderedType = sig type t val compare : t -> t -> int end
module Make (Ord : OrderedType) =
struct
  type item = Ord.t
  type t = (item * item) list
  let empty = []
  let add l r list = (l,r)::list
  let find = List.assoc
  let mem = List.mem_assoc
  let rmem item = List.exists (fun (_,i) -> i = item)
end

