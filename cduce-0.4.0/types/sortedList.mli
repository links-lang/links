(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

module Make(X : Custom.T) :
sig
  include Custom.T with type t = X.t list
  module Elem : Custom.T with type t = X.t

  external get: t -> X.t list = "%identity"

  val singleton: X.t -> t
  val iter: (X.t -> unit) -> t -> unit
  val filter: (X.t -> bool) -> t -> t
  val exists: (X.t -> bool) -> t -> bool
  val fold: ('a -> X.t -> 'a) -> 'a -> t -> 'a
  val pick: t -> X.t option
  val choose: t -> X.t
  val length: t -> int

  val empty: t
  val is_empty: t -> bool
  val from_list : X.t list -> t
  val add: X.t -> t -> t
  val remove: X.t -> t -> t
  val disjoint: t -> t -> bool
  val cup: t -> t -> t
  val split: t -> t -> t * t * t
    (* split l1 l2 = (l1 \ l2, l1 & l2, l2 \ l1) *)
  val cap:  t -> t -> t
  val diff: t -> t -> t
  val subset: t -> t -> bool
  val map: (X.t -> X.t) -> t -> t
  val mem: t -> X.t -> bool

  module Map: sig
    type 'a map
    external get: 'a map -> (X.t * 'a) list = "%identity"
    val add: X.t -> 'a -> 'a map -> 'a map
    val length: 'a map -> int
    val domain: 'a map -> t
    val restrict: 'a map -> t -> 'a map
    val empty: 'a map
    val iter: ('a -> unit) -> 'a map -> unit
    val iteri: (X.t -> 'a -> unit) -> 'a map -> unit
    val filter: (X.t -> 'a -> bool) -> 'a map -> 'a map
    val is_empty: 'a map -> bool
    val singleton: X.t -> 'a -> 'a map
    val assoc_remove: X.t -> 'a map -> 'a * 'a map
    val remove:  X.t -> 'a map -> 'a map
    val merge: ('a -> 'a -> 'a ) -> 'a map -> 'a map -> 'a map
    val combine: ('a -> 'c) -> ('b -> 'c) -> ('a -> 'b -> 'c) ->
      'a map -> 'b map -> 'c map
    val cap: ('a -> 'a -> 'a ) -> 'a map -> 'a map -> 'a map
    val sub: ('a -> 'a -> 'a ) -> 'a map -> 'a map -> 'a map

    val merge_elem: 'a -> 'a map -> 'a map -> 'a map
    val union_disj: 'a map -> 'a map -> 'a map
    val diff: 'a map -> t -> 'a map
    val from_list: ('a -> 'a -> 'a ) -> (X.t * 'a) list -> 'a map
    val from_list_disj: (X.t * 'a) list -> 'a map

    val map_from_slist: (X.t -> 'a) -> t -> 'a map
    val collide: ('a -> 'b -> unit) -> 'a map -> 'b map -> unit
    val may_collide: ('a -> 'b -> unit) -> exn -> 'a map -> 'b map -> unit
    val map: ('a -> 'b) -> 'a map -> 'b map
    val mapi: (X.t -> 'a -> 'b) -> 'a map -> 'b map
    val constant: 'a -> t -> 'a map
    val num: int -> t -> int map
    val map_to_list: ('a -> 'b) -> 'a map -> 'b list
    val mapi_to_list: (X.t -> 'a -> 'b) -> 'a map -> 'b list
    val assoc: X.t -> 'a map -> 'a
    val assoc_present:  X.t -> 'a map -> 'a
    val compare: ('a -> 'a -> int) -> 'a map -> 'a map -> int
    val hash: ('a -> int) -> 'a map -> int
    val equal: ('a -> 'a -> bool) -> 'a map -> 'a map -> bool
  end
  module MakeMap(Y : Custom.T) : sig
    include Custom.T with type t = Y.t Map.map
  end
end

module type FiniteCofinite = sig
  type elem
  type s = private Finite of elem list | Cofinite of elem list
  include Custom.T with type t = s

  val empty: t
  val any: t
  val atom: elem -> t
  val cup: t -> t -> t
  val cap: t -> t -> t
  val diff: t -> t -> t
  val neg: t -> t
  val contains: elem -> t -> bool
  val disjoint: t -> t -> bool
  val is_empty: t -> bool
end

module FiniteCofinite(X : Custom.T) : FiniteCofinite with type elem = X.t

module FiniteCofiniteMap(X : Custom.T)(SymbolSet : FiniteCofinite) :
sig
  include Custom.T
    
  val empty: t
  val any: t
  val any_in_ns: X.t -> t
  val atom: X.t * SymbolSet.elem -> t
  val cup: t -> t -> t
  val cap: t -> t -> t
  val diff: t -> t -> t
  val is_empty: t -> bool

  val symbol_set: X.t -> t -> SymbolSet.t
  val contains: X.t * SymbolSet.elem -> t -> bool
  val disjoint: t -> t -> bool

  val get: t -> [ `Finite of (X.t * SymbolSet.t) list 
		| `Cofinite of (X.t * SymbolSet.t) list ]
end
