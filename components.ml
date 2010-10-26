open Utility

module type AListType =
sig
  type key
  type value
  val incr : key -> int -> key
  val decr : key -> int -> key
  val col : key -> int
end

(* output signature *)
module type S =
sig
  type key
  type value
  type t = (key * value) list
  val empty : t
  val keys : t -> key list 
  val columns : t -> int list
  val incr_cols : t -> int -> t
  val decr_cols : t -> int -> t
  val append : t -> t -> t
  val lookup : t -> key -> value
  val length : t -> int
  val lookup_col : t -> int -> t
  val keep_cols : t -> int list -> t
end

module Make (AList : AListType) : (S with type key = AList.key and type value = AList.value) = 
struct
  type key = AList.key
  type value = AList.value
  type t = (AList.key * AList.value) list
  let empty = []
  let keys (al : t) = List.map fst al
  let columns (al : t) = List.map (AList.col -<- fst) al
  let incr_cols (al : t) i = List.map (fun (key, value) -> (AList.incr key i, value)) al
  let decr_cols (al : t) i = List.map (fun (key, value) -> (AList.decr key i, value)) al
  let append (l : t) (r : t) = List.append l r
  let lookup (al : t) (k : key) = List.assoc k al
  let length (l : t) = List.length l
  let lookup_col (al : t) c = List.filter (fun (key, _) -> (AList.col key) = c) al
  let keep_cols (al : t) cols = List.filter (fun (key, _) -> List.mem (AList.col key) cols) al
end

module Default =
struct
  let incr = (+)
  let decr = (-)
  let col = identity
end

