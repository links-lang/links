module type S = sig
  type t
  val compare: t -> t -> int
  val hash: t -> int
  val equal: t -> t -> bool
end

module MakeMap(X : S) : Map.S with type key = X.t = struct

  type key = X.t 
  module XMap = Map.Make(X)

  type 'a t = 'a XMap.t Ptmap.t

  let add x y t =
    Ptmap.set (X.hash x) 
      (function None -> XMap.add x y (XMap.empty)
	 | Some s -> XMap.add x y s) t
      
  let find x t =
    XMap.find x (Ptmap.find (X.hash x) t)

  let mem x t =
    try XMap.mem x (Ptmap.find (X.hash x) t)
    with Not_found -> false

  let is_empty t = Ptmap.is_empty t

  let empty = Ptmap.empty

  let iter f t =
    Ptmap.iter (fun _ s -> XMap.iter f s) t

  let map f t =
    Ptmap.map (XMap.map f) t

  let mapi f t =
    Ptmap.map (XMap.mapi f) t

  let fold f t accu =
    Ptmap.fold (fun _ s accu -> XMap.fold f s accu) t accu

  let remove x t =
    Ptmap.unset (X.hash x)
      (fun s ->
	 let s = XMap.remove x s in
	 if XMap.is_empty s then None else Some s) t

  let equal f t1 t2 =
    Ptmap.equal (XMap.equal f) t1 t2

  let compare f t1 t2 =
    Ptmap.compare (XMap.compare f) t1 t2

end

module type SET = sig
  type elt
  type t
  val empty: t
  val is_empty: t -> bool
  val mem: elt -> t -> bool
  val add: elt -> t -> t
  val singleton: elt -> t
  val remove: elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t 
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val subset: t -> t -> bool
  val iter: (elt -> unit) -> t -> unit
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val hash: t -> int
  val is_singleton: t -> elt option
  val disjoint: t -> t -> bool
  val diff: t -> t -> t
  val elements: t -> elt list
  val of_list: elt list -> t
end

module MakeSet(X : S) : SET with type elt = X.t = struct

  type elt = X.t 
  module XSet = Set.Make(struct type t = X.t
				let compare x y = X.compare x y end)

  type t = XSet.t Ptmap.t

  let empty = Ptmap.empty
  let is_empty = Ptmap.is_empty

  let mem x t =
    try XSet.mem x (Ptmap.find (X.hash x) t)
    with Not_found -> false

  let add x t =
    Ptmap.set (X.hash x) 
      (function None -> XSet.add x (XSet.empty)
	 | Some s -> XSet.add x s) t
      

  let singleton x =
    Ptmap.singleton (X.hash x) (XSet.singleton x)

  let remove x t =
    Ptmap.unset (X.hash x)
      (fun s ->
	 let s = XSet.remove x s in
	 if XSet.is_empty s then None else Some s) t

  let union t1 t2 =
    Ptmap.union XSet.union t1 t2

  let compare t1 t2 =
    Ptmap.compare XSet.compare t1 t2

  let equal t1 t2 =
    Ptmap.equal XSet.equal t1 t2

  let subset t1 t2 =
    Ptmap.subset XSet.subset t1 t2

  let iter f t =
    Ptmap.iter (fun _ s -> XSet.iter f s) t

  let fold f t accu =
    Ptmap.fold (fun _ s accu -> XSet.fold f s accu) t accu

  let hash t =
    Ptmap.hash XSet.cardinal t

  let is_singleton =
    let xset_singleton t =
      try XSet.fold (fun x accu ->
		       match accu with None -> Some x
			 | Some _ -> raise Exit) t None
      with Exit -> None in
    fun t -> Ptmap.is_singleton xset_singleton t


  let disjoint =
    let xset_disjoint t1 t2 = not (XSet.exists (fun x -> XSet.mem x t2) t1) in
    fun t1 t2 -> Ptmap.disjoint xset_disjoint t1 t2

  let diff t1 t2 =
    Ptmap.diff (fun s1 s2 ->
		  let s = XSet.diff s1 s2 in
		  if XSet.is_empty s then None else Some s) t1 t2

  let inter t1 t2 =
    Ptmap.inter (fun s1 s2 ->
		  let s = XSet.inter s1 s2 in
		  if XSet.is_empty s then None else Some s) t1 t2

  let elements t =
    fold (fun x accu -> x::accu) t []

  let of_list l =
    List.fold_left (fun accu x -> add x accu) empty l
end
