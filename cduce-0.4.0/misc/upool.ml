(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

type 'a typed_int = int
external int: 'a typed_int -> int = "%identity"

module type S = sig
  type token
  type value
  include Custom.T with type t = token typed_int
  exception Not_unique of value * value

  val dummy: t
  val min: t -> t -> t
  val mk: value -> t
  val value: t -> value

  val extract: unit -> value array
  val intract: value array -> unit

  val from_int: int -> t
end

module HInt = Hashtbl.Make(struct type t = int
				  let hash x = x
				  let equal x y = x==y end)

module Make(X : Custom.T) = struct
  type token
  type value = X.t
  type t = token typed_int

  let min = min

  exception Not_unique of value * value
  let compare (x:int) y = if (x=y) then 0 else if (x < y) then (-1) else 1
  let hash x = x
  let equal x y = x==y

  let pool = HInt.create 1024
  let dummy = max_int

  let mk v =
    let h = X.hash v in
    if (h == dummy) then raise (Not_unique (v,v));
    (try 
       let v' = HInt.find pool h in 
       if not (X.equal v v') then raise (Not_unique (v,v'));
     with Not_found -> HInt.add pool h v);
    h

(*  let value h = 
    assert (h != dummy);
    try HInt.find pool h
    with Not_found -> assert false *)

  let value h = HInt.find pool h

  let extract () = Array.of_list (HInt.fold (fun _ v accu -> v::accu) pool [])
  let intract = Array.iter (fun v -> ignore (mk v))

  let check _ = ()
  let dump ppf _ = ()

  let from_int i = i
end
