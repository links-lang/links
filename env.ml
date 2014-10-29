open Notfound

module type S =
sig
  type name
  type 'a t
  module Show_t (A : Deriving_Show.Show) :
    Deriving_Show.Show with type a = A.a t
  val empty : 'a t
  val bind : 'a t -> name * 'a -> 'a t
  val unbind : 'a t -> name -> 'a t
  val extend : 'a t -> 'a t -> 'a t
  val has : 'a t -> name -> bool
  val lookup : 'a t -> name -> 'a
  val find : 'a t -> name -> 'a option
  module Dom : Utility.Set.S
  val domain : 'a t -> Dom.t
  val range : 'a t -> 'a list

  val map : ('a -> 'b) -> 'a t -> 'b t
  val iter : (name -> 'a -> unit) -> 'a t -> unit
  val fold : (name -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module Make (Ord : Utility.OrderedShow) :
  S with type name = Ord.t
    and module Dom = Utility.Set.Make(Ord) =
struct
  module M = Utility.Map.Make(Ord)

  type name = Ord.t
  type 'a t = 'a M.t

  let empty = M.empty
  let bind env (n,v) = M.add n v env
  let unbind env n = M.remove n env
  let extend = M.superimpose
  let has env name = M.mem name env
  let lookup env name = M.find name env
  let find env name = M.lookup name env
  module Dom = Utility.Set.Make(Ord)
  let domain map = M.fold (fun k _ -> Dom.add k) map Dom.empty
  let range map = M.fold (fun _ v l -> v::l) map []
  let map = M.map
  let iter = M.iter
  let fold = M.fold
  module Show_t = M.Show_t
end

module String
  = Make(Utility.String)
module Int
  = Make(Utility.Int)
