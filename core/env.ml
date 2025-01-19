module type S = sig
  type name
  type 'a t
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
  val empty : 'a t
  val singleton : name -> 'a -> 'a t
  val bind : name -> 'a -> 'a t -> 'a t
  val unbind : name -> 'a t -> 'a t
  val extend : 'a t -> 'a t -> 'a t
  val has : name -> 'a t -> bool
  val find : name -> 'a t -> 'a
  val find_opt : name -> 'a t -> 'a option
  module Dom : Utility.Set.S
  val domain : 'a t -> Dom.t
  val range : 'a t -> 'a list
  val bindings : 'a t -> (name * 'a) list

  val map : ('a -> 'b) -> 'a t -> 'b t
  val iter : (name -> 'a -> unit) -> 'a t -> unit
  val fold : (name -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val filter : (name -> 'a -> bool) -> 'a t -> 'a t
  val filter_map : (name -> 'a -> 'b option) -> 'a t -> 'b t

  val complement : 'a t -> 'a t -> 'a t
end

module Make (Ord : Utility.OrderedShow) :
  S with type name = Ord.t
    and module Dom = Utility.Set.Make(Ord) =
struct
  module M = Utility.Map.Make(Ord)

  type name = Ord.t
  type 'a t = 'a M.t

  let empty = M.empty
  let bind n v env = M.add n v env
  let unbind n env = M.remove n env
  let singleton n v = bind n v empty
  let extend = M.superimpose
  let has name env = M.mem name env
  let find name env = M.find name env
  let find_opt name env = M.find_opt name env
  module Dom = Utility.Set.Make(Ord)
  let domain map = M.fold (fun k _ -> Dom.add k) map Dom.empty
  let range map = M.fold (fun _ v l -> v::l) map []
  let bindings map = M.fold (fun k v l -> (k, v)::l) map []
  let map = M.map
  let iter = M.iter
  let fold = M.fold
  let pp = M.pp
  let show = M.show
  let filter = M.filter
  let filter_map = M.filter_map
  let complement env env' =
    fold
      (fun key value env'' ->
        if has key env
        then env''
        else bind key value env'')
    env' empty
end

module String
  = Make(Utility.String)
module Int
  = Make(Utility.Int)
