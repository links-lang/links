module type Show =
  sig
    type a
    val format : Format.formatter -> a -> unit
    val formatList : Format.formatter -> a list -> unit
    val show : a -> string
    val showList : a list -> string
  end

module ShowDefaults (S : 
  sig
    type a
    val format : Format.formatter -> a -> unit 
  end) : Show with type a = S.a

module Show_unprintable (S : sig type a end) : Show with type a = S.a

module Show_list (S : Show)   : Show with type a = S.a list
module Show_ref (S : Show)    : Show with type a = S.a ref
module Show_option (S : Show) : Show with type a = S.a option
module Show_array (S : Show)  : Show with type a = S.a array

module Show_map
  (O : Map.OrderedType) 
  (K : Show with type a = O.t)
  (V : Show)
  : Show with type a = V.a Map.Make(O).t

module Show_set
  (O : Set.OrderedType) 
  (K : Show with type a = O.t)
  : Show with type a = Set.Make(O).t
