module L = Lens

include module type of L.Fun_dep

val colslist_of_string : string -> string list

val colset_of_string : string -> L.Alias.Set.t

val of_string : string -> t

module Set : sig
  include module type of L.Fun_dep.Set

  val of_string : string -> t
end
