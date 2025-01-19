module type S = sig
  include Map.S

  val pp : 'a Lens_format.fmt_fn -> 'a t Lens_format.fmt_fn

  val show : 'a Lens_format.fmt_fn -> 'a t -> string

  val find_exn : 'a t -> key:key -> 'a

  val find : 'a t -> key:key -> 'a option

  val from_alist : (key * 'a) list -> 'a t

  val to_list : (key -> 'a -> 'b) -> 'a t -> 'b list
end
