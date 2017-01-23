(* The purpose of this module is to override all functions in the
   standard library that raise the almost-useless function

      exception Not_found

   with versions that raise the slightly-more-useful

      exception NotFound of string.

   Other than in this module there should be approximately no
   occurrences of Not_found in the code: treat any appearance of
   Not_found at runtime as a bug that can probably be fixed by adding
   an override to this module.
*)

exception NotFound of string

let not_found fn v =
  let nasty_is_string x = Obj.tag (Obj.repr x) = Obj.string_tag in
    raise
    (NotFound
       (if nasty_is_string v then
          (Obj.magic v : string) ^ " (in "^fn^")"
        else
          "<non-string> (in "^fn^")"))

module Buffer =
struct
  include Buffer

  let add_substitute b f s =
    try add_substitute b f s
    with Not_found -> not_found "Buffer.add_substitute" ")"
end

(* Dbm.find and Dbm.nextkey would go here, were we ever to use the Dbm
   module.  I won't include it for now, because it would involve
   pointlessly linking unused libraries. *)

module Hashtbl =
struct
  include Hashtbl

  let lookup (tbl : ('a,'b) t) (key : 'a) : 'b option =
    try Some (find tbl key)
    with Not_found -> None

  let find tbl key =
    try find tbl key
    with Not_found -> not_found "Hashtbl.find" key
end

module List =
struct
  include List
  let find p l =
    try find p l
    with Not_found -> not_found "List.find" "<matching predicate>"

  let assoc v l =
    try assoc v l
    with Not_found -> not_found "List.assoc" v
end

module ListLabels =
struct
  include ListLabels
  let find ~f l =
    try find ~f l
    with Not_found -> not_found "ListLabels.find" "<matching predicate>"

  let assoc v l =
    try assoc v l
    with Not_found -> not_found "ListLabels.assoc" v
end

module Map =
struct
  module type OrderedType = Map.OrderedType
  module type S = Map.S
  module Make (Ord : OrderedType) = struct
    include Map.Make(Ord)

    let lookup (item: key) (map : 'a t) : 'a option =
      try Some (find item map)
      with Not_found -> None

    let find x m =
      try find x m
      with Not_found -> not_found "Map.find" x
  end
end

(* MoreLabels? *)
module MoreLabels =
struct
  open MoreLabels

  module Hashtbl =
  struct
    include Hashtbl
    let find tbl key =
      try find tbl key
      with Not_found -> not_found "MoreLabels.Hashtbl.find" key
  end

  module Map =
  struct
    module type OrderedType = Map.OrderedType
    module type S = Map.S
    module Make (Ord : OrderedType) = struct
      include Map.Make(Ord)
      let find x m =
        try find x m
        with Not_found -> not_found "MoreLabels.Map.find" x
    end
  end

  module Set =
  struct
    module type OrderedType = Set.OrderedType
    module type S = Set.S
    module Make (Ord : OrderedType) = struct
      include Set.Make(Ord)
      let min_elt t =
        try min_elt t
        with Not_found -> invalid_arg "Set.min_elt"

      let max_elt t =
        try max_elt t
        with Not_found -> invalid_arg "Set.max_elt"

      let choose t =
        try choose t
        with Not_found -> invalid_arg "Set.choose"
    end
  end
end

module Set =
struct
  module type OrderedType = Set.OrderedType
  module type S = Set.S
  module Make (Ord : OrderedType) = struct
    include Set.Make(Ord)
    let min_elt t =
      try min_elt t
      with Not_found -> invalid_arg "Set.min_elt"

    let max_elt t =
      try max_elt t
      with Not_found -> invalid_arg "Set.max_elt"

    let choose t =
      try choose t
      with Not_found -> invalid_arg "Set.choose"
  end
end

module Str =
struct
  include Str
  let search_forward r s start =
    try search_forward r s start
    with Not_found -> not_found "Str.search_forward" "<some regex>"

  let search_backward r s last =
    try search_backward r s last
    with Not_found -> not_found "Str.search_backward" "<some regex>"

  let matched_group n s =
    try matched_group n s
    with Not_found -> not_found "Str.matched_group" "<some regex>"

  let group_beginning n =
    try group_beginning n
    with Not_found -> not_found "Str.group_beginning" "<some regex>"

  let group_end n =
    try group_end n
    with Not_found -> not_found "Str.group_beginning" "<some regex>"
end

module String =
struct
  include String

  let index s c =
    try index s c
    with Not_found -> not_found "String.index" (String.make 1 c)

  let rindex s c =
    try rindex s c
    with Not_found -> not_found "String.rindex" (String.make 1 c)

  let index_from s n c =
    try index_from s n c
    with Not_found -> not_found "String.index_from" (String.make 1 c)

  let rindex_from s n c =
    try rindex_from s n c
    with Not_found -> not_found "String.rindex_from" (String.make 1 c)
end

module StringLabels =
struct
  include StringLabels

  let index s c =
    try index s c
    with Not_found -> not_found "StringLabels.index" (String.make 1 c)

  let rindex s c =
    try rindex s c
    with Not_found -> not_found "StringLabels.rindex" (String.make 1 c)

  let index_from s n c =
    try index_from s n c
    with Not_found -> not_found "StringLabels.index_from" (String.make 1 c)

  let rindex_from s n c =
    try rindex_from s n c
    with Not_found -> not_found "StringLabels.rindex_from" (String.make 1 c)
end

module Sys =
struct
  include Sys

  let getenv name =
    try getenv name
    with Not_found -> not_found "Sys.getenv" name
end

module Unix =
struct
  include Unix

  let getenv name =
    try getenv name
    with Not_found -> not_found "Unix.getenv" name

  let getpwnam x =
    try getpwnam x
    with Not_found -> not_found "Unix.getpwnam" x

  let getgrnam x =
    try getgrnam x
    with Not_found -> not_found "Unix.getgrnam" x

  let getpwuid x =
    try getpwuid x
    with Not_found -> not_found "Unix.getpwuid" x

  let getgrgid x =
    try getgrgid x
    with Not_found -> not_found "Unix.getgrgid" x

  let gethostbyname x =
    try gethostbyname x
    with Not_found -> not_found "Unix.gethostbyname" x

  let gethostbyaddr x =
    try gethostbyaddr x
    with Not_found -> not_found "Unix.gethostbyaddr" x

  let getprotobyname x =
    try getprotobyname x
    with Not_found -> not_found "Unix.getprotobyname" x

  let getprotobynumber x =
    try getprotobynumber x
    with Not_found -> not_found "Unix.getprotobynumber" x

  let getservbyname x y =
    try getservbyname x y
    with Not_found -> not_found "Unix.getservbyname" x

  let getservbyport x y =
    try getservbyport x y
    with Not_found -> not_found "Unix.getservbyport" x

  let getnameinfo x y =
    try getnameinfo x y
    with Not_found -> not_found "Unix.getnameinfo" x
end

module UnixLabels =
struct
  include Unix

  let getenv name =
    try getenv name
    with Not_found -> not_found "Unix.getenv" name

  let getpwnam x =
    try getpwnam x
    with Not_found -> not_found "Unix.getpwnam" x

  let getgrnam x =
    try getgrnam x
    with Not_found -> not_found "Unix.getgrnam" x

  let getpwuid x =
    try getpwuid x
    with Not_found -> not_found "Unix.getpwuid" x

  let getgrgid x =
    try getgrgid x
    with Not_found -> not_found "Unix.getgrgid" x

  let gethostbyname x =
    try gethostbyname x
    with Not_found -> not_found "Unix.gethostbyname" x

  let gethostbyaddr x =
    try gethostbyaddr x
    with Not_found -> not_found "Unix.gethostbyaddr" x

  let getprotobyname x =
    try getprotobyname x
    with Not_found -> not_found "Unix.getprotobyname" x

  let getprotobynumber x =
    try getprotobynumber x
    with Not_found -> not_found "Unix.getprotobynumber" x

  let getservbyname x y =
    try getservbyname x y
    with Not_found -> not_found "Unix.getservbyname" x

  let getservbyport x y =
    try getservbyport x y
    with Not_found -> not_found "Unix.getservbyport" x

  let getnameinfo x y =
    try getnameinfo x y
    with Not_found -> not_found "Unix.getnameinfo" x
end
