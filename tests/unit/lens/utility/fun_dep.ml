open OUnit2
module L = Lens
open Lens.Utility

include L.Fun_dep

let colslist_of_string str =
  let cols = String.split_on_char ' ' str in
  let cols = List.filter (fun s -> String.length s <> 0) cols in
  cols

let colset_of_string str =
  let cols = colslist_of_string str in
  Lens.Alias.Set.of_list cols

let of_string str =
  let split = Str.split (Str.regexp "->") str in
  let _ = assert_equal (List.length split) 2 in
  let colsets = List.map ~f:colset_of_string split in
  Lens.Fun_dep.make (List.nth colsets 0) (List.nth colsets 1)

module Set = struct
  include L.Fun_dep.Set

  let of_string str =
    let split = Str.split (Str.regexp ";") str in
    let fds = List.map ~f:of_string split in
    Lens.Fun_dep.Set.of_list fds
end
