open Lens.Utility

module Alias = Lens.Alias
module Sort = Lens.Sort
module Value = Lens.Value

let default_join_cols ?on left right =
  let on =
    match on with
    | None ->
        let left_cols = Lens.Value.sort left |> Sort.colset in
        let right_cols = Lens.Value.sort right |> Sort.colset in
        let join_cols =
          Lens.Column.Set.inter left_cols right_cols
          |> Lens.Column.Set.alias_set
        in
        Alias.Set.elements join_cols
    | Some s -> s
  in
  List.map ~f:(fun a -> (a, a, a)) on

let join_dl ?on left right =
  let on = default_join_cols ?on left right in
  let sort, on =
    Lens.Sort.join_lens_sort (Lens.Value.sort left) (Lens.Value.sort right) ~on
    |> Result.ok_exn
  in
  let del_left = Lens.Phrase.Constant.bool true in
  let del_right = Lens.Phrase.Constant.bool false in
  Value.LensJoin { left; right; on; del_left; del_right; sort }

let join_db ?on left right =
  let on = default_join_cols ?on left right in
  let sort, on =
    Lens.Sort.join_lens_sort (Lens.Value.sort left) (Lens.Value.sort right) ~on
    |> Result.ok_exn
  in
  let del_left = Lens.Phrase.Constant.bool true in
  let del_right = Lens.Phrase.Constant.bool true in
  Value.LensJoin { left; right; on; del_left; del_right; sort }

let join_dr ?on left right =
  let on = default_join_cols ?on left right in
  let sort, on =
    Lens.Sort.join_lens_sort (Lens.Value.sort left) (Lens.Value.sort right) ~on
    |> Result.ok_exn
  in
  let del_left = Lens.Phrase.Constant.bool false in
  let del_right = Lens.Phrase.Constant.bool true in
  Value.LensJoin { left; right; on; del_left; del_right; sort }

let select ~predicate lens =
  let sort = Lens.Value.sort lens in
  let sort = Lens.Sort.select_lens_sort sort ~predicate |> Result.ok_exn in
  Lens.Value.LensSelect { lens; predicate; sort }

let drop lens drop key default =
  let key' = Alias.Set.singleton key in
  let sort = Value.sort lens in
  let sort =
    Sort.drop_lens_sort sort ~drop:[ drop ] ~default:[ default ] ~key:key'
    |> Result.ok_exn
  in
  Value.LensDrop { lens; drop; key; default; sort }

let select_query l predicate =
  let predicate = Some predicate in
  Lens.Value.lens_get_select_opt l ~predicate
