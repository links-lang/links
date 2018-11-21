type t = Types.lens_sort

let fds t = t.fds
let predicate t = t.predicate
let cols t = t.cols

let cols_present_aliases (sort : t) =
    let cols = cols sort in
    LensColList.present_aliases cols

let colset (sort:t) =
    let columns = cols sort in
    let columns = LensColList.present_aliases columns in
    ColSet.of_list columns

let make ~fds ~predicate ~typ =
  { fds; predicate; typ }

let find_col_alias (alias : string) (sort : t) =
    LensColList.find_alias alias (cols sort)
