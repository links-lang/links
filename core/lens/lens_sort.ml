open Types

type t = lens_sort

let fds t = t.fds
let predicate t = t.predicate
let cols t = t.cols

let cols_present_aliases t =
    Lens_column.List.present_aliases t.cols

let colset t =
  t.cols
  |> Lens_column.Set.of_list

let present_colset t =
  t.cols
  |> Lens_column.List.present
  |> Lens_column.Set.of_list

let make ?(fds=Lens_fun_dep.Set.empty) ?(predicate=None) cols =
  { fds; predicate; cols }

let find_col_alias t ~alias =
  Lens_column.List.find_alias ~alias t.cols

let record_type t =
  cols t
  |> Lens_column.List.record_type

let update_table_name t ~table =
  let cols = t.cols |> List.map (fun c -> { c with table }) in
  { t with cols }

let update_predicate t ~predicate =
  { t with predicate }

let equal sort1 sort2 =
  let fd_equal = Lens_fun_dep.Set.equal (fds sort1) (fds sort2) in
  let pred_equal = (predicate sort1) = (predicate sort2) in
  let cols_equal = Lens_column.Set.equal (colset sort1) (colset sort2) in
  fd_equal && pred_equal && cols_equal
