open Value
open Utility
open Lens_types
open LensHelpersIncremental

module Sorted = Lens_sorted_records

let lens_put_set_step lens data (fn : Value.t -> Sorted.t -> unit) =
  let get l =
    let dat = Lens_value.lens_get l in
    Sorted.construct_cols ~columns:(Lens_value.cols_present_aliases l) ~records:dat in
  match lens with
  | `Lens _ -> fn lens data
  | `LensDrop (l, drop, key, default, _sort) ->
    let r = get l in
    let columns = Lens_value.cols_present_aliases lens in
    let nplus = Sorted.minus data (Sorted.project_onto r ~columns) in
    let a = Sorted.construct ~records:(box_list [box_record [drop, default]]) in
    let on_left = List.map (fun v -> v,v,v) columns in
    let m = Sorted.merge (Sorted.join r data ~on:on_left) (Sorted.join nplus a ~on:[]) in
    let res = Sorted.relational_update ~fun_deps:(Fun_dep.Set.of_lists [[key], [drop]]) m ~update_with:r in
    fn l res
  | `LensJoin (l1, l2, join, pd, qd, _sort)  ->
    let getfds l = Lens_value.fundeps l in
    let cols l = Lens_value.cols_present_aliases l in
    let r = get l1 in
    let s = get l2 in
    let m0 = Sorted.relational_update
        ~fun_deps:(getfds l1)
        ~update_with:(Sorted.project_onto data ~columns:(cols l1)) r in
    let n0 = Sorted.relational_update
        ~fun_deps:(getfds l2)
        ~update_with:(Sorted.project_onto data ~columns:(cols l2)) s in
    let l = Sorted.minus (Sorted.join m0 n0 ~on:join) data in
    let join_cols = cols lens |> List.map (fun v -> v,v,v) in
    let ll = Sorted.join l data ~on:join_cols in
    let la = Sorted.minus l ll in
    let m = Sorted.minus (
        Sorted.minus m0 (Sorted.project_onto (Sorted.filter la ~predicate:pd) ~columns:(cols l1))
      ) (Sorted.project_onto ll ~columns:(cols l1)) in
    let n = Sorted.minus n0 (Sorted.project_onto (Sorted.filter la ~predicate:qd) ~columns:(cols l2)) in
    fn l1 m;
    fn l2 n
  | `LensSelect (l, predicate, _sort) ->
    let sort = Lens_value.sort l in
    let r = get l in
    let m1 = Sorted.relational_update ~fun_deps:(Sort.fds sort) ~update_with:data (Sorted.filter r ~predicate) in
    let nh = Sorted.minus (Sorted.filter m1 ~predicate) data in
    let r = Sorted.minus m1 nh in
    fn l r
  | _ -> ()

let apply_table_data t data =
  let show_query = false in
  let (db, _), table, _keys, _ = t in
  let exec cmd = Lens_statistics.time_query (fun () -> db#exec cmd) in
  let cmd = "delete from " ^ db#quote_field table ^ " where TRUE" in
  let _ = exec cmd in
  if show_query then print_endline cmd else ();
  let cols = Sorted.columns data in
  let insert_vals = List.map (fun row ->
      List.map (db_string_of_value db) row) (Sorted.plus_rows data |> Array.to_list) in
  if insert_vals <> [] then
    begin
      let insert_cmd = db#make_insert_query (table, cols, insert_vals) in
      if show_query then print_endline insert_cmd else ();
      let _ = exec insert_cmd in
      ()
    end;
  ()

let lens_put_step lens data (fn : Value.t -> Sorted.t -> unit) =
  let data = Sorted.construct_cols ~columns:(Lens_value.cols_present_aliases lens) ~records:data in
  lens_put_set_step lens data fn

let lens_put (lens : Value.t) (data : Value.t) =
  let rec do_step_rec lens data =
    match lens with
    | `Lens (t,_) -> apply_table_data t data
    | _ -> lens_put_set_step lens data do_step_rec in
  do_step_rec lens (Sorted.construct_cols ~columns:(Lens_value.cols_present_aliases lens) ~records:data)
