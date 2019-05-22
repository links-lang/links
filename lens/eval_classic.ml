open Value
open Phrase_value
module Sorted = Sorted_records

let lens_put_set_step lens data (fn : Value.t -> Sorted.t -> unit) =
  let get l =
    let dat = Value.lens_get l in
    Sorted.construct_cols ~columns:(Value.cols_present_aliases l) ~records:dat
  in
  match lens with
  | Lens _ -> fn lens data
  | LensDrop {lens= l; drop; key; default; _} ->
      let r = get l in
      let columns = Value.cols_present_aliases lens in
      let nplus = Sorted.minus data (Sorted.project_onto r ~columns) in
      let a = Sorted.construct ~records:[box_record [(drop, default)]] in
      let on_left = List.map (fun v -> (v, v, v)) columns in
      let m =
        Sorted.merge
          (Sorted.join r data ~on:on_left)
          (Sorted.join nplus a ~on:[])
      in
      let res =
        Sorted.relational_update
          ~fun_deps:(Fun_dep.Set.of_lists [([key], [drop])])
          m ~update_with:r
      in
      fn l res
  | LensJoin {left; right; on; del_left; del_right; _} ->
      let getfds l = Value.fundeps l in
      let cols l = Value.cols_present_aliases l in
      let r = get left in
      let s = get right in
      let m0 =
        Sorted.relational_update ~fun_deps:(getfds left)
          ~update_with:(Sorted.project_onto data ~columns:(cols left))
          r
      in
      let n0 =
        Sorted.relational_update ~fun_deps:(getfds right)
          ~update_with:(Sorted.project_onto data ~columns:(cols right))
          s
      in
      let l = Sorted.minus (Sorted.join m0 n0 ~on) data in
      let join_cols = cols lens |> List.map (fun v -> (v, v, v)) in
      let ll = Sorted.join l data ~on:join_cols in
      let la = Sorted.minus l ll in
      let m =
        Sorted.minus
          (Sorted.minus m0
             (Sorted.project_onto
                (Sorted.filter la ~predicate:del_left)
                ~columns:(cols left)))
          (Sorted.project_onto ll ~columns:(cols left))
      in
      let n =
        Sorted.minus n0
          (Sorted.project_onto
             (Sorted.filter la ~predicate:del_right)
             ~columns:(cols right))
      in
      fn left m ; fn right n
  | LensSelect {lens= l; predicate; _} ->
      let sort = Value.sort l in
      let r = get l in
      let m1 =
        Sorted.relational_update ~fun_deps:(Sort.fds sort) ~update_with:data
          (Sorted.filter r ~predicate)
      in
      let nh = Sorted.minus (Sorted.filter m1 ~predicate) data in
      let r = Sorted.minus m1 nh in
      fn l r
  | _ -> ()

let apply_table_data ~table ~database data =
  let open Database in
  let open Database.Table in
  let table = table.name in
  let db = database in
  let show_query = false in
  let exec cmd = Statistics.time_query (fun () -> database.execute cmd) in
  let cmd = "delete from " ^ database.quote_field table ^ " where TRUE" in
  let _ = exec cmd in
  if show_query then print_endline cmd else () ;
  let columns = Sorted.columns data in
  let values = Sorted.plus_rows data |> Array.to_list in
  let fmt_insert f values =
    let values = [values] in
    let insert = {Database.Insert.table; columns; values; db} in
    Database.Insert.fmt f insert
  in
  let fmt_cmd_sep f () = Format.pp_print_string f ";\n" in
  let fmt_all f () =
    Format.pp_print_list ~pp_sep:fmt_cmd_sep fmt_insert f values
  in
  let cmds = Format.asprintf "%a" fmt_all () in
  if String.equal "" cmds |> not then (
    if show_query then print_endline cmds ;
    exec cmds |> ignore )

let lens_put_step lens data (fn : Value.t -> Sorted.t -> unit) =
  let data =
    Sorted.construct_cols
      ~columns:(Value.cols_present_aliases lens)
      ~records:data
  in
  lens_put_set_step lens data fn

let lens_put (lens : Value.t) (data : Phrase_value.t list) =
  let rec do_step_rec lens data =
    match lens with
    | Lens {table; database; _} -> apply_table_data ~table ~database data
    | _ -> lens_put_set_step lens data do_step_rec
  in
  do_step_rec lens
    (Sorted.construct_cols
       ~columns:(Value.cols_present_aliases lens)
       ~records:data)
