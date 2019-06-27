open Value
open Lens_utility
module Sorted = Sorted_records

let matches_change changes =
  let is_changed ((cols_l, _cols_r), vals) =
    let vals_l = List.map ~f:(fun (left, _) -> left) vals in
    Phrase.Option.in_expr cols_l vals_l
  in
  List.map ~f:is_changed changes |> Phrase.List.fold_or_opt

let delta_merge_affected lens data =
  let sort = Value.sort lens in
  let fun_deps = Sort.fds sort in
  let changelist = Sorted.calculate_fd_changelist data ~fun_deps in
  (* query relevant rows in database *)
  let predicate = matches_change changelist in
  let res = Value.lens_get_select_opt lens ~predicate in
  let res =
    Sorted.construct_cols
      ~columns:(Value.cols_present_aliases lens)
      ~records:res
  in
  (* perform relational update *)
  let rel_merge =
    Sorted_records.relational_merge res ~update_with:data ~fun_deps
  in
  Sorted.merge rel_merge (Sorted.negate res)

let query_join_records lens set on =
  let proj = Sorted.project_onto set ~columns:on in
  let recs = Sorted.all_values proj in
  let predicate = Phrase.Option.in_expr on recs in
  let records = Value.lens_get_select_opt lens ~predicate in
  Sorted.construct_cols ~columns:(Value.cols_present_aliases lens) ~records

let query_project_records lens set key drop =
  let set = Sorted.force_positive set in
  let proj = Sorted.project_onto set ~columns:key in
  let recs = Sorted.all_values proj in
  let predicate = Phrase.Option.in_expr key recs in
  let records = Value.lens_get_select_opt lens ~predicate in
  let records =
    Sorted.construct_cols ~columns:(Value.cols_present_aliases lens) ~records
  in
  Sorted.project_onto records ~columns:(List.append key drop)

let lens_put_set_step lens delt (fn : Value.t -> Sorted.t -> unit) =
  match lens with
  | Lens _ -> fn lens delt
  | LensDrop {lens= l; drop; key; default; _} ->
      let relevant = query_project_records l delt [key] [drop] in
      let delt =
        Sorted.relational_extend delt ~key ~by:drop ~default ~data:relevant
      in
      fn l delt
  | LensJoin {left; right; on; del_left; del_right; _} ->
      let cols_simp = List.map ~f:(fun (a, _, _) -> a) on in
      let sort1 = Value.sort left in
      let proj1 =
        Sorted.project_onto delt ~columns:(Sort.cols_present_aliases sort1)
      in
      let sort2 = Value.sort right in
      let proj2 =
        Sorted.project_onto delt ~columns:(Sort.cols_present_aliases sort2)
      in
      let delta_m0 = delta_merge_affected left proj1 in
      let delta_n0 = delta_merge_affected right proj2 in
      let on' = List.map ~f:(fun a -> (a, a, a)) cols_simp in
      let delta_l =
        Sorted.merge
          (Sorted.merge
             (Sorted.join delta_m0 delta_n0 ~on:on')
             (Sorted.merge
                (Sorted.join delta_m0
                   (query_join_records right delta_m0 cols_simp)
                   ~on:on')
                (Sorted.join delta_n0
                   (query_join_records left delta_n0 cols_simp)
                   ~on:on')))
          (Sorted.negate delt)
      in
      let j =
        Sorted.project_onto
          (Sorted.merge (query_join_records lens delta_l cols_simp) delt)
          ~columns:cols_simp
      in
      let delta_l_l = Sorted.join delta_l j ~on:on' in
      let delta_l_a = Sorted.merge delta_l (Sorted.negate delta_l_l) in
      let delta_m =
        Sorted.merge
          (Sorted.merge delta_m0
             (Sorted.negate
                (Sorted.project_onto_set
                   (Sorted.filter delta_l_a ~predicate:del_left)
                   ~onto:delta_m0)))
          (Sorted.negate (Sorted.project_onto_set delta_l_l ~onto:delta_m0))
      in
      let delta_n =
        Sorted.merge delta_n0
          (Sorted.negate
             (Sorted.project_onto_set
                (Sorted.filter delta_l_a ~predicate:del_right)
                ~onto:delta_n0))
      in
      fn left delta_m ; fn right delta_n
  | LensSelect {lens; predicate; _} ->
      let delta_m1 =
        Sorted.merge
          (delta_merge_affected
             (Value.lens_select_internal lens
                ~predicate:(Phrase.not' predicate))
             delt)
          (Sorted.negative delt)
      in
      let m1_cap_P = Sorted.filter delta_m1 ~predicate in
      let delta_nhash = Sorted.merge m1_cap_P (Sorted.negate delt) in
      let new_delta = Sorted.merge delta_m1 (Sorted.negate delta_nhash) in
      fn lens new_delta
  | _ -> failwith "Unsupport lens."

let lens_get_delta lens data =
  let columns = Value.cols_present_aliases lens in
  let orig = Sorted.construct_cols ~columns ~records:(Value.lens_get lens) in
  let data =
    Sorted.merge
      (Sorted.construct_cols ~columns ~records:data)
      (Sorted.negate orig)
  in
  data

let lens_put_step lens data (fn : Value.t -> Sorted.t -> unit) =
  let data = lens_get_delta lens data in
  lens_put_set_step lens data fn

let rec take (l : 'a list) (n : int) =
  match n with
  | 0 -> []
  | _ -> List.hd l :: take (List.tl l) (n - 1)

let rec skip (l : 'a list) (n : int) =
  match n with
  | 0 -> l
  | _ -> skip (List.tl l) (n - 1)

let apply_delta ~table ~database:db data =
  let {Database.Table.name= table; keys} = table in
  let exec cmd =
    let open Database in
    Debug.print cmd ;
    Statistics.time_query (fun () -> db.execute cmd)
  in
  (* get the first key, otherwise return an empty key *)
  let key =
    match keys with
    | [] -> Sorted.columns data
    | _ -> List.hd keys
  in
  let columns, (insert_vals, update_vals, delete_vals) =
    Sorted.to_diff data ~key
  in
  let prepare_where row =
    List.zip_nofail key row
    |> List.map ~f:(fun (k, v) ->
           Phrase.equal (Phrase.var k) (Phrase.Constant.of_value v))
    |> Phrase.List.fold_and
  in
  let fmt_cmd_sep f () = Format.pp_print_string f ";\n" in
  let fmt_delete f row =
    let predicate = prepare_where row in
    let delete = {Database.Delete.table; predicate; db} in
    Database.Delete.fmt f delete
  in
  let fmt_update f row =
    let predicate = prepare_where row in
    let set = List.zip_exn columns row |> List.drop ~n:(List.length key) in
    let update = {Database.Update.table; predicate; db; set} in
    Database.Update.fmt f update
  in
  let fmt_insert f values =
    let values = [values] in
    let insert = {Database.Insert.table; columns; values; db} in
    Database.Insert.fmt f insert
  in
  let fmt_insert_cmds = List.map ~f:(fun v -> (fmt_insert, v)) insert_vals in
  let fmt_delete_cmds = List.map ~f:(fun v -> (fmt_delete, v)) delete_vals in
  let fmt_update_cmds = List.map ~f:(fun v -> (fmt_update, v)) update_vals in
  let fmt_fmt f (fmt, v) = fmt f v in
  let fmt_all f () =
    Format.pp_print_list ~pp_sep:fmt_cmd_sep fmt_fmt f
    @@ List.flatten [fmt_insert_cmds; fmt_delete_cmds; fmt_update_cmds]
  in
  let cmds = Format.asprintf "%a" fmt_all () in
  if String.equal "" cmds |> not then exec cmds

let get_fds (fds : (string list * string list) list) (cols : Column.t list) :
    Fun_dep.Set.t =
  let check_col xs =
    List.iter
      ~f:(fun alias ->
        if not (Column.List.mem_alias cols ~alias) then
          failwith ("The column " ^ alias ^ " does not exist."))
      xs
  in
  List.iter ~f:(fun (left, right) -> check_col left ; check_col right) fds ;
  let fd_of (left, right) =
    Fun_dep.make (Alias.Set.of_list left) (Alias.Set.of_list right)
  in
  Fun_dep.Set.of_list (List.map ~f:fd_of fds)

let lens_put (lens : Value.t) (data : Phrase_value.t list) =
  let rec do_step_rec lens delt =
    match lens with
    | Lens {table; database; _} -> apply_delta ~table ~database delt
    | _ -> lens_put_set_step lens delt do_step_rec
  in
  do_step_rec lens (lens_get_delta lens data)
