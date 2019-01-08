open Value
open Utility
open Lens_types
open Lens_utility

module Sorted = Lens_sorted_records

let matches_change changes =
  let is_changed ((cols_l, _cols_r),(vals)) =
    let vals_l = List.map (fun (left,_) -> left) vals in
    Lens_phrase.Option.in_expr cols_l vals_l in
  List.map is_changed changes |> Lens_phrase.List.fold_or_opt

let get_changes lens data =
  let sort = Lens_value.sort lens in
  let fun_deps = Sort.fds sort in
  let changelist = Sorted.calculate_fd_changelist data ~fun_deps in
  (* query relevant rows in database *)
  let predicate = matches_change changelist in
  let res = Lens_value.lens_get_select_opt lens ~predicate in
  let res = Sorted.construct_cols ~columns:(Lens_value.cols_present_aliases lens) ~records:res in
  (* perform relational update *)
  Lens_sorted_records.relational_update res ~update_with:data ~fun_deps

let query_join_records lens set on =
  let proj = Sorted.project_onto set ~columns:on in
  let recs = Sorted.all_values proj in
  let predicate = Lens_phrase.Option.in_expr on recs in
  let records = Lens_value.lens_get_select_opt lens ~predicate in
  Sorted.construct_cols ~columns:(Lens_value.cols_present_aliases lens) ~records

let query_project_records lens set key drop =
  let proj = Sorted.project_onto set ~columns:key in
  let recs = Sorted.all_values proj in
  let predicate = Lens_phrase.Option.in_expr key recs in
  let records = Lens_value.lens_get_select_opt lens ~predicate in
  let records = Sorted.construct_cols ~columns:(Lens_value.cols_present_aliases lens) ~records in
  Sorted.project_onto records ~columns:(List.append key drop)

let lens_put_set_step lens delt (fn : Value.t -> Sorted.t -> unit) =
  match lens with
  | `Lens _ -> fn lens delt
  | `LensDrop (l, drop, key, default, _sort) ->
    let relevant = query_project_records l delt [key] [drop] in
    let delt = Sorted.relational_extend delt ~key ~by:drop ~default ~data:relevant in
    fn l delt
  | `LensJoin (l1, l2, on, pd, qd, _sort)  ->
    let cols_simp = List.map (fun (a,_,_) -> a) on in
    let sort1 = Lens_value.sort l1 in
    let proj1 = Sorted.project_onto delt ~columns:(Sort.cols_present_aliases sort1) in
    let sort2 = Lens_value.sort l2 in
    let proj2 = Sorted.project_onto delt ~columns:(Sort.cols_present_aliases sort2) in
    let delta_m0 = get_changes l1 proj1 in
    let delta_n0 = get_changes l2 proj2 in
    let on' = List.map (fun a -> a,a,a) cols_simp in
    let delta_l =
      Sorted.merge
        (Sorted.merge
           (Sorted.join delta_m0 delta_n0 ~on:on')
           (Sorted.merge
              (Sorted.join delta_m0 (query_join_records l2 delta_m0 cols_simp) ~on:on')
              (Sorted.join delta_n0 (query_join_records l1 delta_n0 cols_simp) ~on:on')
           )
        )
        (Sorted.negate delt) in
    Format.eprintf "On: %a, delta l: %a%!\n"
      Format.pp_comma_string_list cols_simp Format.pp_comma_string_list (Sorted.columns delta_l);
    let j = Sorted.project_onto (Sorted.merge (query_join_records lens delta_l cols_simp) (delt)) ~columns:cols_simp in
    let delta_l_l = Sorted.join delta_l j ~on:on' in
    let delta_l_a = Sorted.merge (delta_l) (Sorted.negate delta_l_l) in
    let delta_m = Sorted.merge
        (Sorted.merge delta_m0 (Sorted.negate (Sorted.project_onto_set (Sorted.filter delta_l_a ~predicate:pd) ~onto:delta_m0)))
        (Sorted.negate (Sorted.project_onto_set delta_l_l ~onto:delta_m0)) in
    let delta_n = Sorted.merge
        delta_n0
        (Sorted.negate (Sorted.project_onto_set (Sorted.filter delta_l_a ~predicate:qd) ~onto:delta_n0)) in
    fn l1 delta_m;
    fn l2 delta_n
  | `LensSelect (l, predicate, _sort) ->
    let delta_m1 = Sorted.merge (get_changes (Lens_value.lens_select l ~predicate:(Lens_phrase.not' predicate)) delt)
        (Sorted.negative delt) in
    let m1_cap_P = Sorted.filter delta_m1 ~predicate in
    let delta_nhash = Sorted.merge (m1_cap_P) (Sorted.negate delt) in
    let new_delta = Sorted.merge delta_m1 (Sorted.negate delta_nhash) in
    fn l new_delta
  | _ -> failwith "Unsupport lens."

let lens_get_delta lens data =
  let columns = Lens_value.cols_present_aliases lens in
  let orig = Sorted.construct_cols ~columns ~records:(Lens_value.lens_get lens) in
  let data = Sorted.merge (Sorted.construct_cols ~columns ~records:data) (Sorted.negate orig) in
  data

let lens_put_step lens data (fn : Value.t -> Sorted.t -> unit) =
  let data = lens_get_delta lens data in
  lens_put_set_step lens data fn

let db_string_of_value (db : Lens_database.t) v =
  match v with
  | `Int i -> string_of_int i
  | `String s -> "'" ^ db#escape_string s ^ "'"
  | `Float f -> string_of_float f
  | _ -> failwith ("db_string_of_value does not support " ^ string_of_value v)

let rec take (l : 'a list) (n : int) =
  match n with
  | 0 -> []
  | _ -> List.hd l :: take (List.tl l) (n - 1)

let rec skip (l : 'a list) (n : int) =
  match n with
  | 0 -> l
  | _ -> skip (List.tl l) (n - 1)

let apply_delta t data =
  let show_query = false in
  let (db, _), table, keys, _ = t in
  let exec cmd =
    Lens_statistics.time_query (fun () -> db#exec cmd) in
  (* get the first key, otherwise return an empty key *)
  let key = match keys with [] -> Sorted.columns data | _ -> List.hd keys in
  let columns, (insert_vals, update_vals, delete_vals) = Sorted.to_diff data ~key in
  let prepare_where row =
    List.zip_nofail key row
    |> List.map (fun (k,v) -> Lens_phrase.equal (Lens_phrase.var k) (Lens_phrase.Constant.of_value v))
    |> Lens_phrase.List.fold_and in
  let fmt_cmd_sep f () = Format.pp_print_string f ";\n" in
  let fmt_delete f row =
    let predicate = prepare_where row in
    let delete = { Lens_database.Delete. table; predicate; db; } in
    Lens_database.Delete.fmt f delete in
  let fmt_update f row =
    let predicate = prepare_where row in
    let set = List.combine columns row |> List.skip ~n:(List.length key) in
    let update = { Lens_database.Update. table; predicate; db; set; } in
    Lens_database.Update.fmt f update in
  let fmt_insert f values =
    let insert = { Lens_database.Insert. table; columns; values; db; } in
    Lens_database.Insert.fmt f insert in
  let fmt_insert_cmds = List.map (fun v -> fmt_insert, v) insert_vals in
  let fmt_delete_cmds = List.map (fun v -> fmt_delete, v) delete_vals in
  let fmt_update_cmds = List.map (fun v -> fmt_update, v) update_vals in
  let fmt_fmt f (fmt, v) = fmt f v in
  let fmt_all f () = Format.pp_print_list ~pp_sep:fmt_cmd_sep fmt_fmt f @@
    List.flatten [fmt_insert_cmds; fmt_delete_cmds; fmt_update_cmds] in
  let cmds = Format.asprintf "%a" fmt_all () in
  if String.equal "" cmds |> not then
    begin
      if show_query then print_endline cmds;
      exec cmds |> ignore
    end

let get_fds (fds : (string list * string list) list) (cols : Types.lens_col list) : Fun_dep.Set.t =
  let check_col xs = List.iter (fun alias -> if not (Column.List.mem_alias cols ~alias) then failwith ("The column " ^ alias ^ " does not exist.")) xs in
  List.iter (fun (left, right) -> check_col left; check_col right) fds;
  let fd_of (left, right) =
    Fun_dep.make (Alias.Set.of_list left) (Alias.Set.of_list right) in
  Fun_dep.Set.of_list (List.map fd_of fds)

let lens_put (lens : Value.t) (data : Value.t) =
  let rec do_step_rec lens delt =
    match lens with
    | `Lens (t,_) -> apply_delta t delt
    | _ -> lens_put_set_step lens delt do_step_rec in
  do_step_rec lens (lens_get_delta lens data)
