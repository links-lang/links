open Value
open Utility
open Lens_types


let calculate_fd_changelist fds data =
  (* get the key of the row for finding complements *)
  let rec loop fds =
    if Fun_dep.Set.is_empty fds then
      []
    else
      let fun_dep = Fun_dep.Set.root_fd fds |> OptionUtils.val_of in
      let cols, changeset = Lens_sorted_records.project_fun_dep data ~fun_dep in
      let changeset = Array.to_list changeset in
      (* remove duplicates and sort *)
      let changeset = List.sort_uniq (fun (a,_) (a',_) -> Lens_sorted_records.Simple_record.compare_val a a') changeset in
      let fds = Fun_dep.Set.remove fun_dep fds in
      (cols, changeset) :: loop fds in
  let res = loop fds in
  (* reverse the list, so that the FD roots appear first *)
  List.rev res

let matches_change changes =
  let is_changed ((cols_l, _cols_r),(vals)) =
    let vals_l = List.map (fun (left,_) -> left) vals in
    Lens_phrase.Option.in_expr cols_l vals_l in 
  List.map is_changed changes |> Lens_phrase.List.fold_or_opt


let relational_update fds changedata updatedata =
  let fds = fds in
  let changelist = calculate_fd_changelist fds changedata in
  let changes = List.map (fun ((cols_l,_cols_r),_l) ->
      (* get a map from simp rec to col value *)
      let col_maps = List.map (Lens_sorted_records.get_col_map updatedata) cols_l in
      let col_maps = List.flatten (List.map (fun mp -> match mp with None -> [] | Some a -> [a]) col_maps) in
      (* get a map from col name to change value *)
      let (val_maps,_) = List.fold_left (fun (maps,fn) _ ->
          (fun a -> List.hd (fn a)) :: maps, (fun a -> List.tl (fn a))) ([], fun a -> a) cols_l in
      let maps = List.combine col_maps val_maps in
      (* get a function which compares column with change *)
      let comp = List.fold_left (fun a (mp1, mp2) ->
          fun record change_key -> mp1 record = mp2 change_key && a record change_key) (fun _a _b -> true) maps in
      comp
    ) changelist in
  (* each entry in changelist is a functional dependency, and then the corresponding records *)
  (* generate a function for every change list entry, which can replace the columns in the
   * right side of the functional dependency of a record in res *)
  let apply_changes = List.map (fun ((_cols_l, cols_r),_l) ->
      (* upd cols returns a function which, given a record another record containing cols_r,
       * replaces every column value in the first record from that in the second record if it
       * matches *)
      let rec upd cols =
        match cols with
        | [] -> fun _r _target -> []
        | x :: xs ->
          let fn = upd xs in
          (* get a function which maps a row to the x's value *)
          let map = SortedRecords.get_col_map_list cols_r x in
          match map with
          | None -> (* the column does not have to be replaced *)
            fun yl target -> (List.hd yl) :: fn (List.tl yl) target
          | Some mp -> (* the column has been found, replace *)
            fun yl target -> mp target :: fn (List.tl yl) target
      in
      upd updatedata.columns
    ) changelist in
  let update arr = Array.map (fun r ->
      let r' = List.fold_left (fun r ((check, update),(_, changes)) ->
          let upd = List.find_opt (fun (left, _right) -> check r left) changes in
          match upd with
          | None -> r
          | Some (_left, right) -> update r right
        ) r (List.combine (List.combine changes apply_changes) changelist) in
      (r', r)
      (* print_endline (string_of_value (box_list r) ^ " to " ^ string_of_value (box_list r'));
         r' *)
    ) arr in
  let res2 = update updatedata.plus_rows in
  let res2 = List.flatten (List.map (fun (r, r') -> if r = r' then [] else [r, r']) (Array.to_list res2)) in
  let res = { SortedRecords.columns = updatedata.columns; neg_rows = Array.of_list (List.map (fun (_,b) -> b) res2); plus_rows = Array.of_list (List.map (fun (a,_) -> a) res2); } in
  SortedRecords.sort_uniq res


let get_changes (lens : Value.t) (data : SortedRecords.recs) =
  let sort = LensValue.sort lens in
  let fds = Sort.fds sort in
  let changelist = calculate_fd_changelist fds data in
  (* query relevant rows in database *)
  let phrase = matches_change changelist in
  let res = lens_get_select_opt lens phrase in
  let res = SortedRecords.construct_cols (LensValue.cols_present_aliases lens) res in
  (* perform relational update *)
  relational_update fds data res

let query_join_records (lens : Value.t) (set : SortedRecords.recs) (on : string list) =
  let proj = SortedRecords.project_onto set on in
  let recs = List.append (Array.to_list proj.plus_rows) (Array.to_list proj.neg_rows) in
  let recs = List.sort_uniq SortedRecords.compare recs in
  let query = Phrase.Option.in_expr on recs in
  let recs = lens_get_select_opt lens query in
  SortedRecords.construct_cols (LensValue.cols_present_aliases lens) recs

let query_project_records (lens : Value.t) (set : SortedRecords.recs) (key : string list) (drop : string list) =
  let proj = SortedRecords.project_onto set key in
  let recs = List.append (Array.to_list proj.plus_rows) (Array.to_list proj.neg_rows) in
  let recs = List.sort_uniq SortedRecords.compare recs in
  let query = Phrase.Option.in_expr key recs in
  let recs = lens_get_select_opt lens query in
  let recs = SortedRecords.construct_cols (LensValue.cols_present_aliases lens) recs in
  SortedRecords.project_onto recs (List.append key drop)

let lens_put_set_step (lens : Value.t) (delt : SortedRecords.recs) (fn : Value.t -> SortedRecords.recs -> unit) =
  match lens with
  | `Lens _ -> fn lens delt
  | `LensDrop (l, drop, key, default, _sort) ->
    let relevant = query_project_records l delt [key] [drop] in
    let colmap = SortedRecords.get_cols_map delt [key] in
    let relevant_value_map = OptionUtils.val_of (SortedRecords.get_col_map relevant drop) in
    let extend (row : SortedRecords.simp_rec) =
      let find = colmap row in
      let rel = SortedRecords.find_rec relevant.plus_rows find in
      let v = match rel with
        | None -> default
        | Some r -> relevant_value_map r in
      List.append row [v] in
    let plus = Array.map extend delt.plus_rows in
    let neg = Array.map extend delt.neg_rows in
    let delt = { SortedRecords.columns = List.append delt.columns [drop]; plus_rows = plus; neg_rows = neg } in
    fn l delt
  | `LensJoin (l1, l2, cols, pd, qd, _sort)  ->
    let cols_simp = List.map (fun (a,_,_) -> a) cols in
    let sort1 = LensValue.sort l1 in
    let proj1 = SortedRecords.project_onto delt (Sort.cols_present_aliases sort1) in
    let sort2 = LensValue.sort l2 in
    let proj2 = SortedRecords.project_onto delt (Sort.cols_present_aliases sort2) in
    let delta_m0 = get_changes l1 proj1 in
    let delta_n0 = get_changes l2 proj2 in
    let delta_l =
      SortedRecords.merge
        (SortedRecords.merge
           (SortedRecords.join delta_m0 delta_n0 cols_simp)
           (SortedRecords.merge
              (SortedRecords.join delta_m0 (query_join_records l2 delta_m0 cols_simp) cols_simp)
              (SortedRecords.join delta_n0 (query_join_records l1 delta_n0 cols_simp) cols_simp)
           )
        )
        (SortedRecords.negate delt) in
    let j = SortedRecords.project_onto (SortedRecords.merge (query_join_records lens delta_l cols_simp) (delt)) cols_simp in
    let delta_l_l = SortedRecords.join delta_l j cols_simp in
    let delta_l_a = SortedRecords.merge (delta_l) (SortedRecords.negate delta_l_l) in
    let delta_m = SortedRecords.merge
        (SortedRecords.merge delta_m0 (SortedRecords.negate (SortedRecords.project_onto_set (SortedRecords.filter delta_l_a pd) delta_m0)))
        (SortedRecords.negate (SortedRecords.project_onto_set delta_l_l delta_m0)) in
    let delta_n = SortedRecords.merge
        delta_n0
        (SortedRecords.negate (SortedRecords.project_onto_set (SortedRecords.filter delta_l_a qd) delta_n0)) in
    fn l1 delta_m;
    fn l2 delta_n

  | `LensSelect (l, pred, _sort) ->
    let delta_m1 = SortedRecords.merge (get_changes (lens_select l (Phrase.not' pred)) delt)
        { columns = delt.columns; plus_rows = Array.of_list []; neg_rows = delt.neg_rows } in
    (* Debug.print (SortedRecords.to_string_tabular delta_m1);  *)
    let m1_cap_P = SortedRecords.filter delta_m1 pred in
    (* Debug.print (SortedRecords.to_string_tabular m1_cap_P);  *)
    let delta_nhash = SortedRecords.merge (m1_cap_P) (SortedRecords.negate delt) in
    (* Debug.print (SortedRecords.to_string_tabular delta_nhash); *)
    let new_delta = SortedRecords.merge delta_m1 (SortedRecords.negate delta_nhash) in
    (* Debug.print (SortedRecords.to_string_tabular new_delta);  *)
    fn l new_delta
  | _ -> failwith "Unsupport lens."

let lens_get_delta (lens : Value.t) (data : Value.t) =
  let cols = LensValue.cols_present_aliases lens in
  let orig = SortedRecords.construct_cols cols (lens_get lens ()) in
  let data = SortedRecords.merge (SortedRecords.construct_cols cols data) (SortedRecords.negate orig) in
  data


let lens_put_step (lens : Value.t) (data : Value.t) (fn : Value.t -> SortedRecords.recs -> unit) =
  let data = lens_get_delta lens data in
  lens_put_set_step lens data fn

let db_string_of_value (db : Value.database) (v : Value.t) =
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

let apply_delta (t : Value.table) (data : SortedRecords.recs) =
  let show_query = false in
  let (db, _), table, keys, _ = t in
  let exec cmd =
    Debug.debug_time_out
      (fun () -> db#exec cmd)
      (fun time -> query_timer := !query_timer + time; query_count := !query_count + 1) in
  (* get the first key, otherwise return an empty key *)
  let key = match keys with [] -> data.columns | _ -> List.hd keys in
  let key_len = List.length key in
  let cols = data.columns in
  let cols = SortedRecords.reorder_cols cols key in
  let data = (SortedRecords.project_onto data cols) in
  let (insert_vals, update_vals) = List.partition (fun row ->
      let key_vals = take row key_len in
      let row = SortedRecords.find_mul data.neg_rows key_vals in
      match row with None -> true | Some _ -> false) (Array.to_list data.plus_rows) in
  let delete_vals = List.map (fun row ->
      let key_vals = take row key_len in
      let row = SortedRecords.find_mul data.plus_rows key_vals in
      match row with None -> [key_vals] | Some _ -> []) (Array.to_list data.neg_rows) in
  let delete_vals = List.flatten delete_vals in
  let delete_commands = List.map (fun del_key ->
      let cond (key, v) = (db#quote_field key) ^ " = " ^ db_string_of_value db v in
      let zipped = List.combine key del_key in
      let cond = List.fold_left (fun a b -> a ^ " AND " ^ cond b) (cond (List.hd zipped)) (List.tl zipped) in
      let cmd = "delete from " ^ db#quote_field table ^ " where " ^ cond in
      (* exec cmd; *)
      cmd) delete_vals in
  let update_commands = List.map (fun row ->
      let key_vals = take row key_len in
      let cond (key, v) = (db#quote_field key) ^ " = " ^ db_string_of_value db v in
      let zipped = List.combine key key_vals in
      let where = List.fold_left (fun a b -> a ^ " AND " ^ cond b) (cond (List.hd zipped)) (List.tl zipped) in
      let upd = skip (List.combine cols row) key_len in
      let upd = List.fold_left (fun a b -> a ^ ", " ^ cond b) (cond (List.hd upd)) (List.tl upd) in
      let cmd = "update " ^ db#quote_field table ^ " set " ^ upd ^ " where " ^ where in
      (* exec cmd; *)
      cmd
    ) update_vals in
  let insert_vals = List.map (fun row ->
      List.map (db_string_of_value db) row) insert_vals in
  let b = Buffer.create 255 in
  let app_cmd str = if Buffer.length b > 0 then Buffer.add_string b "; "; Buffer.add_string b str in
  List.iter app_cmd delete_commands;
  List.iter app_cmd update_commands;
  if insert_vals <> [] then
    begin
      let insert_cmd = db#make_insert_query (table, cols, insert_vals) in
      app_cmd insert_cmd
    end;
  if Buffer.length b > 0 then
    begin
      let cmd = Buffer.contents b in
      if show_query then print_endline cmd;
      let _ = exec (Buffer.contents b) in ()
    end;
  ()

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
