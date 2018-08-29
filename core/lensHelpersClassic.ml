open Value
open Utility
open LensUtility
open LensHelpers
open LensHelpersIncremental
open LensSetOperations
open LensRecordHelpers
open LensSetOperations.SortedRecords



let relational_update (fds : fundepset) (changedata : SortedRecords.recs) (updatedata : SortedRecords.recs) =
    let fds = fds in
    let changelist = calculate_fd_changelist fds changedata in
    let changes = List.map (fun ((cols_l,_cols_r),_l) -> 
        (* get a map from simp rec to col value *)
        let col_maps = List.map (SortedRecords.get_col_map updatedata) cols_l in
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
                        fun yl target -> 
                            mp target :: fn (List.tl yl) target
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
    let res = { SortedRecords.columns = updatedata.columns; neg_rows = Array.of_list []; plus_rows = Array.map (fun (a,_) -> a) res2; } in
    SortedRecords.sort_uniq res

let lens_put_set_step (lens : Value.t) (data : SortedRecords.recs) (fn : Value.t -> SortedRecords.recs -> unit) =
    let get l = 
        let dat = LensHelpers.lens_get l () in
        SortedRecords.construct_cols (Lens.cols_present_aliases l) dat in
    match lens with
    | `Lens _ -> fn lens data
    | `LensDrop (l, drop, key, default, _sort) -> 
            let r = get l in
            let cols = Lens.cols_present_aliases lens in
            let nplus = SortedRecords.minus data (SortedRecords.project_onto r cols) in
            let a = SortedRecords.construct (box_list [box_record [drop, default]]) in
            let m = SortedRecords.merge (SortedRecords.join r data cols) (SortedRecords.join nplus a []) in
            let res = relational_update (FunDepSet.of_lists [[key], [drop]]) r m in
            fn l res 
    | `LensJoin (l1, l2, join, pd, qd, _sort)  -> 
            let join_simp = List.map (fun (a,_,_) -> a) join in
            let getfds l = Lens.fundeps l in
            let cols l = Lens.cols_present_aliases l in
            let r = get l1 in
            let s = get l2 in
            let m0 = relational_update (getfds l1) (SortedRecords.project_onto data (cols l1)) r in
            let n0 = relational_update (getfds l2) (SortedRecords.project_onto data (cols l2)) s in
            let l = SortedRecords.minus (SortedRecords.join m0 n0 join_simp) data in
            let ll = SortedRecords.join l data (cols lens) in
            let la = SortedRecords.minus l ll in 
            let m = SortedRecords.minus (
                    SortedRecords.minus m0 (SortedRecords.project_onto (SortedRecords.filter la pd) (cols l1))
                ) (SortedRecords.project_onto ll (cols l1)) in
            let n = SortedRecords.minus n0 (SortedRecords.project_onto (SortedRecords.filter la qd) (cols l2)) in
            fn l1 m;
            fn l2 n
    | `LensSelect (l, pred, _sort) ->
            let sort = Lens.sort l in
            let r = get l in
            let m1 = relational_update (LensSort.fundeps sort) data (SortedRecords.filter r pred) in
            let nh = SortedRecords.minus (SortedRecords.filter m1 pred) data in
            let r = SortedRecords.minus m1 nh in 
            fn l r
    | _ -> ()

let apply_table_data (t : Value.table) (data : SortedRecords.recs) =
    let show_query = false in
    let (db, _), table, _keys, _ = t in
    let exec cmd = 
            Debug.debug_time_out 
            (fun () -> db#exec cmd) 
            (fun time -> query_timer := !query_timer + time; query_count := !query_count + 1) in
    let cmd = "delete from " ^ db#quote_field table ^ " where TRUE" in
    let _ = exec cmd in
    if show_query then print_endline cmd else ();
    let cols = data.columns in
    let insert_vals = List.map (fun row -> 
        List.map (db_string_of_value db) row) (Array.to_list data.plus_rows) in
    if insert_vals <> [] then
        begin
            let insert_cmd = db#make_insert_query (table, cols, insert_vals) in
            if show_query then print_endline insert_cmd else (); 
            let _ = exec insert_cmd in
            ()
        end;
    ()

let lens_put_step (lens : Value.t) (data : Value.t) (fn : Value.t -> SortedRecords.recs -> unit) =
    let data = SortedRecords.construct_cols (Lens.cols_present_aliases lens) data in
    lens_put_set_step lens data fn

let lens_put (lens : Value.t) (data : Value.t) =
    let rec do_step_rec lens data =
        match lens with
        | `Lens (t,_) -> apply_table_data t data 
        | _ -> lens_put_set_step lens data do_step_rec in
    do_step_rec lens (SortedRecords.construct_cols (Lens.cols_present_aliases lens) data)


