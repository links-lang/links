open Utility
open LensUtility
open LensRecordHelpers
open Value



let records_match_on recA recB on =
    ColSet.for_all (fun col ->
        get_record_val col recA = get_record_val col recB) on

let find_update_record (fd : fundep) (t, m : Value.t * int) (recs : (Value.t * int) list) =
    if m = 0 then
        None
    else
        let sameleft = fun t2 -> records_match_on t t2 (FunDep.left fd) in
        let diffright = fun t2 -> not (records_match_on t t2 (FunDep.right fd)) in
        try
            let compl = List.find (fun (t', m') -> m = - m' && sameleft t' && diffright t') recs in
            Some compl
        with
            NotFound _ -> None

let is_update_record (fds : fundepset) (r : Value.t * int) (recs : (Value.t * int) list) =
    FunDepSet.exists (fun fd -> OptionUtils.is_some (find_update_record fd r recs)) fds

let lens_join_split_updates (fds_left : fundepset) (fds_right : fundepset) (data : (Value.t * int) list) =
    let upd_left, n_updleft = List.partition (fun l -> is_update_record fds_left l data) data in
    let upd_right, n_update = List.partition (fun l -> is_update_record fds_right l data) n_updleft in
    let upd_left_right, upd_left = List.partition (fun l -> is_update_record fds_right l data) upd_left in
    upd_left_right, upd_left, upd_right, n_update


(* record revision *)

let apply_fd_update (m : Value.t) (n : Value.t) (fd : fundep) : Value.t =
    (* update all columns from the right side of the functional dependency fd
       in m with the value from n  *)
    (* assume we know that n and m have the same values for columns in left(fd) *)
    let n_cols = unbox_record n in
    let m_cols = List.map (fun (k, v) ->
            if ColSet.exists (fun a -> a = k) (FunDep.right fd) then
                let _, n_v = List.find (fun (n_k, _) -> n_k = k) n_cols in
                k, n_v
            else
                k, v
        ) (unbox_record m) in
        box_record m_cols

let is_row_cols_record_match (m : Value.t) (n : Value.t) (cols : colset) : bool =
    (* determines wether the records m and n have the same values for the columns in cols *)
    (* check if all columns in left(fd) match *)
    let is_match =
        ColSet.for_all (fun col ->
            try
                let n_v = get_record_val col m in
                let m_v = get_record_val col n in
                    n_v = m_v
            with NotFound _ -> false
        ) cols in
    is_match

let is_fd_record_match (m : Value.t) (n : Value.t) (fd : fundep) : bool =
    (* checks wether two records m and n match w.r.t. the functional dependency fd *)
    is_row_cols_record_match m n (FunDep.right fd)

let apply_fd_record_row_revision (m : Value.t) (n : Value.t) (fd : fundep) : bool * Value.t =
   (* first check if the two records match w.r.t. the given functional dependency fd and if so apply the updates
      from record n to record m, otherwise return m unchanged *)
    if is_fd_record_match m n fd then
        (* if so apply fd update *)
        true, apply_fd_update m n fd
    else
        (* otherwise return record unchanged *)
        false, m

let apply_fd_record_revision (m : Value.t) (n : Value.t) (fds : fundepset) : bool * Value.t =
    (* m of `Record and n of `List `Record *)
    List.fold_right (fun nrow (upd, mrow) ->
            FunDepSet.fold (fun fd (upd, mrow) ->
                let upd_t, mrow = apply_fd_record_row_revision mrow nrow fd in
                upd_t || upd, mrow
            ) fds (upd, mrow)
    ) (unbox_list n) (false, m)

let mark_found_records (n : Value.t) (data : (Value.t * bool) array) : unit =
    Array.iteri (fun i (row, marked) ->
        if not marked && (records_equal row n) then
            Array.set data i (row, true)
    ) data

let apply_fd_merge_record_revision (n : Value.t) (m : Value.t) (fds : fundepset) =
    (* `List `Record * `List `Record * fds *)
    let arrM = Array.of_list (List.map (fun r -> r,false) (unbox_list m)) in
    let output = ref [] in
    let _ = List.map (fun r ->
        let _upd, r = apply_fd_record_revision r m fds in
            mark_found_records r arrM;
            output := r :: !output
    ) (unbox_list n) in
    let filteredData = List.filter (fun (_r,m) -> not m) (Array.to_list arrM) in
    let output = List.append !output (List.map (fun (r,_m) -> r) filteredData) in
        (box_list output)

