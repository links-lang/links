open Types
open Utility
open LensRecordHelpers
open Value


module FunDep = struct 
    include Types.FunDep

    let left (l,_) = l
    let right (_,r) = r 
    
    let compare (l1,r1) (l2, r2) = 
        let res = ColSet.compare l1 l2 in
        if res = 0 then
            ColSet.compare r1 r2
        else
            res

    let of_lists (left, right : string list * string list) : t =
        let left = ColSet.of_list left in
        let right = ColSet.of_list right in
        (left, right)

    let make left right : t = (left, right)

    let key_fd (keys : string list) (cols : string list) =
        let keys = ColSet.of_list keys in
        let cols = ColSet.of_list cols in
        let right = ColSet.diff cols keys in
        make keys right

end

module FunDepSet = struct
    include FunDepSet

    let of_lists (fds : (string list * string list) list) : t =
        let fds = List.map FunDep.of_lists fds in
        of_list fds

    let remove_defines (fds : t) (cols : Types.colset) =
        let remove fd = 
            if ColSet.inter cols (FunDep.left fd) |> ColSet.is_empty |> not then
                failwith "Cannot remove a column defining other columns.";
            let newRight = ColSet.diff (FunDep.right fd) cols in
            FunDep.make (FunDep.left fd) newRight in
        let fds = map remove fds in
        let keep fd = not (ColSet.is_empty (FunDep.right fd)) in
        let fds = filter keep fds in
        fds

    let key_fds left right =
        FunDepSet.of_list [FunDep.key_fd left right]

    (* Find the a functional dependency at the root, which is the functional dependency that defines all other nodes *)
    let root_fd (fds : t) = 
        let res = filter (fun fd ->
            not (exists (fun fd2 ->
                ColSet.subset (FunDep.right fd2) (FunDep.left fd)) fds
            )
        ) fds in
        if is_empty res then
            None
        else
            Some(min_elt res)

    (* Get the functional dependency which defines the columns cols *)
    let defining_fd (cols : colset) (fds : t) =
        let res = filter (fun fd ->
            ColSet.subset cols (FunDep.right fd)    
        ) fds in
        min_elt res

    let transitive_closure (cols : colset) (fds : fundepset) =
        let rec get = fun attrs fds ->
            let (newAttrs, fds) = FunDepSet.partition (fun fd -> ColSet.subset (FunDep.left fd) attrs) fds in
            let newAttrs = FunDepSet.fold (fun fd c -> ColSet.union c (FunDep.right fd)) newAttrs attrs in
            if ColSet.cardinal newAttrs > ColSet.cardinal attrs then
                get newAttrs fds
            else
                attrs in
        get cols fds

end

module FunDepTree = struct  
    type t = [ `FDNode of colset * (t list) ]
    
    let cols (n : t) = 
        match n with
        | `FDNode (cols, _) -> cols

    let subnodes (n : t) =
        match n with
        | `FDNode (_, subnodes) -> subnodes 

    let rec pp_pretty (fmt : Format.formatter) (v : t) = 
        Format.pp_force_newline fmt ();
        Format.pp_open_box fmt 2;
        Format.pp_print_string fmt "-";
        Format.pp_print_space fmt ();
        ColSet.pp_pretty fmt (cols v);
        List.iter (fun node -> pp_pretty fmt node) (subnodes v);
        Format.pp_close_box fmt ()

    let rec fd_subnodes (fds : FunDepSet.t) (fd : FunDep.t) : t = 
        let subfds = FunDepSet.filter (fun fd2 ->
            ColSet.subset (FunDep.right fd) (FunDep.left fd2)
        ) fds in
        let remaining = ColSet.filter (fun col ->
            not (FunDepSet.exists (fun fd2 -> ColSet.mem col (FunDep.left fd2)) subfds)) (FunDep.right fd) in
        let subfds = List.map (fd_subnodes fds) (FunDepSet.elements subfds) in
        let subfds = if ColSet.is_empty remaining then subfds else `FDNode (remaining, []) :: subfds in
            `FDNode (FunDep.left fd, subfds)

    let of_fds (fds : Types.fundepset) = 
        let root = FunDepSet.root_fd fds in
        OptionUtils.opt_map (fd_subnodes fds) root

end

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

let apply_fd_update (m : Value.t) (n : Value.t) (fd : Types.fundep) : Value.t =
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

let is_fd_record_match (m : Value.t) (n : Value.t) (fd : Types.fundep) : bool =
    (* checks wether two records m and n match w.r.t. the functional dependency fd *)
    is_row_cols_record_match m n (FunDep.right fd)

let apply_fd_record_row_revision (m : Value.t) (n : Value.t) (fd : Types.fundep) : bool * Value.t =
   (* first check if the two records match w.r.t. the given functional dependency fd and if so apply the updates
      from record n to record m, otherwise return m unchanged *)
    if is_fd_record_match m n fd then
        (* if so apply fd update *)
        true, apply_fd_update m n fd
    else
        (* otherwise return record unchanged *)
        false, m

let apply_fd_record_revision (m : Value.t) (n : Value.t) (fds : Types.fundepset) : bool * Value.t =
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

let apply_fd_merge_record_revision (n : Value.t) (m : Value.t) (fds : Types.fundepset) = 
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

