open Types
open Utility
open LensUtility
open Value
open LensFDHelpers
open LensQueryHelpers
open LensRecordHelpers

let query_timer = ref 0
let query_count = ref 0

(** print a debug message if debugging is enabled *)
let print message =
  (if false then print_endline message)

let ensure_lenses_enabled () = 
  if Settings.get_value Basicsettings.RelationalLenses.relational_lenses then
    ()
  else
    failwith "Code uses relational lenses, but relational lenses are not enabled. Please set the relational lenses flag."

module Lens = struct
    type t = Value.t

    let sort (v : t) = 
        match v with
        | `Lens (_table, sort) -> sort
        | `LensMem (_data, sort) -> sort
        | `LensDrop (_lens, _drop, _key, _def, sort) -> sort
        | `LensSelect (_lens, _pred, sort) -> sort
        | `LensJoin (_lens1, _lens2, _on, _left, _right, sort) -> sort
        | _e -> failwith "Did not match a lens value (Lens.sort)"
   
    let rec is_memory_lens (lens : Value.t) =
        match lens with
        | `Lens _ -> false
        | `LensMem _ -> true
        | `LensDrop (lens, _drop, _key, _def, _rtype) -> is_memory_lens lens
        | `LensSelect (lens, _pred, _sort) -> is_memory_lens lens
        | `LensJoin (lens1, lens2, _on, _, _, _sort) -> is_memory_lens lens1 || is_memory_lens lens2
        | _ -> failwith ("Unknown lens (is_memory_lens) :" ^ (string_of_value lens))

    let cols (lens : t) =
        let sort = sort lens in
        let cols = LensSort.cols sort in
        cols

    let cols_present_aliases (lens : t) =
        let cols = cols lens in
        LensColList.present_aliases cols

    let colset (lens : t) =
        let sort = sort lens in
        LensSort.colset sort

    let fundeps (lens : t) =
        let sort = sort lens in
        LensSort.fundeps sort

    let predicate (lens : t) =
        let sort = sort lens in
        LensSort.predicate sort
end

module LensType = struct
    type t = Types.typ

    let sort (v : t) = 
        match v with 
        | `Lens (sort) -> sort
        | _e -> failwith "Type is not a lens (LensType.sort)."
end


(* get / put operations *)

let rec get_primary_key (lens : Value.t) =
    match lens with 
    | `Lens (_a, sort) -> 
        let fds = LensSort.fundeps sort in
        let fd = FunDepSet.min_elt fds in
        let left = FunDep.left fd in
        left
    | `LensMem (_a, sort) ->  
        let fds = LensSort.fundeps sort in
        let fd = FunDepSet.min_elt fds in
        let left = FunDep.left fd in
        left
    | `LensDrop (lens, _, _, _, _) -> get_primary_key lens
    | `LensSelect (lens, _, _) -> get_primary_key lens
    | `LensJoin (lens1, _, _, _, _, _) -> (* right table has to be defined by left table *) get_primary_key lens1 
    | _ -> failwith ("Unknown lens (get_primary_key) : " ^ (string_of_value lens))

let rec lens_get_mem (lens : Value.t) callfn =
    match lens with
    | `Lens _ -> failwith "Non memory lenses not implemented."
    | `LensMem (table, _rtype) -> table
    | `LensDrop (lens, drop, _key, _def, _rtype) ->
        let records = lens_get_mem lens callfn in
        let result = List.map (fun a -> drop_record_column drop a) (unbox_list records) in
          `List result
    | `LensSelect (lens, pred, _sort) ->
        let records = lens_get_mem lens callfn in
        let res = List.filter (fun x -> unbox_bool (calculate_predicate_rec pred x)) (unbox_list records) in 
           box_list res
    | `LensJoin (lens1, lens2, on, _left, _right, _sort) ->
        let records1 = lens_get_mem lens1 callfn in
        let records2 = lens_get_mem lens2 callfn in
        let on = List.map (fun (a, _, _) -> a) on in
        let output = List.map (fun r1 -> 
            let rows = List.filter (fun r2 -> is_row_cols_record_match r1 r2 (ColSet.of_list on)) (unbox_list records2) in
            List.map (fun r2 ->  join_records r1 r2 on) rows
        ) (unbox_list records1) in
        box_list (List.concat output)
    | _ -> failwith "Not a lens."

let rec lens_get_db (lens : Value.t) =
    match lens with 
    | `Lens (((db, _), _, _, _), _) -> db
    | `LensDrop (l, _, _, _, _) -> lens_get_db l
    | `LensSelect (lens, _, _) -> lens_get_db lens
    | `LensJoin (l1, _, _, _, _, _) -> lens_get_db l1
    | _ -> failwith "Unsupported lens for get db."

let rec lens_get_query (lens : Value.t) =
  match lens with
  | `Lens (((db, _), table, _, _), sort) -> 
        let cols = LensSort.cols sort in
        {
            tables = [table, table];
            cols = cols;
            pred = None;
            db = db;
        }
  | `LensSelect (lens, pred, _sort) ->
        let query = lens_get_query lens in
        { query with pred = Some pred }
        (* get_lens_sort_row_type sort *)
  | `LensJoin (lens1, lens2, _on, _left, _right, sort) ->
        let q1 = lens_get_query lens1 in
        let q2 = lens_get_query lens2 in
        (* all table names must be unique, rename them *)
        let _tables2 = List.map (fun (n2, al2) -> 
            try 
                let _tbl = List.find (fun (n1,_al1) -> n1 = n2) q1.tables in
                failwith "Cannot reuse a table twice in a join query!"
            with
                NotFound _ -> (n2, al2)
        ) q2.tables in
        let tables = List.append q1.tables q2.tables in
        let cols = LensSort.cols sort in
        if (q1.db <> q2.db) then
            failwith "Only single database expressions supported."
        else
            {tables = tables; cols = cols; pred = get_lens_sort_pred sort; db = q1.db}
  | _ -> failwith "Unsupported lens for query"

(* BUG: Lists can be too big for List.map; need to be careful about recursion *)
let lens_get (lens : Value.t) callfn =
    if Lens.is_memory_lens lens then
        lens_get_mem lens callfn 
    else
        let _ = Debug.print "getting tables" in
        let sort = Lens.sort lens in
        let db = lens_get_db lens in
        let cols = LensSort.cols sort in
        (* print_endline (ListUtils.print_list (get_lens_sort_cols_list sort)); *)
        let sql = construct_select_query_sort db sort in
        (* let _ = print_endline sql in *)
        (* let query = lens_get_query lens in
        let sql = construct_select_query query  in *)
        let mappings = List.map (fun c -> get_lens_col_alias c, get_lens_col_type c) cols in
        let _ = Debug.print sql in
        let res = Debug.debug_time_out 
            (fun () -> execute_select mappings sql db) 
            (fun time -> query_timer := !query_timer + time; query_count := !query_count + 1) in
        res

let lens_debug_delta (delta : (Value.t * int) list) = 
    List.map (fun (t,m) -> print (string_of_int m ^ ": " ^ string_of_value t)) delta

let project_lens (l : Value.t) (t : (Value.t * int) list) = 
    let cols = Lens.colset l in
    List.map (fun (row,t) -> project_record_columns cols row,t) t

let lens_select (lens : Value.t) (phrase : Types.lens_phrase) =
    let sort = Lens.sort lens in
    let sort = LensTypes.select_lens_sort sort phrase in
    `LensSelect (lens, phrase, sort) 

let lens_get_select (lens : Value.t) (phrase : Types.lens_phrase) =
    lens_get (lens_select lens phrase) None

let lens_get_select_opt (lens : Value.t) (phrase : Types.lens_phrase option) =
    match phrase with
    | None -> lens_get lens ()
    | Some phrase  -> lens_get_select lens phrase

let calculate_fd_changelist (fds : FunDepSet.t) (data : (Value.t * int) list) =
    let additions = List.filter (fun (_t,m) -> m = +1) data in
    let additions = List.map (fun (t,_m) -> t) additions in
    (* get the key of the row for finding complements *)
    let rec loop fds =
        begin 
            match FunDepSet.root_fd fds with
            | None -> []
            | Some fd -> 
                let fdl, fdr = FunDep.left fd, FunDep.right fd in
                let changeset = List.map (fun t ->
                    (Record.project t fdl, Record.project t fdr)) additions in
                let fds = FunDepSet.remove fd fds in
                (fd, changeset) :: loop fds
        end in
    let res = loop fds in
    res

let query_exists (lens : Value.t) phrase =
    let sort = Lens.sort lens in
    let sort = LensTypes.select_lens_sort sort phrase in
    if Lens.is_memory_lens lens then
        let res = lens_get (`LensSelect (lens, phrase, sort)) None in
        unbox_list res <> []
    else
        let db = lens_get_db lens in
        let sql = construct_select_query_sort db sort in
        let sql = "SELECT EXISTS(" ^ sql ^ ") AS t" in
        let mappings = ["t", `Primitive `Bool] in
        let res = Debug.debug_time_out 
            (fun () -> execute_select mappings sql db)
            (fun time -> query_timer := !query_timer + time; query_count := !query_count + 1) in
        let _ = Debug.print sql in
        let (_,v) = unbox_record (List.hd (unbox_list res)) |> List.hd in
        unbox_bool v

let join_lens_should_swap (sort1 : Types.lens_sort) (sort2 : Types.lens_sort) (on_columns : string list) =
    let fds1 = LensSort.fundeps sort1 in
    let fds2 = LensSort.fundeps sort2 in
    let on_cols = ColSet.of_list on_columns in
    let covers fds sort =
        let fdcl = FunDepSet.transitive_closure on_cols fds in
        let other = LensSort.colset sort in
        (* print_endline (ColSet.Show_t.show fdcl ^ " = " ^ ColSet.Show_t.show (other)); *)
        ColSet.equal other fdcl in
    if covers fds2 sort2 then
        false
    else if covers fds1 sort1 then
        true
    else
        failwith "One of the tables needs to be defined by the join column set."

let join_lens_sort (sort1 : Types.lens_sort) (sort2 : Types.lens_sort) (on_columns : string list) = 
    (* helper function to find new alias, e.g. for 'name' it will find 'name_1', 'name_2' etc. *)
    let rec get_new_alias alias columns num = 
        let nal = alias ^ "_" ^ string_of_int num in
        if LensColList.mem_alias nal columns then 
            get_new_alias alias columns (num + 1)
        else 
            nal in
    (* verify both sorts have all columns in on_columns and that the types match *)
    let on_match = List.for_all (fun onc -> 
        let c1 = LensSort.find_col_alias onc sort1 in
        let c2 = LensSort.find_col_alias onc sort2 in
        match c1, c2 with
        | Some c1, Some c2 -> LensCol.typ c1 = LensCol.typ c2
        | _ -> false) on_columns in
    if not on_match then
        failwith "The key does not match between the two lenses.";
    (* join the two column lists while renaming columns and keeping track of renames *)
    let union, join_renames = List.fold_left (fun (output, jrs) c -> 
        (* see if column c's alias already exists *)
        if LensColList.mem_alias (LensCol.alias c) output |> not then
            (* if not, just add the column *)
            c :: output, jrs
        else
            (* is the column a join column *)
            let new_alias = get_new_alias c.alias output 1 in
            if List.mem (LensCol.alias c) on_columns then
                (* then renamed column and hide it *)
                (c |> LensCol.rename new_alias |> LensCol.hide) :: output, (c.alias, new_alias) :: jrs
            else 
                (* otherwise just rename the column *)
                (c |> LensCol.rename new_alias) :: output, jrs
    ) (LensSort.cols sort1, []) (LensSort.cols sort2) in
    (* combine the predicates *)
    let pred = match LensSort.predicate sort1, LensSort.predicate sort2 with
    | None, None -> None
    | Some p1, None -> Some p1
    | None, Some p2 -> Some (Phrase.rename_var p2 join_renames)
    | Some p1, Some p2 -> Some (create_phrase_and (create_phrase_tuple p1) (create_phrase_tuple (Phrase.rename_var p2 join_renames))) in
    let join_pred = List.fold_left (fun pred (alias, newalias) -> 
        let jn = create_phrase_equal (create_phrase_var alias) (create_phrase_var newalias) in
        match pred with Some p -> Some (create_phrase_and p jn) | None -> Some jn
    ) pred join_renames in
    let fn_deps = FunDepSet.union (LensSort.fundeps sort1) (LensSort.fundeps sort2) in
    (* determine the on column renames as a tuple (join, left, right) *)
    let jrs = List.map (fun on -> 
        let left = on in
        let (_, right) = List.find (fun (a,_) -> a = on) join_renames in
        on, left, right) on_columns in
    (fn_deps, join_pred, union), jrs 


