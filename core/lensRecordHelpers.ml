open Types
open Utility
open Value


let get_lens_sort_pred (_, pred, _ : Types.lens_sort) = pred


module LensCol = struct 
    type t = Types.lens_col

    let alias (col : t) = col.alias
    let exists (cols : t list) (colalias : string) = List.exists (fun c -> alias c = colalias) cols
    let present (col : t) = col.present
    let typ (col : t) = col.typ

    let hide (cs : t) = 
        { cs with present = false }

    let rename (name : string) (cs : t) =
        { cs with alias = name }
end

module LensColList = struct
    type t = LensCol.t list 

    let present (cs : t) =
        List.filter LensCol.present cs

    let aliases (cs : t) = 
        List.map LensCol.alias cs

    let present_aliases (cs : t) =
        let pr = present cs in
        aliases pr

    let mem_alias (alias : string) (cs : t) = 
        List.exists (fun c -> LensCol.alias c = alias) cs

    let find_alias (alias : string) (cs : t) =
        List.find_opt (fun c -> LensCol.alias c = alias) cs
end

module LensSort = struct
    type t = Types.lens_sort

    let fundeps (fds, _, _ : t) = fds
    let predicate = get_lens_sort_pred
    let cols (_fds, _pred, rowType : t) = rowType

    let cols_present_aliases (sort : t) = 
        let cols = cols sort in
        LensColList.present_aliases cols 

    let colset (sort:t) = 
        let columns = cols sort in
        let columns = LensColList.present_aliases columns in 
        ColSet.of_list columns

    let make fds pred rowType : Types.lens_sort = (fds, pred, rowType)

    let find_col_alias (alias : string) (sort : t) =
        LensColList.find_alias alias (cols sort)
end


let get_record_type_from_cols rowType = 
    let rowType = List.filter (fun f -> f.present) rowType in
    let map : field_spec_map = List.fold_left (fun a col -> StringMap.add col.alias (`Present col.typ) a) StringMap.empty rowType in
    `Record (map, Unionfind.fresh `Closed, false)

let get_lens_sort_row_type (_, _, rowType : Types.lens_sort) = 
   let map = get_record_type_from_cols rowType in
   map

let set_lens_sort_table_name (fn_dep, pred, rowType : Types.lens_sort) (table : string) =
    let rowType = List.map (fun c -> {c with table = table;}) rowType in
    (fn_dep, pred, rowType)

let get_lens_col_by_alias (cols : lens_col list) alias =
    try 
        Some (List.find (fun col -> col.alias = alias) cols) 
    with 
        NotFound _ -> None

let get_lens_sort_col_by_alias sort alias = 
    let cols = LensSort.cols sort in
    get_lens_col_by_alias cols alias

let get_lens_col_type (col : Types.lens_col) = col.typ

let get_lens_col_alias (col : Types.lens_col) = col.alias

let set_lens_col_alias (col : Types.lens_col) (new_alias : string) = { col with alias = new_alias; }

let match_lens_col_alias (c1 : lens_col) (c2 : lens_col) = c1.alias = c2.alias


let remove_list_values (l : string list) (remove : string list) = 
    List.filter (fun x -> not (List.mem x remove)) l

let update_rowtype_cols (cols : Types.field_spec_map) (rowType : Types.typ) =
    match rowType with 
    | `Record (_, row_var, dual) -> `Record (cols, row_var, dual)
    | _e -> failwith "Expected a record."

let try_find p l = try Some (List.find p l) with Not_found -> None | NotFound _ -> None

let get_record_val (key : string) (r : Value.t) = 
    let columns = unbox_record r in
    let (_, value) = List.find (fun (name, _value) -> name = key) columns in
    value
      
let get_field_spec_type (typ : Types.field_spec) = 
    match typ with
    | `Present t -> t
    | _ -> failwith "Expected `Present"

let records_equal recA recB =
    (* this function checks that every entry in recA is equal in recB *)
    not (List.exists (fun (name, value) -> get_record_val name recB <> value) (unbox_record recA))

let records_match_on recA recB on =
    List.for_all (fun col ->
        get_record_val col recA = get_record_val col recB) on

let contains_record (recA : Value.t) (recordsB : Value.t) =
    let recordsB = unbox_list recordsB in
    List.exists (fun recB -> records_equal recA recB) recordsB

let build_col_map (record : Value.t) =
    let r = unbox_record record in
    List.map (fun (n,_a) -> n) r

let reorder_record_cols cols r =
    let vals = unbox_record r in
    let vals = List.mapi (fun i n ->
        let (n',v') = List.nth vals i in
        if n' = n then
            (n, v')
        else
            (n, get_record_val n r)
    ) cols in
    box_record vals

let reorder_record_list_cols (recs : Value.t) =
    let recs = unbox_list recs in
    match recs with
    | [] -> box_list []
    | t::xs -> 
        let cols = build_col_map t in
        let xs = List.map (reorder_record_cols cols) xs in
        box_list (t::xs)

let reorder_delta_list_cols (recs : (Value.t * int) list) = 
    match recs with 
    | [] -> []
    | (t,m)::xs -> 
        let cols = build_col_map t in
        let xs = List.map (fun (t,m) -> (reorder_record_cols cols t, m)) xs in
        (t,m)::xs

let reorder_delta_list_cols_sort (sort : lens_sort) (recs : (Value.t * int) list) = 
    let cols = List.filter (fun c -> c.present) (LensSort.cols sort) in
    let cols = List.map (fun c -> c.alias) cols in
    List.map (fun (t,m) -> (reorder_record_cols cols t, m)) recs

let compare_records (recA : Value.t) (recB : Value.t) =
    let recA = unbox_record recA in
    let recB = unbox_record recB in
    let rec cmp recA recB = match recA, recB with
    | [], [] -> 0
    | [], _ -> failwith "records not matching length"
    | _, [] -> failwith "records not matching length"
    | (n, x) :: xs, (n', y) :: ys -> if n <> n' then 
            failwith "records not matching col order"
        else
            match compare x y with 0 -> cmp xs ys | n -> n in
    cmp recA recB

let compare_delta_entry (t,m) (t',m') =
    match compare_records t t' with
    | 0 -> compare m m'
    | a -> a

(* Drop related methods *)
let remove_record_type_column (a : string) (r : Types.lens_col list) =
    let fields = List.filter (fun col -> get_lens_col_alias col <> a) r in
    fields

let project_record_columns (a : colset) (record : Value.t) =
    let columns = List.filter (fun (name, _) -> ColSet.mem name a) (unbox_record record) in
    box_record columns

let drop_record_columns (a : string list) (record : Value.t) =
    let columns = List.filter (fun (name, _) -> not (List.mem name a)) (unbox_record record) in
    box_record columns

let drop_records_columns (a : string list) (records : Value.t) =
    let records = unbox_list records in
    let records = List.map (drop_record_columns a) records in
    box_list records

let drop_record_column (a : string) (record : Value.t) =
    drop_record_columns [a] record

let join_records (m : Value.t) (n : Value.t) (on : string list) = 
    let n = drop_record_columns on n in
    let out = List.append (unbox_record m) (unbox_record n) in
    box_record out

let restore_column (drop : string) (key : string) (default : Value.t) (row : Value.t) (records : Value.t) =
    let unb_records = unbox_list records in
    let record = try_find (fun x -> records_equal (drop_record_column drop x)  row) unb_records in
    match record with
    | Some r -> r
    | None -> 
        let keyVal = get_record_val key row in 
        let record = try_find (fun x -> get_record_val key x = keyVal) unb_records in
        let dropVal = match record with
        | Some r -> get_record_val drop r
        | None -> default in
        box_record ((drop, dropVal) :: unbox_record row) 

module Record = struct
    let equal = records_equal
    let column = get_record_val
    let match_on = records_match_on
    let project record cols = 
        let d = unbox_record record in
        let d = List.filter (fun (k,_v) -> ColSet.mem k cols) d in
        box_record d

    let set_column (record : Value.t) (k : string) (v : Value.t) = 
        let d = unbox_record record in
        let d = List.map (fun (k',v') ->  if k' = k then k',v' else k',v) d in
        box_record d

end


