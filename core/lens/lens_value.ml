open Value
open Utility

module Sort = Lens_sort
module Column = Lens_column
module Phrase = Lens_phrase
module Alias = Lens_alias
module Fun_dep = Lens_fun_dep

type t = Value.t

let sort v =
  match v with
  | `Lens (_table, sort) -> sort
  | `LensMem (_data, sort) -> sort
  | `LensDrop (_lens, _drop, _key, _def, sort) -> sort
  | `LensSelect (_lens, _pred, sort) -> sort
  | `LensJoin (_lens1, _lens2, _on, _left, _right, sort) -> sort
  | _e -> failwith "Did not match a lens value (LensValue.sort)"

let rec is_memory_lens lens =
  match lens with
  | `Lens _ -> false
  | `LensMem _ -> true
  | `LensDrop (lens, _drop, _key, _def, _rtype) -> is_memory_lens lens
  | `LensSelect (lens, _pred, _sort) -> is_memory_lens lens
  | `LensJoin (lens1, lens2, _on, _, _, _sort) -> is_memory_lens lens1 || is_memory_lens lens2
  | _ -> failwith ("Unknown lens (is_memory_lens) :" ^ (string_of_value lens))

let columns lens =
  let sort = sort lens in
  let cols = Lens_sort.cols sort in
  cols

let cols_present_aliases (lens : t) =
  let cols = columns lens in
  Lens_column.List.present_aliases cols

let colset (lens : t) =
  let sort = sort lens in
  Lens_sort.colset sort

let fundeps (lens : t) =
  let sort = sort lens in
  Lens_sort.fds sort

let predicate (lens : t) =
  let sort = sort lens in
  Lens_sort.predicate sort

let rec get_primary_key (lens : Value.t) =
  match lens with
  | `Lens (_a, sort) ->
    let fds = Lens_sort.fds sort in
    let fd = Lens_fun_dep.Set.min_elt fds in
    let left = Lens_fun_dep.left fd in
    left
  | `LensMem (_a, sort) ->
    let fds = Lens_sort.fds sort in
    let fd = Lens_fun_dep.Set.min_elt fds in
    let left = Lens_fun_dep.left fd in
    left
  | `LensDrop (lens, _, _, _, _) -> get_primary_key lens
  | `LensSelect (lens, _, _) -> get_primary_key lens
  | `LensJoin (lens1, _, _, _, _, _) -> (* right table has to be defined by left table *) get_primary_key lens1
  | _ -> failwith ("Unknown lens (get_primary_key) : " ^ (string_of_value lens))


let rec database (lens : Value.t) =
  match lens with
  | `Lens (((db, _), _, _, _), _) -> db
  | `LensDrop (l, _, _, _, _) -> database l
  | `LensSelect (lens, _, _) -> database lens
  | `LensJoin (l1, _, _, _, _, _) -> database l1
  | _ -> failwith "Unsupported lens for get database."

let rec compute_memory_sorted lens =
  match lens with
  | `Lens _ -> failwith "Non memory lenses not implemented."
  | `LensMem (records, _rtype) -> Lens_sorted_records.construct ~records
  | `LensDrop (lens, drop, _key, _def, _rtype) ->
    let records = compute_memory_sorted lens in
    let columns = List.filter (fun c -> c <> drop) @@ Lens_sorted_records.columns records in
    Lens_sorted_records.project_onto records ~columns
  | `LensSelect (lens, predicate, _sort) ->
    let records = compute_memory_sorted lens in
    Lens_sorted_records.filter records ~predicate
  | `LensJoin (lens1, lens2, on, _left, _right, _sort) ->
    let records1 = compute_memory_sorted lens1 in
    let records2 = compute_memory_sorted lens2 in
    Lens_sorted_records.join records1 records2 ~on
  | _ -> failwith "Not a lens."

let get_memory lens =
  compute_memory_sorted lens |> Lens_sorted_records.to_value

let rec generate_query lens =
  let open Lens_database.Select in
  match lens with
  | `Lens (((db, _), table, _, _), sort) ->
    let cols = Lens_sort.cols sort in
    {
      tables = [table, table];
      cols = cols;
      predicate = None;
      db = db;
    }
  | `LensSelect (lens, pred, _sort) ->
    let query = generate_query lens in
    { query with predicate = Some pred }
  (* get_lens_sort_row_type sort *)
  | `LensJoin (lens1, lens2, _on, _left, _right, sort) ->
    let q1 = generate_query lens1 in
    let q2 = generate_query lens2 in
    (* all table names must be unique, rename them *)
    let _tables2 = List.map (fun (n2, al2) ->
        try
          let _tbl = List.find (fun (n1,_al1) -> n1 = n2) q1.tables in
          failwith "Cannot reuse a table twice in a join query!"
        with
          NotFound _ -> (n2, al2)
      ) q2.tables in
    let tables = List.append q1.tables q2.tables in
    let cols = Lens_sort.cols sort in
    if (q1.db <> q2.db) then
      failwith "Only single database expressions supported."
    else
      {tables = tables; cols = cols; predicate = Lens_sort.predicate sort; db = q1.db}
  | _ -> failwith "Unsupported lens for query"

(** Benchmarking helpers *)
let query_timer = ref 0
let query_count = ref 0

let get_query lens =
  let _ = Debug.print "getting tables" in
  let sort = sort lens in
  let database = database lens in
  let cols = Lens_sort.cols sort in
  let query  = Lens_database.Select.of_sort database ~sort in
  let sql = Format.asprintf "%a" Lens_database.Select.fmt query in
  (* let _ = print_endline sql in *)
  (* let query = lens_get_query lens in
     let sql = construct_select_query query  in *)
  let field_types = List.map (fun c -> Lens_column.alias c, Lens_column.typ c) cols in
  let _ = Debug.print sql in
  let res = Debug.debug_time_out
      (fun () -> Lens_database.Select.execute query ~field_types ~database)
      (fun time -> query_timer := !query_timer + time; query_count := !query_count + 1) in
  res

let lens_get lens =
  if is_memory_lens lens then
    get_memory lens
  else
    get_query lens

let lens_select lens phrase =
  let sort = sort lens in
  let sort = LensTypes.select_lens_sort sort phrase in
  `LensSelect (lens, phrase, sort)

let lens_get_select lens phrase =
    lens_get (lens_select lens phrase)

let lens_get_select_opt lens phrase =
    match phrase with
    | None -> lens_get lens
    | Some phrase  -> lens_get_select lens phrase

let join_lens_should_swap (sort1 : Types.lens_sort) (sort2 : Types.lens_sort) (on_columns : string list) =
    let fds1 = Sort.fds sort1 in
    let fds2 = Sort.fds sort2 in
    let on_cols = Alias.Set.of_list on_columns in
    let covers fds sort =
        let fdcl = Fun_dep.Set.transitive_closure ~cols:on_cols fds in
        let other = Sort.colset sort in
        (* print_endline (ColSet.Show_t.show fdcl ^ " = " ^ ColSet.Show_t.show (other)); *)
        Alias.Set.equal (Column.Set.alias_set other) fdcl in
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
        if Column.List.mem_alias ~alias:nal columns then
            get_new_alias alias columns (num + 1)
        else
            nal in
    (* verify both sorts have all columns in on_columns and that the types match *)
    let on_match = List.for_all (fun onc ->
        let c1 = Sort.find_col_alias ~alias:onc sort1 in
        let c2 = Sort.find_col_alias ~alias:onc sort2 in
        match c1, c2 with
        | Some c1, Some c2 -> Column.typ c1 = Column.typ c2
        | _ -> false) on_columns in
    if not on_match then
        failwith "The key does not match between the two lenses.";
    (* join the two column lists while renaming columns and keeping track of renames *)
    let union, join_renames = List.fold_left (fun (output, jrs) c ->
        (* see if column c's alias already exists *)
        if Column.List.mem_alias ~alias:(Column.alias c) output |> not then
            (* if not, just add the column *)
            c :: output, jrs
        else
            (* is the column a join column *)
          let new_alias = get_new_alias (Lens_column.alias c) output 1 in
            if List.mem (Column.alias c) on_columns then
                (* then renamed column and hide it *)
                (c |> Column.rename ~alias:new_alias |> Column.hide) :: output, (Lens_column.alias c, new_alias) :: jrs
            else
                (* otherwise just rename the column *)
                (c |> Column.rename ~alias:new_alias) :: output, jrs
    ) (Sort.cols sort1, []) (Sort.cols sort2) in
    (* combine the predicates *)
    let join_renames_m = Alias.Map.from_alist join_renames in
    let pred = match Sort.predicate sort1, Sort.predicate sort2 with
    | None, None -> None
    | Some p1, None -> Some p1
    | None, Some p2 -> Some (Phrase.rename_var p2 ~replace:join_renames_m)
    | Some p1, Some p2 -> Some (Phrase.and' (Phrase.tuple_singleton p1) (Phrase.tuple_singleton (Phrase.rename_var p2 ~replace:join_renames_m))) in
    let predicate = List.fold_left (fun pred (alias, newalias) ->
        let jn = Phrase.equal (Phrase.var alias) (Phrase.var newalias) in
        match pred with Some p -> Some (Phrase.and' p jn) | None -> Some jn
    ) pred join_renames in
    let fds = Fun_dep.Set.union (Sort.fds sort1) (Sort.fds sort2) in
    (* determine the on column renames as a tuple (join, left, right) *)
    let jrs = List.map (fun on ->
        let left = on in
        let (_, right) = List.find (fun (a,_) -> a = on) join_renames in
        on, left, right) on_columns in
    Sort.make ~fds ~predicate union, jrs

let query_exists (lens : Value.t) phrase =
  let sort = sort lens in
  let sort = LensTypes.select_lens_sort sort phrase in
  if is_memory_lens lens then
    let res = lens_get (`LensSelect (lens, phrase, sort)) in
    unbox_list res <> []
  else
    let database = database lens in
    let query = Lens_database.Select.of_sort database ~sort in
    let res = Debug.debug_time_out
        (fun () -> Lens_database.Select.query_exists query ~database)
        (fun time -> query_timer := !query_timer + time; query_count := !query_count + 1) in
    res
