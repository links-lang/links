open Lens_utility
module Column = Column
module Phrase = Lens_phrase

type t =
  { fds: Fun_dep.Set.t
  ; predicate: Lens_phrase.t option
  ; cols: Column.t list }
[@@deriving show]

let fds t = t.fds

let predicate t = t.predicate

let cols t = t.cols

let cols_present_aliases t = Column.List.present_aliases t.cols

let colset t = t.cols |> Column.Set.of_list

let present_colset t =
  t.cols |> Column.List.present |> Column.Set.of_list

let make ?(fds = Fun_dep.Set.empty) ?(predicate = None) cols =
  {fds; predicate; cols}

let find_col_alias t ~alias = Column.List.find_alias ~alias t.cols

let update_table_name t ~table =
  let cols = t.cols |> List.map ~f:(Column.set_table ~table) in
  {t with cols}

let update_predicate t ~predicate = {t with predicate}

let equal sort1 sort2 =
  let fd_equal = Fun_dep.Set.equal (fds sort1) (fds sort2) in
  let pred_equal = predicate sort1 = predicate sort2 in
  let cols_equal = Column.Set.equal (colset sort1) (colset sort2) in
  fd_equal && pred_equal && cols_equal

let record_type t = cols t |> Column.List.record_type

let join_lens_should_swap sort1 sort2 ~on:on_columns =
  let fds1 = fds sort1 in
  let fds2 = fds sort2 in
  let on_cols = Alias.Set.of_list on_columns in
  let covers fds sort =
    let fdcl = Fun_dep.Set.transitive_closure ~cols:on_cols fds in
    let other = colset sort in
    Alias.Set.equal (Column.Set.alias_set other) fdcl
  in
  if covers fds2 sort2 then false
  else if covers fds1 sort1 then true
  else failwith "One of the tables needs to be defined by the join column set."

let select_lens_sort sort ~predicate:pred =
  let oldPred = predicate sort in
  let predicate = Phrase.Option.combine_and oldPred (Some pred) in
  update_predicate sort ~predicate

let drop_lens_sort sort ~drop ~key =
  (* Verify that the functional dependencies contain X \to A *)
  if
    Alias.Set.subset drop
      (Fun_dep.Set.transitive_closure ~cols:key (fds sort))
    |> not
  then failwith "The dropped columns must be defined by the key" ;
  let fds = Fun_dep.Set.remove_defines (fds sort) ~cols:drop in
  let cols =
    List.map
      ~f:(fun c ->
        if Alias.Set.mem (Column.alias c) drop then Column.hide c
        else c )
      (cols sort)
  in
  let predicate = predicate sort in
  make ~fds ~predicate cols

let join_lens_sort sort1 sort2 ~on =
  (* helper function to find new alias, e.g. for 'name' it will find 'name_1', 'name_2' etc. *)
  let rec get_new_alias alias columns num =
    let nal = alias ^ "_" ^ string_of_int num in
    if Column.List.mem_alias ~alias:nal columns then
      get_new_alias alias columns (num + 1)
    else nal
  in
  (* verify both sorts have all columns in on_columns and that the types match *)
  let on_match =
    List.for_all
      ~f:(fun onc ->
        let c1 = find_col_alias ~alias:onc sort1 in
        let c2 = find_col_alias ~alias:onc sort2 in
        match (c1, c2) with
        | Some c1, Some c2 -> Column.typ c1 = Column.typ c2
        | _ -> false )
      on
  in
  if not on_match then
    failwith "The key does not match between the two lenses." ;
  (* join the two column lists while renaming columns and keeping track of renames *)
  let union, join_renames =
    List.fold_left
      (fun (output, jrs) c ->
        (* see if column c's alias already exists *)
        if Column.List.mem_alias ~alias:(Column.alias c) output |> not then
          (* if not, just add the column *)
          (c :: output, jrs)
        else
          (* is the column a join column *)
          let new_alias = get_new_alias (Column.alias c) output 1 in
          if List.mem ~equal:String.equal on (Column.alias c) then
            (* then renamed column and hide it *)
            ( (c |> Column.rename ~alias:new_alias |> Column.hide) :: output
            , (Column.alias c, new_alias) :: jrs )
          else
            (* otherwise just rename the column *)
            ((c |> Column.rename ~alias:new_alias) :: output, jrs) )
      (cols sort1, [])
      (cols sort2)
  in
  (* combine the predicates *)
  let join_renames_m = Alias.Map.from_alist join_renames in
  let pred =
    match (predicate sort1, predicate sort2) with
    | None, None -> None
    | Some p1, None -> Some p1
    | None, Some p2 -> Some (Phrase.rename_var p2 ~replace:join_renames_m)
    | Some p1, Some p2 ->
        Some
          (Phrase.and'
             (Phrase.tuple_singleton p1)
             (Phrase.tuple_singleton
                (Phrase.rename_var p2 ~replace:join_renames_m)))
  in
  let predicate =
    List.fold_left
      (fun pred (alias, newalias) ->
        let jn = Phrase.equal (Phrase.var alias) (Phrase.var newalias) in
        match pred with Some p -> Some (Phrase.and' p jn) | None -> Some jn )
      pred join_renames
  in
  let fds = Fun_dep.Set.union (fds sort1) (fds sort2) in
  (* determine the on column renames as a tuple (join, left, right) *)
  let jrs =
    List.map
      ~f:(fun on ->
        let left = on in
        let _, right = List.find_exn ~f:(fun (a, _) -> a = on) join_renames in
        (on, left, right) )
      on
  in
  (make ~fds ~predicate union, jrs)
