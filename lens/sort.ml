open Lens_utility
open Lens_utility.O
module Column = Column

type t =
  { fds: Fun_dep.Set.t
  ; predicate: Phrase.t option
  ; query: Phrase.t option
  ; cols: Column.t list }
[@@deriving show]

let fds t = t.fds

let predicate t = t.predicate

let query t = t.query

let cols t = t.cols

let cols_present_aliases t = Column.List.present_aliases t.cols

let cols_present_aliases_set t = cols_present_aliases t |> Alias.Set.of_list

let colset t = t.cols |> Column.Set.of_list

let present_colset t = t.cols |> Column.List.present |> Column.Set.of_list

let make ?(fds = Fun_dep.Set.empty) ?(predicate = None) ?(query = None) cols =
  {fds; predicate; query; cols}

let find_col_alias t ~alias = Column.List.find_alias ~alias t.cols

let update_table_name t ~table =
  let cols = t.cols |> List.map ~f:(Column.set_table ~table) in
  {t with cols}

let update_predicate t ~query ~predicate = {t with predicate; query}

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

module Select_sort_error = struct
  type t =
    | PredicateDoesntIgnoreOutputs of {fds: Fun_dep.Set.t; columns: Alias.Set.t}
    | UnboundColumns of Alias.Set.t
  [@@deriving show]

  let equal v1 v2 =
    match (v1, v2) with
    | ( PredicateDoesntIgnoreOutputs {columns; fds}
      , PredicateDoesntIgnoreOutputs {columns= c'; fds= f'} ) ->
        Alias.Set.equal columns c' && Fun_dep.Set.equal fds f'
    | UnboundColumns c, UnboundColumns c' -> Alias.Set.equal c c'
    | _ -> false
end

let select_lens_sort sort ~predicate:pred =
  let open Result.O in
  let oldPred = predicate sort in
  let outputs = fds sort |> Fun_dep.Set.outputs in
  let predVars = Phrase.get_vars pred in
  let unbound_columns =
    cols_present_aliases_set sort |> Alias.Set.diff predVars
  in
  Alias.Set.is_empty unbound_columns
  |> Result.of_bool ~error:(Select_sort_error.UnboundColumns unbound_columns)
  >>= fun () ->
  let fds = fds sort in
  let violating_outputs =
    Alias.Set.inter outputs (Phrase.Option.get_vars oldPred)
  in
  violating_outputs |> Alias.Set.is_empty
  |> Result.of_bool
       ~error:
         (Select_sort_error.PredicateDoesntIgnoreOutputs { fds; columns = violating_outputs })
  >>| fun () ->
  let predicate = Phrase.Option.combine_and oldPred (Some pred) in
  let query = Phrase.Option.combine_and (query sort) (Some pred) in
  update_predicate sort ~query ~predicate

module Drop_sort_error = struct
  type t =
    | UnboundColumns of Alias.Set.t
    | DefiningFDNotFound of Alias.Set.t
    | DropNotDefinedByKey
    | DefaultDropMismatch
    | DropTypeError of
        { column: Alias.t
        ; default_type: Phrase_type.t
        ; column_type: Phrase_type.t }
  [@@deriving show]

  let equal v1 v2 =
    match (v1, v2) with
    | UnboundColumns c, UnboundColumns c' -> Alias.Set.equal c c'
    | DefiningFDNotFound c, DefiningFDNotFound c' -> Alias.Set.equal c c'
    | DropNotDefinedByKey, DropNotDefinedByKey -> true
    | DefaultDropMismatch, DefaultDropMismatch -> true
    | ( DropTypeError {column; default_type; column_type}
      , DropTypeError {column= c; default_type= dt; column_type= ct} ) ->
        column = c
        && Phrase_type.equal default_type dt
        && Phrase_type.equal column_type ct
    | _ -> false
end

let drop_lens_sort sort ~drop ~default ~key =
  let open Result.O in
  let drop_set = Alias.Set.of_list drop in
  let unbound =
    cols_present_aliases_set sort
    |> Alias.Set.diff (Alias.Set.union drop_set key)
  in
  (* ensure the drop columns are bound *)
  Alias.Set.is_empty unbound
  |> Result.of_bool ~error:(Drop_sort_error.UnboundColumns unbound)
  >>= fun () ->
  (* Verify that the functional dependencies contain X \to A *)
  Alias.Set.subset drop_set
    (Fun_dep.Set.transitive_closure ~cols:key (fds sort))
  |> Result.of_bool ~error:Drop_sort_error.DropNotDefinedByKey
  >>= fun () ->
  (* ensure that number of items specified for drop is identical to number of columns to drop *)
  List.length drop = List.length default
  |> Result.of_bool ~error:Drop_sort_error.DefaultDropMismatch
  >>= fun () ->
  (* type check columns *)
  let cols_map =
    List.map ~f:(fun c -> (Column.alias c, c)) (cols sort)
    |> Alias.Map.from_alist
  in
  let tc_column (key, default) =
    let column_type = Alias.Map.find_exn cols_map ~key |> Column.typ in
    let default_type = Phrase_value.type_of default in
    Phrase_type.equal column_type default_type
  in
  List.zip_exn drop default
  |> List.for_all_or_error ~f:tc_column ~error:(fun (key, default) ->
         let column_type = Alias.Map.find_exn ~key cols_map |> Column.typ in
         let default_type = Phrase_value.type_of default in
         Drop_sort_error.DropTypeError {column= key; column_type; default_type}
     )
  >>= fun () ->
  (* remove the functional dependency which defines the drop column *)
  Fun_dep.Set.remove_defines (fds sort) ~cols:drop_set
  |> Result.map_error ~f:(function
         | Fun_dep.Remove_defines_error.DefiningFDNotFound c ->
         Drop_sort_error.DefiningFDNotFound c )
  >>| fun fds ->
  (* hide all columns that are dropped. *)
  let cols =
    List.map_if
      ~b:(Column.alias >> fun c -> Alias.Set.mem c drop_set)
      ~f:Column.hide (cols sort)
  in
  (* remove references to the dropped column by performing partial evaluation with
     the default value. *)
  let replace = List.zip_exn drop default |> Alias.Map.from_alist in
  let predicate =
    predicate sort |> Option.map ~f:(Phrase.replace_var ~replace)
  in
  (* query is unchanged *)
  let query = query sort in
  make ~fds ~predicate ~query cols

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
    let predicate2 = Option.map ~f:(Phrase.rename_var ~replace:join_renames_m) (predicate sort2) in
    Phrase.Option.combine_and (predicate sort1) predicate2 in
  let query =
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
  (make ~fds ~query ~predicate:pred union, jrs)
