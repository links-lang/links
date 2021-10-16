open Lens_utility
open Lens_utility.O
module Column = Column

type t = {
  fds : Fun_dep.Set.t;
  predicate : Phrase.t option;
  query : Phrase.t option;
  cols : Column.t list;
}
[@@deriving show, sexp]

let fds t = t.fds

let predicate t = t.predicate

let query t = t.query

let cols t = t.cols

let cols_present_aliases t = Column.List.present_aliases t.cols

let cols_present_aliases_set t = cols_present_aliases t |> Alias.Set.of_list

let colset t = t.cols |> Column.Set.of_list

let present_colset t = t.cols |> Column.List.present |> Column.Set.of_list

let make ?(fds = Fun_dep.Set.empty) ?(predicate = None) ?(query = None) cols =
  { fds; predicate; query; cols }

let set_serial sort ~columns =
  let cols =
    cols sort
    |> List.map ~f:(fun c ->
           if Alias.Set.mem (Column.alias c) columns then
             Column.set_typ ~typ:Phrase_type.Serial c
           else c)
  in
  { sort with cols }

let find_col_alias t ~alias = Column.List.find_alias ~alias t.cols

let update_table_name t ~table =
  let cols = t.cols |> List.map ~f:(Column.set_table ~table) in
  { t with cols }

let update_predicate t ~query ~predicate = { t with predicate; query }

let update_fds t ~fds = { t with fds }

let equal sort1 sort2 =
  let fd_equal = Fun_dep.Set.equal (fds sort1) (fds sort2) in
  let pred_equal = predicate sort1 = predicate sort2 in
  let cols_equal = Column.Set.equal (colset sort1) (colset sort2) in
  fd_equal && pred_equal && cols_equal

let record_type t = cols t |> Column.List.record_type

module Lens_sort_error = struct
  type t = UnboundColumns of Alias.Set.t [@@deriving eq]
end

let lens_sort ~fds ~columns =
  let open Result.O in
  let unbound =
    Alias.Set.diff
      (Fun_dep.Set.all_columns fds)
      (Column.List.present_aliases_set columns)
  in
  Alias.Set.is_empty unbound
  |> Result.of_bool ~error:(Lens_sort_error.UnboundColumns unbound)
  >>| fun () -> make ~fds columns

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
    | PredicateDoesntIgnoreOutputs of {
        fds : Fun_dep.Set.t;
        columns : Alias.Set.t;
      }
    | TreeFormError of { error : Fun_dep.Tree.Tree_form_error.t }
    | UnboundColumns of Alias.Set.t
  [@@deriving show, eq]
end

let select_lens_sort_dynamic sort =
  let open Result.O in
  let fds' = fds sort in
  Fun_dep.Tree.in_tree_form fds'
  |> Result.map_error ~f:(fun error ->
         Select_sort_error.TreeFormError { error })
  >>= fun fds' ->
  let oldPred = predicate sort in
  let outputs = fds sort |> Fun_dep.Set.outputs in
  let violating_outputs =
    Alias.Set.inter outputs (Phrase.Option.get_vars oldPred)
  in
  violating_outputs
  |> Alias.Set.is_empty
  |> Result.of_bool
       ~error:
         (Select_sort_error.PredicateDoesntIgnoreOutputs
            { fds = fds'; columns = violating_outputs })
  >>| fun () -> sort |> update_fds ~fds:fds'

let select_lens_sort sort ~predicate:pred =
  let open Result.O in
  select_lens_sort_dynamic sort >>= fun sort ->
  let predVars = Phrase.get_vars pred in
  let oldPred = predicate sort in
  let unbound_columns =
    cols_present_aliases_set sort |> Alias.Set.diff predVars
  in
  Alias.Set.is_empty unbound_columns
  |> Result.of_bool ~error:(Select_sort_error.UnboundColumns unbound_columns)
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
    | DefaultDoesntMatchPredicate
    | DropTypeError of {
        column : Alias.t;
        default_type : Phrase_type.t;
        column_type : Phrase_type.t;
      }
    | NotIndependent of Alias.Set.t
  [@@deriving show, eq]
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
  List.length drop
  = List.length default
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
         Drop_sort_error.DropTypeError
           { column = key; column_type; default_type })
  >>= fun () ->
  (* remove the functional dependency which defines the drop column *)
  Fun_dep.Set.remove_defines (fds sort) ~cols:drop_set
  |> Result.map_error ~f:(function
         | Fun_dep.Remove_defines_error.DefiningFDNotFound c ->
         Drop_sort_error.DefiningFDNotFound c)
  >>= fun fds ->
  (* hide all columns that are dropped. *)
  let cols =
    List.map_if
      ~b:(Column.alias >> fun c -> Alias.Set.mem c drop_set)
      ~f:Column.hide (cols sort)
  in
  (* remove references to the dropped column by performing partial evaluation with
     the default value. *)
  let replace = List.zip_exn drop default |> Alias.Map.from_alist in
  (* first simplify the predicate *)
  let predicate =
    predicate sort |> Phrase.Option.partial_eval ~lookup:(fun _ -> None)
  in
  (* ensure that predicate can be rewritten into P = P[A] join P[U-A] *)
  let gtv =
    Option.map ~f:Phrase.Grouped_variables.gtv predicate
    |> Option.value ~default:Alias.Set.Set.empty
  in
  Phrase.Grouped_variables.no_partial_overlaps gtv ~cols:drop_set
  |> Result.map_error ~f:(fun (Phrase.Grouped_variables.Error.Overlaps cols) ->
         Drop_sort_error.NotIndependent cols)
  >>= fun () ->
  (* calculate P[U-A]*)
  let predicate' = predicate in
  let predicate =
    predicate
    |> Phrase.Option.partial_eval ~lookup:(fun key ->
           Alias.Map.find replace ~key)
  in
  (* Ensure that (A=a) satisfies P[A]. *)
  (match (predicate', predicate) with
  | Some (Phrase.Constant (Phrase.Value.Bool false)), _ -> Result.return ()
  | _, Some (Phrase.Constant (Phrase.Value.Bool false)) ->
      Result.error Drop_sort_error.DefaultDoesntMatchPredicate
  | _ -> Result.return ())
  >>| fun () ->
  (* query is unchanged *)
  let query = query sort in
  make ~fds ~predicate ~query cols

let get_unaliased col =
  let parts = String.split_on_char '_' col in
  let last_part_no = List.length parts - 1 in
  let alias_no = List.nth parts last_part_no |> int_of_string_opt in
  match alias_no with
  | None -> (col, 1)
  | Some v -> (String.concat "_" (List.take ~n:last_part_no parts), v + 1)

(* helper function to find new alias, e.g. for 'name' it will find 'name_1', 'name_2' etc. *)
let get_new_alias alias columns =
  let alias, num = get_unaliased alias in
  let rec try_next num =
    let nal = alias ^ "_" ^ string_of_int num in
    if Column.List.mem_alias ~alias:nal columns then try_next (num + 1) else nal
  in
  try_next num

module Join_sort_error = struct
  type t =
    | UnboundColumn of Alias.Set.t
    | AlreadyBound of Alias.Set.t
    | TreeFormError of { error : Fun_dep.Tree.Tree_form_error.t }
  [@@deriving eq]
end

let rename_columns ~rename columns =
  let rename c =
    let key = Column.alias c in
    Alias.Map.find rename ~key
    |> Option.map ~f:(fun v -> Column.rename ~alias:v c)
    |> Option.value ~default:c
  in
  List.map ~f:rename columns

let join_lens_sort sort1 sort2 ~on =
  let open Result.O in
  (* verify both sorts have all columns in on_columns and that the types match *)
  let on_left = List.map ~f:(fun (l, _, _) -> l) on |> Alias.Set.of_list in
  let cols_left = cols_present_aliases_set sort1 in
  let unbound_left = Alias.Set.diff on_left cols_left in
  let on_right = List.map ~f:(fun (_, r, _) -> r) on |> Alias.Set.of_list in
  let cols_right = cols_present_aliases_set sort2 in
  let unbound_right = Alias.Set.diff on_right cols_right in
  let unbound = Alias.Set.union unbound_left unbound_right in
  Alias.Set.is_empty unbound
  |> Result.of_bool ~error:(Join_sort_error.UnboundColumn unbound)
  >>= fun () ->
  Fun_dep.Tree.in_tree_form (fds sort1)
  |> Result.map_error ~f:(fun error -> Join_sort_error.TreeFormError { error })
  >>= fun fds_left ->
  Fun_dep.Tree.in_tree_form (fds sort2)
  |> Result.map_error ~f:(fun error -> Join_sort_error.TreeFormError { error })
  >>= fun fds_right ->
  (* Find all 'on' target name columns that are already bound in one of the lenses. *)
  let on_bind_to = List.map ~f:(fun (_, _, t) -> t) on |> Alias.Set.of_list in
  let already_bound_left =
    Alias.Set.diff (Alias.Set.inter cols_left on_bind_to) on_left
  in
  let already_bound_right =
    Alias.Set.diff (Alias.Set.inter cols_right on_bind_to) on_right
  in
  let already_bound = Alias.Set.union already_bound_left already_bound_right in
  Alias.Set.is_empty already_bound
  |> Result.of_bool ~error:(Join_sort_error.AlreadyBound already_bound)
  >>= fun () ->
  (* join the two column lists while renaming columns and keeping track of renames *)
  let cols_l =
    cols sort1
    |> rename_columns
         ~rename:
           (List.map ~f:(fun (l, _, t) -> (l, t)) on |> Alias.Map.from_alist)
  in
  let cols_r =
    cols sort2
    |> rename_columns
         ~rename:
           (List.map ~f:(fun (_, r, t) -> (r, t)) on |> Alias.Map.from_alist)
  in
  let union, join_renames =
    List.fold_left
      (fun (output, jrs) c ->
        (* see if column c's alias already exists *)
        if Column.List.mem_alias ~alias:(Column.alias c) output |> not then
          (* if not, just add the column *)
          (c :: output, jrs)
        else
          (* is the column a join column *)
          let new_alias = get_new_alias (Column.alias c) output in
          if Alias.Set.mem (Column.alias c) on_bind_to then
            (* then renamed column and hide it *)
            ( (c |> Column.rename ~alias:new_alias |> Column.hide) :: output,
              (Column.alias c, new_alias) :: jrs )
          else
            (* otherwise just rename the column *)
            ((c |> Column.rename ~alias:new_alias) :: output, jrs))
      (cols_l, []) cols_r
  in
  (* combine the predicates *)
  let join_renames_m = Alias.Map.from_alist join_renames in
  let pred =
    let predicate2 =
      Option.map
        ~f:(Phrase.rename_var ~replace:join_renames_m)
        (predicate sort2)
    in
    Phrase.Option.combine_and (predicate sort1) predicate2
  in
  let query =
    List.fold_left
      (fun pred (alias, newalias) ->
        let jn = Phrase.equal (Phrase.var alias) (Phrase.var newalias) in
        Phrase.Option.combine_and (Some jn) pred)
      pred join_renames
  in
  let fds = Fun_dep.Set.union fds_left fds_right in
  Fun_dep.Tree.in_tree_form fds
  |> Result.map_error ~f:(fun error -> Join_sort_error.TreeFormError { error })
  >>| fun fds ->
  (* determine the on column renames as a tuple (join, left, right) *)
  let jrs =
    Alias.Set.elements on_bind_to
    |> List.map ~f:(fun on ->
           let left = on in
           let _, right =
             List.find_exn ~f:(fun (a, _) -> a = on) join_renames
           in
           (on, left, right))
  in
  (make ~fds ~query ~predicate:pred union, jrs)
