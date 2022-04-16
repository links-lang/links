open Lens_utility

type behaviour = Classic | Incremental

module Error = struct
  type t =
    | InvalidDataType
    | InvalidData
    | ViolatesFunDepConstraint of Fun_dep.t

  exception E of t

  let raise e = E e |> raise
end

let satisfies_predicate sort data =
  let predicate = Sort.predicate sort in
  let f record =
    let record =
      try Phrase.Value.unbox_record record with
      | Phrase.Value.Unbox_error.E _ -> Error.raise Error.InvalidDataType
    in
    let map = Alias.Map.from_alist record in
    let lookup key =
      Alias.Map.find_opt key map
      |> Option.value_exn
           ~exn:
             (Failure
                "Could not find column in record. Was this record type checked?")
    in
    Phrase.Option.eval predicate lookup |> Phrase.Value.unbox_bool
  in
  try
    let res = List.for_all ~f data in
    if res then Result.return () else Error.raise Error.InvalidData
  with
  | Error.E e -> Result.error e

let satisfies_fds sort records =
  let columns = Sort.cols_present_aliases sort in
  let records = Sorted_records.construct_cols ~columns ~records in
  let fds = Sort.fds sort in
  let check_fd fd =
    let both =
      List.append
        (Fun_dep.left fd |> Alias.Set.elements)
        (Fun_dep.right fd |> Alias.Set.elements)
    in
    let proj_both = Sorted_records.project_onto records ~columns:both in
    let proj_left =
      Sorted_records.project_onto records
        ~columns:(Fun_dep.left fd |> Alias.Set.elements)
    in
    if Sorted_records.total_size proj_both > Sorted_records.total_size proj_left
    then Error.raise (Error.ViolatesFunDepConstraint fd)
  in
  try Fun_dep.Set.iter check_fd fds |> Result.return with
  | Error.E e -> Result.error e

let put ?(behaviour = Incremental) ~db lens data =
  let open Result.O in
  let sort = Value.sort lens in
  satisfies_predicate sort data >>= fun () ->
  satisfies_fds sort data >>| fun () ->
  match behaviour with
  | Incremental -> Eval_incremental.lens_put ~db lens data
  | Classic -> Eval_classic.lens_put ~db lens data
