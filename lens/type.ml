open Lens_utility

type t =
  | ConcreteLens of Sort.t
  | AbstractLens of { checked : bool; sort : Sort.t }

let pp f _ = Format.fprintf f "Lens"

let show _ = "Lens"

let cols v =
  match v with
  | ConcreteLens sort -> Sort.cols sort
  | AbstractLens { sort; _ } -> Sort.cols sort

let update v f1 f2 =
  let open Result.O in
  match v with
  | ConcreteLens sort -> f1 sort >>| fun sort -> ConcreteLens sort
  | AbstractLens { sort; _ } ->
      f2 sort >>| fun sort -> AbstractLens { sort; checked = false }

let sort v =
  match v with
  | ConcreteLens sort -> sort
  | AbstractLens { sort; _ } -> sort

let set_serial sort ~columns =
  match sort with
  | ConcreteLens sort ->
      let sort = Sort.set_serial sort ~columns in
      ConcreteLens sort
  | AbstractLens { sort; checked } ->
      let sort = Sort.set_serial sort ~columns in
      AbstractLens { sort; checked }

let equal t1 t2 =
  match (t1, t2) with
  | ConcreteLens sort1, ConcreteLens sort2 -> Sort.equal sort1 sort2
  | ( AbstractLens { sort; checked },
      AbstractLens { sort = sort'; checked = chk' } ) ->
      Sort.equal sort sort' && checked = chk'
  | _ -> false

let type_lens_fun_dep ~fds ~columns =
  let open Result.O in
  let fds = Fun_dep.Set.of_lists fds in
  Sort.lens_sort ~fds ~columns >>| fun sort -> ConcreteLens sort

module Select_lens_error = struct
  type 'a t =
    | SortError of Sort.Select_sort_error.t
    | PredicateTypeError of 'a Phrase_typesugar.error
    | PredicateNotBoolean of Phrase_type.t
end

let type_select_lens_dynamic t =
  let open Result.O in
  sort t
  |> Sort.select_lens_sort_dynamic
  |> Result.map_error ~f:(fun v -> Select_lens_error.SortError v)
  >>| fun sort -> AbstractLens { checked = false; sort }

let type_select_lens t ~predicate =
  let open Result.O in
  let columns = cols t in
  (Phrase_typesugar.tc_columns ~columns predicate
   |> Result.map_error ~f:(fun v -> Select_lens_error.PredicateTypeError v)
   >>= function
   | Phrase_type.Bool -> Phrase.of_sugar predicate |> Result.return
   | _ as t -> Select_lens_error.PredicateNotBoolean t |> Result.error)
  >>= fun predicate ->
  update t
    (fun sort -> Sort.select_lens_sort sort ~predicate)
    (fun sort -> sort |> Result.return)
  |> Result.map_error ~f:(fun v -> Select_lens_error.SortError v)

module Drop_lens_error = struct
  type t = Sort.Drop_sort_error.t
end

let type_drop_lens t ~drop ~default ~key =
  let check sort =
    let default = List.map ~f:Phrase_value.default_value default in
    Sort.drop_lens_sort sort ~drop ~default ~key
  in
  update t check check

module Join_lens_error = struct
  type lens = Left | Right

  type 'a t =
    | PredicateTypeError of lens * 'a Phrase_typesugar.error
    | PredicateNotBoolean of lens * Phrase_type.t
    | SortError of Sort.Join_sort_error.t
end

let type_join_lens s t ~on ~del_left ~del_right =
  let open Result.O in
  let open Join_lens_error in
  let cols1 = cols s in
  let cols2 = cols t in
  (Phrase_typesugar.tc_columns ~columns:cols1 del_left
   |> Result.map_error ~f:(fun v -> PredicateTypeError (Left, v))
   >>= function
   | Phrase_type.Bool -> Phrase.of_sugar del_left |> Result.return
   | _ as t -> PredicateNotBoolean (Left, t) |> Result.error)
  >>= fun _del_left ->
  (Phrase_typesugar.tc_columns ~columns:cols2 del_right
   |> Result.map_error ~f:(fun v -> PredicateTypeError (Right, v))
   >>= function
   | Phrase_type.Bool -> Phrase.of_sugar del_right |> Result.return
   | _ as t -> PredicateNotBoolean (Right, t) |> Result.error)
  >>= fun _del_right ->
  let f sort1 sort2 =
    Sort.join_lens_sort sort1 sort2 ~on
    |> Result.map_error ~f:(fun e -> SortError e)
    >>| fst
  in
  match (s, t) with
  | AbstractLens { sort = sort1; _ }, _ ->
      let sort2 = sort t in
      f sort1 sort2 >>| fun sort -> AbstractLens { sort; checked = false }
  | _, AbstractLens { sort = sort2; _ } ->
      let sort1 = sort s in
      f sort1 sort2 >>| fun sort -> AbstractLens { sort; checked = false }
  | _ ->
      let sort1 = sort s in
      let sort2 = sort t in
      f sort1 sort2 >>| fun sort -> ConcreteLens sort

let record_type t = sort t |> Sort.record_type

module Unchecked_lens_error = struct
  type t = UncheckedLens
end

let is_abstract s =
  match s with
  | AbstractLens _ -> true
  | ConcreteLens _ -> false

let is_checked s =
  match s with
  | AbstractLens { checked; _ } -> checked
  | ConcreteLens _ -> true

let ensure_checked s =
  if is_checked s then Result.return ()
  else Result.error Unchecked_lens_error.UncheckedLens

let make_checked s =
  match s with
  | AbstractLens { sort; _ } -> AbstractLens { sort; checked = true }
  | _ -> s
