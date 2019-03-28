open Lens_utility

type t = Lens of Sort.t

let pp f _ = Format.fprintf f "Lens"

let show _ = "Lens"

let sort v = match v with Lens sort -> sort

let equal t1 t2 =
  match (t1, t2) with Lens sort1, Lens sort2 -> Sort.equal sort1 sort2

module Lens_error = struct
  type t =
    | UnboundColumns of Alias.Set.t
    | ProbablyCycle of Alias.Set.t
    | FunDepNotTreeForm of Alias.Set.t

  let of_fun_dep_check_error e =
    match e with
    | Fun_dep.Check_error.UnboundColumns c -> UnboundColumns c
    | Fun_dep.Check_error.ProbablyCycle c -> ProbablyCycle c
    | Fun_dep.Check_error.FunDepNotTreeForm c -> FunDepNotTreeForm c
end

let check_tree_form fds ~columns =
  let open Result.O in
  Fun_dep.Tree.of_fds fds ~columns
  |> Result.map_error ~f:Lens_error.of_fun_dep_check_error
  >>| fun _ -> ()

let type_lens_fun_dep ~fds ~columns =
  let cols = columns in
  let columns = Column.List.present_aliases_set columns in
  let open Result.O in
  Fun_dep.Set.checked_fds_of_lists fds ~columns
  |> Result.map_error ~f:Lens_error.of_fun_dep_check_error
  >>= fun fds ->
  check_tree_form fds ~columns
  >>| fun () ->
  let sort = Sort.make ~fds cols in
  Lens sort

module Select_lens_error = struct
  type 'a t =
    | SortError of Sort.Select_sort_error.t
    | PredicateTypeError of 'a Phrase_typesugar.error
    | PredicateNotBoolean of Phrase_type.t
end

let type_select_lens t ~predicate =
  let open Result.O in
  let sort = sort t in
  Phrase_typesugar.tc_sort ~sort predicate
  |> Result.map_error ~f:(fun v -> Select_lens_error.PredicateTypeError v)
  >>= (function
        | Phrase_type.Bool -> Phrase.of_sugar predicate |> Result.return
        | _ as t -> Select_lens_error.PredicateNotBoolean t |> Result.error)
  >>= fun predicate ->
  Sort.select_lens_sort sort ~predicate
  |> Result.map_error ~f:(fun v -> Select_lens_error.SortError v)
  >>| fun sort -> Lens sort

module Drop_lens_error = struct
  type t = Sort.Drop_sort_error.t
end

let type_drop_lens t ~drop ~default ~key =
  let open Result.O in
  let sort = sort t in
  let default = List.map ~f:Phrase_value.default_value default in
  Sort.drop_lens_sort sort ~drop ~default ~key >>| fun sort -> Lens sort
