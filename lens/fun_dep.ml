open Lens_utility
open Lens_utility.O
open Result.O

type t = Alias.Set.t * Alias.Set.t [@@deriving show]

let left (l, _) = l

let right (_, r) = r

let pp_pretty f fd =
  let pp_cols = Format.pp_print_list ~pp_sep:(Format.pp_constant " ") Format.pp_print_string in
  Format.fprintf f "%a -> %a" pp_cols (left fd |> Alias.Set.elements) pp_cols (right fd |> Alias.Set.elements)

let compare (l1, r1) (l2, r2) =
  let res = Alias.Set.compare l1 l2 in
  if res = 0 then Alias.Set.compare r1 r2 else res

let of_lists ((left, right) : string list * string list) : t =
  let left = Alias.Set.of_list left in
  let right = Alias.Set.of_list right in
  (left, right)

let make left right : t = (left, right)

let key_fd ~keys ~cols =
  let keys = Alias.Set.of_list keys in
  let cols = Alias.Set.of_list cols in
  let right = Alias.Set.diff cols keys in
  make keys right

module Check_error = struct
  type t =
    | UnboundColumns of Alias.Set.t
    | ProbablyCycle of Alias.Set.t
    | FunDepNotTreeForm of Alias.Set.t
  [@@deriving show]

  exception E of t

  let raise e = E e |> raise
end

module Remove_defines_error = struct
  type t = DefiningFDNotFound of Alias.Set.t
  [@@deriving show]
end

module Compare = struct
  type elt = t [@@deriving show]

  type t = elt [@@deriving show]

  let compare = compare
end

module Set = struct
  include Set.Make (Compare)

  let pp_pretty f v =
    elements v
    |> (Format.pp_print_list ~pp_sep:(Format.pp_constant "; ") pp_pretty) f

  let show_pretty = Format.show_of_pp pp_pretty

  let of_lists (fds : (string list * string list) list) : t =
    let fds = List.map ~f:of_lists fds in
    of_list fds

  let remove_defines t ~cols =
    (* first try and find the fd X -> Y where Y is a subset of [cols]. *)
    let def_fd =
      find_first_opt (right >> fun r -> Alias.Set.subset cols r) t
    in
    Result.of_option def_fd ~error:(fun _ ->
        Remove_defines_error.DefiningFDNotFound cols |> Result.error )
    >>| fun def_fd ->
    let new_def = left def_fd in
    (* replace all FDs which assume Y -> Z (where Y is a subset of cols) and replace them with X -> Z *)
    let t =
      map
        (fun fd ->
          if Alias.Set.subset (left fd) cols then make new_def (right fd)
          else fd )
        t
    in
    (* remove X -> Y *)
    let t = remove def_fd t in
    let right = Alias.Set.diff (right def_fd) cols in
    (* if our updated X -> Y where Y has [cols] removed is empty, don't re-add it *)
    if Alias.Set.is_empty right then t else add (make new_def right) t

  let key_fds ~keys ~cols = of_list [key_fd ~keys ~cols]

  (* Find the a functional dependency at the root, which is the functional dependency that defines all other nodes *)
  let root_fds fds =
    let res =
      filter
        (fun fd ->
          not (exists (fun fd2 -> Alias.Set.subset (right fd2) (left fd)) fds)
          )
        fds
    in
    elements res

  let simple_key fds ~columns =
    let defined_columns =
      fold (fun e acc -> right e |> Alias.Set.union acc) fds Alias.Set.empty
    in
    Alias.Set.diff columns defined_columns

  (* Get the functional dependency which defines the columns cols *)
  let defining_fd fds ~cols =
    let res = filter (fun fd -> Alias.Set.subset cols (right fd)) fds in
    min_elt res

  let transitive_closure fds ~cols =
    let rec get attrs fds =
      let newAttrs, fds =
        partition (fun fd -> Alias.Set.subset (left fd) attrs) fds
      in
      let newAttrs =
        fold (fun fd c -> Alias.Set.union c (right fd)) newAttrs attrs
      in
      if Alias.Set.cardinal newAttrs > Alias.Set.cardinal attrs then
        get newAttrs fds
      else attrs
    in
    get cols fds

  let checked_fds_of_lists fds ~columns =
    let to_set (left, right) =
      (Alias.Set.of_list left, Alias.Set.of_list right)
    in
    let fds = List.map ~f:to_set fds in
    let check_input (left, right) =
      Alias.Set.subset left columns && Alias.Set.subset right columns
    in
    List.for_all_or_error ~f:check_input
      ~error:(fun (l, r) ->
        let fdcols = Alias.Set.union l r in
        Check_error.UnboundColumns (Alias.Set.diff fdcols columns) )
      fds
    >>| fun () ->
    let fd_of (left, right) = make left right in
    List.map ~f:fd_of fds |> of_list

  let all_cols fds =
    let cols fd = Alias.Set.union (left fd) (right fd) in
    elements fds |> List.map ~f:cols |> Alias.Set.union_all

  let outputs fds =
    fold (fun elt v -> right elt |> Alias.Set.union v) fds Alias.Set.empty
end

module Tree = struct
  type elt = Alias.Set.t [@@deriving show]

  type node = FDNode of elt * t

  and t = node list [@@deriving show]

  let cols n = match n with FDNode (cols, _) -> cols

  let subnodes n = match n with FDNode (_, subnodes) -> subnodes

  let rec pp_pretty fmt v =
    let pp_node node =
      Format.pp_force_newline fmt () ;
      Format.pp_open_box fmt 2 ;
      Format.pp_print_string fmt "-" ;
      Format.pp_print_space fmt () ;
      Alias.Set.pp_pretty fmt (cols node) ;
      pp_pretty fmt (subnodes node) ;
      Format.pp_close_box fmt ()
    in
    List.iter pp_node v

  let show_pretty v = Format.asprintf "%a" pp_pretty v

  let rec fd_subnodes fds key =
    let subfds = Set.filter (fun fd -> Alias.Set.subset (left fd) key) fds in
    let subkeys =
      Set.elements subfds |> List.map ~f:left
      |> List.sort_uniq Alias.Set.compare
    in
    let remain =
      Alias.Set.diff key
        (List.fold_right Alias.Set.union subkeys Alias.Set.empty)
    in
    let of_sub_key key =
      let subfds = Set.filter (left >> Alias.Set.equal key) fds in
      let remaining = Set.diff fds subfds in
      let subfds =
        List.map ~f:(right >> fd_subnodes remaining) (Set.elements subfds)
        |> List.flatten
      in
      FDNode (key, subfds)
    in
    let subtrees = List.map ~f:of_sub_key subkeys in
    if Alias.Set.is_empty remain then subtrees
    else FDNode (remain, []) :: subtrees

  let is_disjoint tree ~columns =
    (* for a tree to be disjoint, traverse a tree and ensure that the columns of
       each node are in columns, then continue traversing the tree without those columns. *)
    let columns = ref columns in
    let joint = ref Alias.Set.empty in
    let rec traverse (FDNode (c, subnodes)) =
      if Alias.Set.subset c !columns then (
        columns := Alias.Set.diff !columns c ;
        List.for_all ~f:traverse subnodes )
      else (
        joint := c ;
        false )
    in
    List.for_all ~f:traverse tree
    |> Result.of_bool ~error:(Check_error.FunDepNotTreeForm !joint)

  let rec all_cols tree =
    let f tree =
      let sub = subnodes tree |> all_cols in
      let node = cols tree in
      Alias.Set.union sub node
    in
    let l = List.map ~f tree in
    Alias.Set.union_all l

  let of_fds fds ~columns =
    let roots = Set.simple_key fds ~columns in
    let tree = fd_subnodes fds roots in
    let not_included = Alias.Set.diff (Set.all_cols fds) (all_cols tree) in
    Alias.Set.is_empty not_included
    |> Result.of_bool ~error:(Check_error.ProbablyCycle not_included)
    >>= fun () -> is_disjoint tree ~columns >>| fun () -> tree
end
