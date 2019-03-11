open Lens_utility

type t = Alias.Set.t * Alias.Set.t
  [@@deriving show]

let left (l,_) = l
let right (_,r) = r

let compare (l1,r1) (l2, r2) =
  let res = Alias.Set.compare l1 l2 in
  if res = 0 then
    Alias.Set.compare r1 r2
  else
    res

let of_lists (left, right : string list * string list) : t =
  let left = Alias.Set.of_list left in
  let right = Alias.Set.of_list right in
  (left, right)

let make left right : t = (left, right)

let key_fd ~keys ~cols =
  let keys = Alias.Set.of_list keys in
  let cols = Alias.Set.of_list cols in
  let right = Alias.Set.diff cols keys in
  make keys right

module Compare = struct
  type elt = t [@@deriving show]
  type t = elt [@@deriving show]

  let compare = compare
end

module Set = struct
  include Set.Make(Compare)

  let of_lists (fds : (string list * string list) list) : t =
    let fds = List.map ~f:of_lists fds in
    of_list fds

  let remove_defines t ~cols =
    let remove fd =
      if Alias.Set.inter cols (left fd) |> Alias.Set.is_empty |> not then
        failwith "Cannot remove a column defining other columns.";
      let newRight = Alias.Set.diff (right fd) cols in
      make (left fd) newRight in
    let t = map remove t in
    let keep fd = not (Alias.Set.is_empty (right fd)) in
    let t = filter keep t in
    t

  let key_fds ~keys ~cols =
    of_list [key_fd ~keys ~cols]

  (* Find the a functional dependency at the root, which is the functional dependency that defines all other nodes *)
  let root_fd fds =
    let res = filter (fun fd ->
      not (exists (fun fd2 ->
        Alias.Set.subset (right fd2) (left fd)) fds
      )
    ) fds in
    if is_empty res then
      None
    else
      Some(min_elt res)

  (* Get the functional dependency which defines the columns cols *)
  let defining_fd fds ~cols =
    let res = filter (fun fd ->
      Alias.Set.subset cols (right fd)
    ) fds in
    min_elt res

  let transitive_closure fds ~cols =
    let rec get = fun attrs fds ->
      let (newAttrs, fds) = partition (fun fd -> Alias.Set.subset (left fd) attrs) fds in
      let newAttrs = fold (fun fd c -> Alias.Set.union c (right fd)) newAttrs attrs in
      if Alias.Set.cardinal newAttrs > Alias.Set.cardinal attrs then
        get newAttrs fds
      else
        attrs in
    get cols fds

end

module Tree = struct
  type elt = Alias.Set.t [@@deriving show]
  type t = | FDNode of elt * (t list) [@@deriving show]

  let cols n =
    match n with
    | FDNode (cols, _) -> cols

  let subnodes n =
    match n with
    | FDNode (_, subnodes) -> subnodes

  let rec pp_pretty fmt v =
    Format.pp_force_newline fmt ();
    Format.pp_open_box fmt 2;
    Format.pp_print_string fmt "-";
    Format.pp_print_space fmt ();
    Alias.Set.pp_pretty fmt (cols v);
    List.iter (fun node -> pp_pretty fmt node) (subnodes v);
    Format.pp_close_box fmt ()

  let rec fd_subnodes fds fd =
    let subfds = Set.filter (fun fd2 ->
      Alias.Set.subset (right fd) (left fd2)
    ) fds in
    let remaining = Alias.Set.filter (fun col ->
      not (Set.exists (fun fd2 -> Alias.Set.mem col (left fd2)) subfds)) (right fd) in
    let subfds = List.map ~f:(fd_subnodes fds) (Set.elements subfds) in
    let subfds = if Alias.Set.is_empty remaining then subfds else FDNode (remaining, []) :: subfds in
      FDNode (left fd, subfds)

  let of_fds fds =
    let root = Set.root_fd fds in
    Option.map ~f:(fd_subnodes fds) root
end
