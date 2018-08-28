open Utility

(* Lenses *)
module ColSet = struct
    include StringSet

    let pp_pretty fmt cs = 
        List.iter (fun c -> Format.pp_print_string fmt c; Format.pp_print_space fmt ()) (elements cs)
end

type colset = ColSet.t
      [@@deriving show]

module FunDep = struct 
    type t = colset * colset 
      [@@deriving show]

    let left (l,_) = l
    let right (_,r) = r 
    
    let compare (l1,r1) (l2, r2) = 
        let res = ColSet.compare l1 l2 in
        if res = 0 then
            ColSet.compare r1 r2
        else
            res

    let of_lists (left, right : string list * string list) : t =
        let left = ColSet.of_list left in
        let right = ColSet.of_list right in
        (left, right)

    let make left right : t = (left, right)

    let key_fd (keys : string list) (cols : string list) =
        let keys = ColSet.of_list keys in
        let cols = ColSet.of_list cols in
        let right = ColSet.diff cols keys in
        make keys right
end

type fundep = colset * colset 
        [@@deriving show]

module FunDepSet = struct
    include Set.Make(FunDep)
    
    let of_lists (fds : (string list * string list) list) : t =
        let fds = List.map FunDep.of_lists fds in
        of_list fds

    let remove_defines (fds : t) (cols : colset) =
        let remove fd = 
            if ColSet.inter cols (FunDep.left fd) |> ColSet.is_empty |> not then
                failwith "Cannot remove a column defining other columns.";
            let newRight = ColSet.diff (FunDep.right fd) cols in
            FunDep.make (FunDep.left fd) newRight in
        let fds = map remove fds in
        let keep fd = not (ColSet.is_empty (FunDep.right fd)) in
        let fds = filter keep fds in
        fds

    let key_fds left right =
        of_list [FunDep.key_fd left right]

    (* Find the a functional dependency at the root, which is the functional dependency that defines all other nodes *)
    let root_fd (fds : t) = 
        let res = filter (fun fd ->
            not (exists (fun fd2 ->
                ColSet.subset (FunDep.right fd2) (FunDep.left fd)) fds
            )
        ) fds in
        if is_empty res then
            None
        else
            Some(min_elt res)

    (* Get the functional dependency which defines the columns cols *)
    let defining_fd (cols : colset) (fds : t) =
        let res = filter (fun fd ->
            ColSet.subset cols (FunDep.right fd)    
        ) fds in
        min_elt res

    let transitive_closure (cols : colset) (fds : t) =
        let rec get = fun attrs fds ->
            let (newAttrs, fds) = partition (fun fd -> ColSet.subset (FunDep.left fd) attrs) fds in
            let newAttrs = fold (fun fd c -> ColSet.union c (FunDep.right fd)) newAttrs attrs in
            if ColSet.cardinal newAttrs > ColSet.cardinal attrs then
                get newAttrs fds
            else
                attrs in
        get cols fds

end

type fundepset = FunDepSet.t
        [@@deriving show]

module FunDepTree = struct  
    type t = [ `FDNode of colset * (t list) ]
    
    let cols (n : t) = 
        match n with
        | `FDNode (cols, _) -> cols

    let subnodes (n : t) =
        match n with
        | `FDNode (_, subnodes) -> subnodes 

    let rec pp_pretty (fmt : Format.formatter) (v : t) = 
        Format.pp_force_newline fmt ();
        Format.pp_open_box fmt 2;
        Format.pp_print_string fmt "-";
        Format.pp_print_space fmt ();
        ColSet.pp_pretty fmt (cols v);
        List.iter (fun node -> pp_pretty fmt node) (subnodes v);
        Format.pp_close_box fmt ()

    let rec fd_subnodes (fds : FunDepSet.t) (fd : FunDep.t) : t = 
        let subfds = FunDepSet.filter (fun fd2 ->
            ColSet.subset (FunDep.right fd) (FunDep.left fd2)
        ) fds in
        let remaining = ColSet.filter (fun col ->
            not (FunDepSet.exists (fun fd2 -> ColSet.mem col (FunDep.left fd2)) subfds)) (FunDep.right fd) in
        let subfds = List.map (fd_subnodes fds) (FunDepSet.elements subfds) in
        let subfds = if ColSet.is_empty remaining then subfds else `FDNode (remaining, []) :: subfds in
            `FDNode (FunDep.left fd, subfds)

    let of_fds (fds : fundepset) = 
        let root = FunDepSet.root_fd fds in
        OptionUtils.opt_map (fd_subnodes fds) root

end
