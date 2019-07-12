open Lens_utility
open Lens_utility.O
module Value = Phrase_value

module Simple_record = struct
  (** simplified record type drops column names for efficiency *)
  type t = Value.t list

  let compare_val a b =
    match (a, b) with
    | Value.Bool b1, Value.Bool b2 -> compare b1 b2
    | Value.Char c1, Value.Char c2 -> compare c1 c2
    | Value.Float f1, Value.Float f2 -> compare f1 f2
    | Value.Int i1, Value.Int i2 -> compare i1 i2
    | Value.String i1, Value.String i2 -> compare i1 i2
    | _, _ -> failwith "Unsupported comparison types."

  let equal_val a b = compare_val a b = 0

  let rec compare a b =
    match (a, b) with
    | x :: xs, y :: ys ->
        let res = compare_val x y in
        if res = 0 then compare xs ys else res
    | _, _ -> 0

  (* if either of the lists are empty, return match
                  this allows us to perform partial matching *)

  let find_index rs ~record =
    let rec pivot s e =
      if s > e then None
      else
        let m = (s + e) / 2 in
        let r = compare rs.(m) record in
        match r with
        | a when a > 0 -> pivot s (m - 1)
        | a when a < 0 -> pivot (m + 1) e
        | _ -> Some r
    in
    let r = pivot 0 (Array.length rs - 1) in
    r

  let find_record rs ~record =
    find_index rs ~record |> Option.map ~f:(Array.get rs)

  let find_all_index rs ~record =
    let rec pivot s e b =
      if s > e then if b then e else s
      else
        let m = (s + e) / 2 in
        let res = compare rs.(m) record in
        match res with
        | a when a > 0 -> pivot s (m - 1) b
        | a when a < 0 -> pivot (m + 1) e b
        | _ -> if b then pivot (m + 1) e b else pivot s (m - 1) b
    in
    let b = pivot 0 (Array.length rs - 1) false in
    let e = pivot 0 (Array.length rs - 1) true in
    (b, e)

  let find_all_record rs ~record =
    let b, e = find_all_index rs ~record in
    Array.sub rs b (e + 1 - b)

  let to_value t ~columns = Value.box_record (List.zip_exn columns t)

  let equal v1 v2 = List.for_all2 equal_val v1 v2
end

type t =
  { columns: string list
  ; plus_rows: Simple_record.t array
  ; neg_rows: Simple_record.t array }

module Inconsistent_columns_error = struct
  exception E of string list * string list

  let () =
    Printexc.register_printer (function
      | E (l1, l2) ->
          Format.asprintf "Lens set columns inconsistent: %a != %a"
            (Format.pp_comma_list Format.pp_print_string)
            l1
            (Format.pp_comma_list Format.pp_print_string)
            l2
          |> Option.return
      | _ -> None)
end

let construct_cols ~columns ~records =
  let recs = List.map ~f:Value.unbox_record records in
  let col_val a r =
    try List.find_exn ~f:(fst >> ( = ) a) r |> snd
    with _ ->
      Inconsistent_columns_error.E (columns, List.map ~f:fst r) |> raise
  in
  let simpl_rec r =
    List.map2 (fun a (k, v) -> if a = k then v else col_val a r) columns r
  in
  let plus_rows = Array.of_list (List.map ~f:simpl_rec recs) in
  Array.sort compare plus_rows ;
  {columns; plus_rows; neg_rows= [||]}

let construct ~records =
  let recs = List.map ~f:Value.unbox_record records in
  let columns = List.map ~f:(fun (k, _v) -> k) (List.hd recs) in
  construct_cols ~columns ~records

let sort rs =
  Array.sort Simple_record.compare rs.plus_rows ;
  Array.sort Simple_record.compare rs.neg_rows

let construct_full ~columns ~plus ~neg =
  let v =
    {columns; plus_rows= Array.of_list plus; neg_rows= Array.of_list neg}
  in
  sort v ; v

let columns t = t.columns

let plus_rows t = t.plus_rows

let neg_rows t = t.neg_rows

let is_positive t = Array.length t.neg_rows = 0

let total_size a = Array.length a.plus_rows + Array.length a.neg_rows

let pp_value f v =
  match v with
  | Value.String s -> Format.fprintf f "\"%s\"" s
  | Value.Int i -> Format.fprintf f "%d" i
  | Value.Float v -> Format.fprintf f "%f" v
  | Value.Bool b -> Format.fprintf f "%b" b
  | Value.Char c -> Format.fprintf f "%c" c
  | _ -> Value.pp f v

let pp b rs =
  let cols = rs.columns in
  let pp_val b (k, v) = Format.fprintf b "%s: %a" k pp_value v in
  let pp_record b row =
    Format.fprintf b "(%a)"
      (Format.pp_comma_list pp_val)
      (List.zip_exn cols row)
  in
  Format.fprintf b "%a"
    (Format.pp_newline_list pp_record)
    (rs.plus_rows |> Array.to_list)

let pp_tabular f rs =
  let pp_sep f () = Format.pp_print_string f "| " in
  let pp_list pp f v = Format.pp_print_list ~pp_sep pp f v in
  let pp_val f v = Format.pp_padded ~length:8 pp_value f v in
  let pp_row f v = pp_list pp_val f v in
  let pp_rows f v = Format.pp_newline_list pp_row f @@ Array.to_list v in
  let pp_padded f v = Format.pp_padded ~length:8 f v in
  let pp_header f v = pp_list (pp_padded Format.pp_print_string) f v in
  let pp_v_sep f () =
    let pp_sep f () = Format.pp_print_string f "--" in
    let pp_str f v =
      Format.pp_print_string f @@ String.make (String.length v) '-'
    in
    Format.pp_print_list ~pp_sep pp_str f rs.columns
  in
  Format.fprintf f "%a\n%a\n%a\n%a\n%a" pp_header rs.columns pp_v_sep ()
    pp_rows rs.plus_rows pp_v_sep () pp_rows rs.neg_rows

let find rs ~record =
  Simple_record.find_index rs.plus_rows ~record |> Option.is_some

(** Construct a function which returns the nth value of a list, correspodning to the position of
    that column in cols *)
let get_col_map_list cols col =
  let rec fn cols =
    match cols with
    | x :: xs -> (
        if x = col then Some (fun x -> List.hd x)
        else
          let fn2 = fn xs in
          match fn2 with
          | Some fn2 -> Some (fun x -> fn2 (List.tl x))
          | None -> None )
    | _ -> None
  in
  fn cols

let get_col_map rs ~column = get_col_map_list rs.columns column

let get_cols_map rs ~columns =
  let maps =
    List.filter_map ~f:(fun column -> get_col_map rs ~column) columns
  in
  fun r -> List.map ~f:(fun mp -> mp r) maps

let sort_uniq rs =
  let fn r = Array.of_list (List.sort_uniq compare (Array.to_list r)) in
  {rs with plus_rows= fn rs.plus_rows; neg_rows= fn rs.neg_rows}

let negate rs = {rs with plus_rows= rs.neg_rows; neg_rows= rs.plus_rows}

let negative rs = {rs with plus_rows= [||]}

(* filter out the records which don't satisfy pred *)
let filter rs ~predicate =
  let getv column = get_col_map rs ~column in
  let get_col_val row col =
    match getv col with
    | Some a -> a row
    | None -> failwith ("Column " ^ col ^ " not in record set.")
  in
  let filter rows =
    Array.of_list
      (List.filter
         (fun r -> Phrase.eval predicate (get_col_val r) = Value.box_bool true)
         (Array.to_list rows))
  in
  { columns= rs.columns
  ; plus_rows= filter rs.plus_rows
  ; neg_rows= filter rs.neg_rows }

(* ensures that all columns in contains are in cols *)
let cols_contain cols contains =
  List.for_all ~f:(List.mem ~equal:String.equal cols) contains

let zip_delta_merge left right =
  let rec do_next left right =
    match (left, right) with
    | x :: xs, y :: ys -> (
      match compare x y with
      | a when a < 0 ->
          (* x < y, so take x and see if can find y *)
          let left, right = do_next xs right in
          (x :: left, right)
      | a when a > 0 ->
          (* x > y, so take y and try find x *)
          let left, right = do_next left ys in
          (left, y :: right)
      | _ -> (* x = y, so skip both *) do_next xs ys )
    | _ -> (left, right)
    (* one of them is empty so return rest *)
  in
  let left, right = do_next left right in
  (List.sort_uniq compare left, List.sort_uniq compare right)

let project_onto rs ~columns =
  let maps v = get_cols_map rs ~columns v in
  let plus_rows = Array.map maps rs.plus_rows |> Array.to_list in
  let neg_rows = Array.map maps rs.neg_rows |> Array.to_list in
  let columns = maps rs.columns in
  let plus_rows, neg_rows = zip_delta_merge plus_rows neg_rows in
  let plus_rows, neg_rows =
    (Array.of_list plus_rows, Array.of_list neg_rows)
  in
  {columns; plus_rows; neg_rows}

let project_onto_set rs ~onto = project_onto rs ~columns:onto.columns

let merge rs1 rs2 =
  let proj = project_onto_set rs2 ~onto:rs1 in
  let plus =
    List.append (Array.to_list rs1.plus_rows) (Array.to_list proj.plus_rows)
  in
  let plus = List.sort compare plus in
  let neg =
    List.append (Array.to_list rs1.neg_rows) (Array.to_list proj.neg_rows)
  in
  let neg = List.sort compare neg in
  let plus, neg = zip_delta_merge plus neg in
  { columns= rs1.columns
  ; plus_rows= Array.of_list plus
  ; neg_rows= Array.of_list neg }

let minus rs1 rs2 =
  if is_positive rs2 |> not then
    failwith "Cannot subtract from negative multiplicities"
  else
    let proj = project_onto_set rs2 ~onto:rs1 in
    let map = get_cols_map rs1 ~columns:proj.columns in
    let plus_rows =
      List.filter (fun r -> find proj ~record:(map r))
      @@ Array.to_list rs1.plus_rows
      |> Array.of_list
    in
    {rs1 with plus_rows}

let reorder_cols cols ~first =
  if not (cols_contain cols first) then
    failwith "Columns do not contain all reorder keys." ;
  let rest =
    List.filter (fun a -> not (List.mem ~equal:String.equal first a)) cols
  in
  List.append first rest

let reorder t ~first =
  let columns = reorder_cols (columns t) ~first in
  project_onto t ~columns

let subtract_cols cols remove =
  List.filter (fun a -> not (List.mem ~equal:String.equal remove a)) cols

let join left right ~on =
  let on_out, on_left, on_right = List.unzip3 on in
  let right_cols = reorder_cols right.columns ~first:on_right in
  let right = project_onto right ~columns:right_cols in
  let lmap = get_cols_map left ~columns:on_left in
  let rjoinmap_right =
    get_cols_map right ~columns:(subtract_cols right_cols on_right)
  in
  let rjoinmap_right' =
    get_cols_map right ~columns:(subtract_cols right_cols on_right)
  in
  let join_list l1 l2 =
    let joined =
      List.map
        ~f:(fun r1 ->
          let proj = lmap r1 in
          let matching = Simple_record.find_all_record l2 ~record:proj in
          let joined =
            List.map
              ~f:(fun r2 -> List.flatten [r1; rjoinmap_right r2])
              (Array.to_list matching)
          in
          joined)
        (Array.to_list l1)
    in
    List.flatten joined
  in
  let pos = join_list left.plus_rows right.plus_rows in
  let pos = List.sort compare pos in
  let neg =
    List.flatten
      [ join_list left.plus_rows right.neg_rows
      ; join_list left.neg_rows right.plus_rows
      ; join_list left.neg_rows right.neg_rows ]
  in
  let neg = List.sort compare neg in
  let pos, neg = zip_delta_merge pos neg in
  let l_map = String.Map.from_alist (List.zip_nofail on_left on_out) in
  let l_cols =
    List.map
      ~f:(fun v -> String.Map.find_opt v l_map |> Option.value ~default:v)
      left.columns
  in
  let columns = List.flatten [l_cols; rjoinmap_right' right_cols] in
  {columns; plus_rows= Array.of_list pos; neg_rows= Array.of_list neg}

let to_value ts =
  if is_positive ts then
    Array.map (Simple_record.to_value ~columns:ts.columns) ts.plus_rows
    |> Array.to_list
  else failwith "Cannot convert a non positive delta to a value type."

let project_fun_dep ts ~fun_dep =
  let fdl, fdr = (Fun_dep.left fun_dep, Fun_dep.right fun_dep) in
  let cols_l = Alias.Set.elements fdl in
  let cols_r = Alias.Set.elements fdr in
  let fdl_map = get_cols_map ts ~columns:cols_l in
  let fdr_map = get_cols_map ts ~columns:cols_r in
  let map r = (fdl_map r, fdr_map r) in
  ((cols_l, cols_r), Array.map map ts.plus_rows, Array.map map ts.neg_rows)

let calculate_fd_changelist data ~fun_deps =
  (* get the key of the row for finding complements *)
  let rec loop fds =
    if Fun_dep.Set.is_empty fds then []
    else
      let fun_dep = Fun_dep.Set.root_fds fds |> fun v -> List.hd v in
      let cols, changeset_pos, _ = project_fun_dep data ~fun_dep in
      let changeset = Array.to_list changeset_pos in
      (* remove duplicates and sort *)
      let changeset =
        List.sort_uniq
          (fun (a, _) (a', _) -> Simple_record.compare a a')
          changeset
      in
      let fds = Fun_dep.Set.remove fun_dep fds in
      (cols, changeset) :: loop fds
  in
  let res = loop fun_deps in
  (* reverse the list, so that the FD roots appear first *)
  List.rev res

let relational_update t ~fun_deps ~update_with =
  let changelist = calculate_fd_changelist ~fun_deps update_with in
  let changes =
    List.map
      ~f:(fun ((cols_l, _cols_r), _l) ->
        (* get a map from simp rec to col value *)
        let col_maps =
          List.map ~f:(fun column -> get_col_map t ~column) cols_l
        in
        let col_maps =
          List.flatten
            (List.map
               ~f:(fun mp ->
                 match mp with
                 | None -> []
                 | Some a -> [a])
               col_maps)
        in
        (* get a function which compares column with change *)
        let comp record change_key =
          List.for_all2 (fun mp1 key -> mp1 record = key) col_maps change_key
        in
        comp)
      changelist
  in
  (* each entry in changelist is a functional dependency, and then the corresponding records *)
  (* generate a function for every change list entry, which can replace the columns in the
   * right side of the functional dependency of a record in res *)
  let apply_changes =
    List.map
      ~f:(fun ((_cols_l, cols_r), _l) ->
        (* upd cols returns a function which, given a record another record containing cols_r,
       * replaces every column value in the first record from that in the second record if it
       * matches *)
        let rec upd cols =
          match cols with
          | [] -> fun _r _target -> []
          | x :: xs -> (
              let fn = upd xs in
              (* get a function which maps a row to the x's value *)
              let map = get_col_map_list cols_r x in
              match map with
              | None ->
                  (* the column does not have to be replaced *)
                  fun yl target -> List.hd yl :: fn (List.tl yl) target
              | Some mp ->
                  (* the column has been found, replace *)
                  fun yl target -> mp target :: fn (List.tl yl) target )
        in
        columns t |> upd)
      changelist
  in
  let update arr =
    Array.map
      (fun r ->
        let r' =
          List.fold_left
            (fun r ((check, update), (_, changes)) ->
              let upd =
                List.find ~f:(fun (left, _right) -> check r left) changes
              in
              match upd with
              | None -> r
              | Some (_left, right) -> update r right)
            r
            (List.zip_exn (List.zip_exn changes apply_changes) changelist)
        in
        r')
      arr
  in
  let plus_rows = update t.plus_rows in
  let neg_rows = [||] in
  let res = {t with neg_rows; plus_rows} in
  sort_uniq res

let abs t =
  let plus_rows = t.plus_rows in
  let columns = t.columns in
  {plus_rows; columns; neg_rows= [||]}

let relational_merge t ~fun_deps ~update_with =
  let updated = relational_update t ~fun_deps ~update_with in
  merge updated @@ abs update_with

let relational_extend t ~key ~by ~data ~default =
  let colmap = get_cols_map t ~columns:[key] in
  let data = reorder data ~first:[key] in
  let relevant_value_map = Option.value_exn (get_col_map data ~column:by) in
  let extend row =
    let find = colmap row in
    let rel = Simple_record.find_record data.plus_rows ~record:find in
    let v =
      match rel with
      | None -> default
      | Some r -> relevant_value_map r
    in
    List.append row [v]
  in
  let plus_rows = Array.map extend t.plus_rows in
  let neg_rows = Array.map extend t.neg_rows in
  let columns = List.append t.columns [by] in
  {columns; plus_rows; neg_rows}

let all_values t =
  let recs =
    List.append (Array.to_list t.plus_rows) (Array.to_list t.neg_rows)
  in
  List.sort_uniq Simple_record.compare recs

let to_diff t ~key =
  let key_len = List.length key in
  let t = reorder t ~first:key in
  let insert_vals, update_vals =
    List.partition
      (fun row ->
        let key_vals = List.take row ~n:key_len in
        let row = Simple_record.find_index t.neg_rows ~record:key_vals in
        Option.is_none row)
      (Array.to_list t.plus_rows)
  in
  let delete_vals =
    Array.to_list t.neg_rows
    |> List.filter (fun row ->
           let key_vals = List.take row ~n:key_len in
           let row = Simple_record.find_index t.plus_rows ~record:key_vals in
           Option.is_none row)
  in
  (columns t, (insert_vals, update_vals, delete_vals))

let force_positive t =
  let t =
    {t with plus_rows= Array.append t.plus_rows t.neg_rows; neg_rows= [||]}
  in
  sort_uniq t
