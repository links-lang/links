open Utility
open Lens_types
open Lens_utility

module Simple_record = struct
  (** simplified record type drops column names for efficiency *)
  type t = Value.t list

  let compare_val a b =
    match a, b with
    | `Bool b1, `Bool b2 -> compare b1 b2
    | `Char c1, `Char c2 -> compare c1 c2
    | `Float f1, `Float f2 -> compare f1 f2
    | `Int i1 , `Int i2 -> compare i1 i2
    | `String i1 , `String i2 -> compare i1 i2
    | _, _ -> failwith "Unsupported comparison types."


  let rec compare a b =
    match a, b with
    | x::xs, y::ys ->
      let res = compare_val x y in
      if res = 0 then compare xs ys else res
    | _,_ -> 0 (* if either of the lists are empty, return match
                  this allows us to perform partial matching *)

  let find_index rs ~record =
    let rec pivot s e =
      if s > e then
        None
      else
        let m = (s + e) / 2 in
        let r = compare (Array.get rs m) record in
        match r with
        | 0 -> Some r
        | a when a > 0 -> pivot s (m-1)
        | a when a < 0 -> pivot (m+1) e
        | _ -> failwith "impossible" in
    let r = pivot 0 ((Array.length rs) - 1) in
    r

  let find_record rs ~record =
    find_index rs ~record |> Option.map ~f:(Array.get rs)

  let find_all_index rs ~record =
    let rec pivot s e b =
      if s > e then
        if b then e else s
      else
        let m = (s + e) / 2 in
        let res = compare (Array.get rs m) record in
        match res with
        | 0 -> if b then pivot (m+1) e b else pivot s (m-1) b
        | a when a > 0 -> pivot s (m-1) b
        | a when a < 0 -> pivot (m+1) e b
        | _ -> failwith "impossible" in
    let b = pivot 0 ((Array.length rs) - 1) false in
    let e = pivot 0 ((Array.length rs) - 1) true in
    (b,e)

  let find_all_record rs ~record =
    let b,e = find_all_index rs ~record in
    Array.sub rs b (e + 1 - b)


  let to_value t ~columns =
    Value.box_record (List.combine columns t)
end

type t = { columns: string list; plus_rows: Simple_record.t array; neg_rows: Simple_record.t array; }


module Inconsistent_columns_error = struct
  exception E of string list * string list

  let () = Printexc.register_printer (
      function
      | E (l1, l2) ->
        Format.asprintf "Lens set columns inconsistent: %a != %a"
          (Format.pp_comma_list Format.pp_print_string) l1 (Format.pp_comma_list Format.pp_print_string) l2
        |> Option.return
      | _ -> None);;
end

let construct_cols ~cols ~records =
  let l = Value.unbox_list records in
  let recs = List.map Value.unbox_record l in
  let col_val a r = try
      let (_,v) = List.find (fun (k,_) -> k = a) r in
      v
    with NotFound _ -> Inconsistent_columns_error.E (cols, List.map (fun (k,_) -> k) r) |> raise in
  let simpl_rec r =
    List.map2 (fun a (k,v) -> if a = k then v else col_val a r) cols r in
  let recs = Array.of_list (List.map simpl_rec recs) in
  Array.sort compare recs;
  { columns = cols; plus_rows = recs; neg_rows = Array.of_list []; }

let construct ~records =
  let l = Value.unbox_list records in
  let recs = List.map Value.unbox_record l in
  let cols = List.map (fun (k,_v) -> k) (List.hd recs) in
  construct_cols ~cols ~records

let columns t = t.columns

let is_positive t =
  Array.length t.neg_rows = 0

let total_size a =
  (Array.length a.plus_rows) + (Array.length a.neg_rows)

let pp b rs =
  let cols = rs.columns in
  let pp_val b (k,v) = Format.fprintf b "%s: %a" k Value.pp v in
  let pp_record b row = Format.fprintf b "(%a)" (Format.pp_comma_list pp_val) (List.combine cols row) in
  Format.fprintf b "%a" (Format.pp_newline_list pp_record) (rs.plus_rows |> Array.to_list)

let pp_tabular f rs =
  let pp_sep f () = Format.pp_print_string f "| " in
  let pp_list pp f v = Format.pp_print_list ~pp_sep pp f v in
  let pp_row f v = pp_list Value.pp f v in
  let pp_rows f v = Format.pp_newline_list pp_row f @@ Array.to_list v in
  let pp_padded f v = Format.pp_padded_string ~length:8 f v in
  let pp_header f v = pp_list pp_padded f v in
  let pp_v_sep f () =
    let pp_sep f () = Format.pp_print_string f "--" in
    let pp_str f v = Format.pp_print_string f @@ String.make (String.length v) '-' in
    Format.pp_print_list ~pp_sep pp_str f rs.columns in
  Format.fprintf f "%a\n%a\n%a\n%a\n%a"
    pp_header rs.columns
    pp_v_sep ()
    pp_rows rs.plus_rows
    pp_v_sep ()
    pp_rows rs.plus_rows

let find rs ~record =
  Simple_record.find_index rs.plus_rows ~record |> Option.is_some

(** Construct a function which returns the nth value of a list, correspodning to the position of
    that column in cols *)
let get_col_map_list cols col =
  let rec fn cols =
    match cols with
    | x::xs ->
      if x = col then
        Some (fun x -> List.hd x)
      else
        begin
          let fn2 = fn xs in
          match fn2 with
          | Some fn2 -> Some (fun x -> fn2 (List.tl x))
          | None -> None
        end
    | _ -> None in
  fn cols

let get_col_map rs ~column =
  get_col_map_list rs.columns column

let get_cols_map rs ~columns =
  let maps = List.filter_map ~f:(fun column -> get_col_map rs ~column) columns in
  fun r -> List.map (fun mp -> mp r) maps

let sort rs =
  Array.sort Simple_record.compare rs.plus_rows;
  Array.sort Simple_record.compare rs.neg_rows

let sort_uniq rs =
  let fn r = Array.of_list (List.sort_uniq compare (Array.to_list r)) in
  { rs with plus_rows = fn rs.plus_rows; neg_rows = fn rs.neg_rows }

let project_onto rs ~columns =
  let maps v = get_cols_map rs ~columns v in
  let plus_rows = Array.map maps rs.plus_rows in
  let neg_rows = Array.map maps rs.neg_rows in
  let columns = maps rs.columns in
  sort_uniq { columns; plus_rows; neg_rows }

let project_onto_set rs ~onto =
  project_onto rs ~columns:onto.columns

let negate rs =
  { rs with plus_rows = rs.neg_rows; neg_rows = rs.plus_rows }

let minus rs1 rs2 =
  if is_positive rs2 |> not then
    failwith "Cannot subtract from negative multiplicities"
  else
    let proj = project_onto_set rs2 ~onto:rs1 in
    let map = get_cols_map rs1 ~columns:proj.columns in
    let plus_rows = List.filter (fun r -> find proj ~record:(map r)) @@ Array.to_list rs1.plus_rows
                    |> Array.of_list in
    { rs1 with plus_rows; }

(* filter out the records which don't satisfy pred *)
let filter rs ~predicate =
  let getv column =
    get_col_map rs ~column in
  let get_col_val row col = match (getv col) with
    | Some a -> a row
    | None -> failwith ("Column " ^ col ^ " not in record set.") in
  let filter rows = Array.of_list (List.filter (
      fun r -> Lens_phrase.eval predicate (get_col_val r) = Value.box_bool true)
      (Array.to_list rows)) in
  {
    columns = rs.columns;
    plus_rows = filter rs.plus_rows;
    neg_rows = filter rs.neg_rows;
  }

(* ensures that all columns in contains are in cols *)
let cols_contain cols contains =
  List.for_all (fun a -> List.mem a cols) contains


let zip_delta_merge left right =
  let rec do_next left right =
    match left, right with
    | x :: xs, y :: ys ->
      begin
        match compare x y with
        | 0 -> (* x = y, so skip both *) do_next xs ys
        | a when a < 0 -> (* x < y, so take x and see if can find y *)
          let (left,right) = do_next xs right in
          (x :: left, right)
        | a when a > 0 -> (* x > y, so take y and try find x *)
          let (left, right) = do_next left ys in
          (left, y :: right)
        | _ -> failwith "impossible"
      end
    | _ -> (left, right) (* one of them is empty so return rest *)
  in
  let (left, right) = do_next left right in
  (List.sort_uniq compare left, List.sort_uniq compare right)

let merge rs1 rs2 =
  let proj = project_onto_set rs2 ~onto:rs1 in
  let plus = List.append (Array.to_list rs1.plus_rows) (Array.to_list proj.plus_rows) in
  let plus = List.sort compare plus in
  let neg = List.append (Array.to_list rs1.neg_rows) (Array.to_list proj.neg_rows) in
  let neg = List.sort compare neg in
  let (plus, neg) = zip_delta_merge plus neg in
  { columns = rs1.columns; plus_rows = Array.of_list plus; neg_rows = Array.of_list neg }

let reorder_cols cols first =
  if not (cols_contain cols first) then
    failwith "Columns do not contain all reorder keys.";
  let rest = List.filter (fun a -> not (List.mem a first)) cols in
  List.append first rest

let subtract_cols cols remove =
  List.filter (fun a -> not (List.mem a remove)) cols

let join left right ~on =
  let on_left, on_right, on_out = List.unzip3 on in
  let right_cols = reorder_cols right.columns on_right in
  let right = project_onto right ~columns:right_cols in
  let lmap = get_cols_map left ~columns:on_left in
  let rjoinmap_left = get_cols_map left ~columns:(subtract_cols left.columns on_left) in
  let rjoinmap_left' = get_cols_map left ~columns:(subtract_cols left.columns on_left) in
  let rjoinmap_right = get_cols_map right ~columns:(subtract_cols right_cols on_right) in
  let rjoinmap_right' = get_cols_map right ~columns:(subtract_cols right_cols on_right) in
  let join_list l1 l2 =
    let joined = List.map (fun r1 ->
        let proj = lmap r1 in
        let matching = Simple_record.find_all_record l2 ~record:proj in
        let joined = List.map (fun r2 ->
            List.flatten [rjoinmap_left r1; proj; rjoinmap_right r2])
            (Array.to_list matching) in
        joined
      ) (Array.to_list l1) in
    List.flatten joined in
  let pos = join_list left.plus_rows right.plus_rows
  in
  let pos = List.sort compare pos in
  let neg = List.flatten [
      join_list left.plus_rows right.neg_rows;
      join_list left.neg_rows right.plus_rows;
      join_list left.neg_rows right.neg_rows
    ] in
  let neg = List.sort compare neg in
  let (pos, neg) = zip_delta_merge pos neg in
  let columns = List.flatten [rjoinmap_left' left.columns; on_out; rjoinmap_right' right_cols] in
  { columns;
    plus_rows = Array.of_list pos;
    neg_rows = Array.of_list neg; }

let to_value ts =
  if is_positive ts then
    Array.map (Simple_record.to_value ~columns:ts.columns) ts.plus_rows
    |> Array.to_list
    |> Value.box_list
  else failwith "Cannot convert a non positive delta to a value type."

let project_fun_dep ts ~fun_dep =
  let fdl, fdr = Fun_dep.left fun_dep, Fun_dep.right fun_dep in
  let cols_l = Alias.Set.elements fdl in
  let cols_r = Alias.Set.elements fdr in
  let fdl_map = get_cols_map ts ~columns:cols_l in
  let fdr_map = get_cols_map ts ~columns:cols_r in
  let map r = fdl_map r, fdr_map r in
  (cols_l, cols_r), Array.map map ts.plus_rows
