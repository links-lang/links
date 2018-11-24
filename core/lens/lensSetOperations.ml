open Value
open Utility

module Phrase = Lens_phrase

module SortedRecords = struct
    (* simplified record type drops column names for efficiency *)
    type simp_rec = Value.t list

    type recs = { columns: string list; plus_rows: simp_rec array; neg_rows: simp_rec array; }

    let is_positiv (t: recs) =
        Array.length t.neg_rows = 0

    let total_size (a : recs) =
        (Array.length a.plus_rows) + (Array.length a.neg_rows)

    let compare_val (a : Value.t) (b : Value.t) : int =
        match a, b with
        | `Bool b1, `Bool b2 -> compare b1 b2
        | `Char c1, `Char c2 -> compare c1 c2
        | `Float f1, `Float f2 -> compare f1 f2
        | `Int i1 , `Int i2 -> compare i1 i2
        | `String i1 , `String i2 -> compare i1 i2
        | _, _ -> failwith "Unsupported comparison types."

    let rec compare (a : simp_rec) (b : simp_rec) : int =
        match a, b with
        | x::xs, y::ys ->
            let res = compare_val x y in
            if res = 0 then compare xs ys else res
        | _,_ -> 0 (* if either of the lists are empty, return match
                    this allows us to perform partial matching *)

    let str_list_to_string lst =
        List.fold_left (fun a c -> if a = "" then c else a ^ ", " ^ c) "" lst

    let construct_cols (cols: string list) (records : Value.t) =
        let l = unbox_list records in
        let recs = List.map unbox_record l in
        let simpl_rec r = List.map2 (fun a (k,v) -> if a = k then v else
            begin
                try
                    let (_,v) = List.find (fun (k,_v) -> k = a) r in
                    v
                with NotFound _ -> failwith ("lens set columns not consistent: " ^ str_list_to_string cols ^ " != " ^ str_list_to_string (List.map (fun (k,_v) -> k) r))
            end
            ) cols r in
        let recs = Array.of_list (List.map simpl_rec recs) in
        Array.sort compare recs;
        { columns = cols; plus_rows = recs; neg_rows = Array.of_list []; }

    (* records: List of records *)
    let construct (records : Value.t) =
        let l = unbox_list records in
        let recs = List.map unbox_record l in
        let keys = List.map (fun (k,_v) -> k) (List.hd recs) in
        construct_cols keys records

    let to_string (rs : recs) =
        let cols = rs.columns in
        let row_to_string row =
            List.fold_left (fun a (k,v) ->
                let str = k ^ ": " ^ string_of_value v in
                let str = "(" ^ str ^ ")" in
                if a = "" then str else a ^ ", " ^ str) "" (List.combine cols row) in
        let pos = Array.fold_left (fun a row ->
            let str = row_to_string row in
            if a = "" then str else
                a ^ "\n" ^ str) "" rs.plus_rows in
        pos

    let to_string_tabular (rs : recs) =
        let pad a b = Pervasives.max 0 (a - b) in
        let cols = rs.columns in
        let cols = List.map (fun c ->
            c ^ (String.make (pad 8 (String.length c)) ' ')) cols in
        let row_to_string row =
            List.fold_left (fun a (k,v) ->
                let str = string_of_value v in
                let str = str ^ (String.make (pad (String.length k + 1) (String.length str)) ' ') in
                if a = "" then str else a ^ "| " ^ str) "" (List.combine cols row) in
        let cols_string = List.fold_left(fun a k ->
                if a = "" then k else a ^ " | " ^ k) "" cols in
        let sep = String.map (fun _ -> '-') cols_string in
        let pos = Array.fold_left (fun a row ->
            let str = row_to_string row in
            if a = "" then str else
                a ^ "\n" ^ str) (cols_string ^ "\n" ^ sep) rs.plus_rows in
        let posn = Array.fold_left (fun a row ->
            let str = row_to_string row in
            if a = "" then str else
                a ^ "\n" ^ str) (pos ^ "\n" ^ sep) rs.neg_rows in
        if Array.length rs.neg_rows > 0 then
            posn
        else
            pos

    let find_mul (rs : simp_rec array) (r : simp_rec) =
        let rec pivot s e =
            if s > e then
                None
            else
                let m = (s + e) / 2 in
                let r = compare (Array.get rs m) r in
                match r with
                | 0 -> Some r
                | a when a > 0 -> pivot s (m-1)
                | a when a < 0 -> pivot (m+1) e
                | _ -> failwith "impossible" in
        let r = pivot 0 ((Array.length rs) - 1) in
        r

    let find_rec (rs : simp_rec array) (r : simp_rec) =
        let ind = find_mul rs r in
        match ind with
        | None -> None
        | Some r -> Some (Array.get rs r)

    let find_all_ind (rs : simp_rec array) (r : simp_rec) : int * int =
        let rec pivot s e b =
            if s > e then
                if b then e else s
            else
                let m = (s + e) / 2 in
                let res = compare (Array.get rs m) r in
                match res with
                | 0 -> if b then pivot (m+1) e b else pivot s (m-1) b
                | a when a > 0 -> pivot s (m-1) b
                | a when a < 0 -> pivot (m+1) e b
                | _ -> failwith "impossible" in
        let b = pivot 0 ((Array.length rs) - 1) false in
        let e = pivot 0 ((Array.length rs) - 1) true in
        (b,e)

    let find_all (rs : simp_rec array) (r : simp_rec) : simp_rec array =
        let b,e = find_all_ind rs r in
        Array.sub rs b (e + 1 - b)


    let find (rs : recs) (r : simp_rec) : bool =
        find_mul rs.plus_rows r <> None

    let get_col_map_list (cols : string list) (col : string) =
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

    let get_col_map (rs : recs) (col : string) =
        let cols = rs.columns in
        get_col_map_list cols col


    let get_cols_map (rs : recs) (cols : string list) =
        let maps = List.map (fun col -> get_col_map rs col) cols in
        let maps = List.flatten (List.map (fun mp -> match mp with None -> [] | Some a -> [a]) maps) in
        (fun r -> List.map (fun mp -> mp r) maps)

    let sort (rs : recs) =
        Array.sort compare rs.plus_rows;
        Array.sort compare rs.neg_rows

    let sort_uniq (rs : recs) =
        let fn r = Array.of_list (List.sort_uniq compare (Array.to_list r)) in
        { columns = rs.columns; plus_rows = fn rs.plus_rows; neg_rows = fn rs.neg_rows }

    let project_onto (rs : recs) (cols : string list) =
        let maps = get_cols_map rs cols in
        let maps2 = get_cols_map rs cols in
        let plus_rows = Array.map maps rs.plus_rows in
        let neg_rows = Array.map maps rs.neg_rows in
        let cols = maps2 rs.columns in
        let sort_uniq arr = Array.of_list (List.sort_uniq compare (Array.to_list arr)) in
        { columns = cols; plus_rows = sort_uniq plus_rows; neg_rows = sort_uniq neg_rows }

    let project_onto_set (rs : recs) (rs2 : recs) =
        project_onto rs (rs2.columns)

    let not_neg_empty (rs : recs) =
        (rs.neg_rows <> Array.of_list [])

    let negate (rs : recs) =
        { columns = rs.columns; plus_rows = rs.neg_rows; neg_rows = rs.plus_rows }

    (* perform regular subtraction, not defined on sets with negative multiplicities *)
    let minus (rs1 : recs) (rs2 : recs) =
        if not_neg_empty rs2 then
            failwith "Cannot subtract from negative multiplicities"
        else
            let rec get_map col cols =
                match cols with
                | x::xs ->
                    if x = col then
                        Some (fun x -> List.hd x)
                    else
                        begin
                            let fn2 = get_map col xs in
                            match fn2 with
                            | Some fn2 -> Some (fun x -> fn2 (List.tl x))
                            | None -> None
                        end
                | _ -> None in
            let proj = project_onto_set rs2 rs1 in
            let maps = List.map (fun c -> match get_map c rs1.columns with None -> [] | Some a -> [a]) proj.columns in
            let maps = List.flatten maps in
            let rows = List.filter (fun r -> not (find proj (List.map (fun mp -> mp r) maps))) (Array.to_list rs1.plus_rows) in
            { rs1 with plus_rows = Array.of_list rows }



    let box_simp_rec (cols : string list) (vals : Value.t list) =
        box_record (List.combine cols vals)

    (* filter out the records which don't satisfy pred *)
    let filter (rs : recs) (pred : Types.lens_phrase) =
        let getv =
            get_col_map rs in
        let get_col_val row col = match (getv col) with
            | Some a -> a row
            | None -> failwith ("Column " ^ col ^ " not in record set.") in
        let filter rows = Array.of_list (List.filter (fun r -> Phrase.eval pred (get_col_val r) = box_bool true) (Array.to_list rows)) in
        {
            columns = rs.columns;
            plus_rows = filter rs.plus_rows;
            neg_rows = filter rs.neg_rows;
        }

    (* ensures that all columns in contains are in cols *)
    let cols_contain (cols : string list) (contains : string list) =
        List.for_all (fun a -> List.mem a cols) contains

    (* reorder cols, so first appears first *)
    (* this requires first to be a subset of cols *)
    let reorder_cols (cols : string list) (first : string list) =
        if not (cols_contain cols first) then
            failwith "Columns do not contain all reorder keys.";
        let rest = List.filter (fun a -> not (List.mem a first)) cols in
        List.append first rest

    let subtract_cols (cols : string list) (remove : string list) =
        List.filter (fun a -> not (List.mem a remove)) cols

    let zip_delta_merge (left : simp_rec list) (right : simp_rec list) =
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

    (* perform the delta set merge operation between two sets *)
    let merge (rs1 : recs) (rs2 : recs) =
        let proj = project_onto_set rs2 rs1 in
        let plus = List.append (Array.to_list rs1.plus_rows) (Array.to_list proj.plus_rows) in
        let plus = List.sort compare plus in
        let neg = List.append (Array.to_list rs1.neg_rows) (Array.to_list proj.neg_rows) in
        let neg = List.sort compare neg in
        let (plus, neg) = zip_delta_merge plus neg in
        { columns = rs1.columns; plus_rows = Array.of_list plus; neg_rows = Array.of_list neg }

        (*
        if rs1.columns <> proj.columns then
            failwith "cannot merge two different sets"
        else
            let rows_rs1 = List.filter (fun r -> None = find_mul proj.neg_rows r) (Array.to_list rs1.plus_rows) in
            let rows_rs1_n = List.filter (fun r -> None = find_mul rs1.plus_rows r) (Array.to_list proj.neg_rows) in
            let rows_rs2 = List.filter (fun r -> None = find_mul rs1.neg_rows r) (Array.to_list proj.plus_rows) in
            let rows_rs2_n = List.filter (fun r -> None = find_mul proj.plus_rows r) (Array.to_list rs1.neg_rows) in
            let rows_rs1 = Array.of_list rows_rs1 in
            let rows_rs1_n = Array.of_list rows_rs1_n in
            (* remove duplicate rows *)
            let rows_rs2 = List.filter (fun r ->  None = find_mul rows_rs1 r) rows_rs2 in
            let rows_rs2_n = List.filter (fun r -> None = find_mul rows_rs1_n r) rows_rs2_n in
            let rows = Array.append rows_rs1 (Array.of_list rows_rs2) in
            let rows_n = Array.append rows_rs1_n (Array.of_list rows_rs2_n) in
            { columns = rs1.columns; plus_rows = rows; neg_rows = rows_n } *)

    (* perform the join on left and right using on as the join key *)
    (* currently not specified what happens if this isn't the natural join *)
    let join (left : recs) (right : recs) (on : string list) =
        let right_cols = reorder_cols right.columns on in
        let right = project_onto right right_cols in
        let lmap = get_cols_map left on in
        let rjoinmap = get_cols_map right (subtract_cols right_cols on) in
        let rjoinmap2 = get_cols_map right (subtract_cols right_cols on) in
        let join_list l1 l2 =
            let joined = List.map (fun r1 ->
                let proj = lmap r1 in
                let matching = find_all l2 proj in
                let joined = List.map (fun r2 ->
                    List.append r1 (rjoinmap r2)) (Array.to_list matching) in
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
        { columns = List.append left.columns (rjoinmap2 right.columns);
          plus_rows = Array.of_list pos;
          neg_rows = Array.of_list neg; }

end
