open Value
open Lens_utility
open Lens_utility.O
module Sorted = Sorted_records

type env = int Int.Map.t

let matches_change changes =
  let is_changed ((cols_l, _cols_r), vals) =
    let vals_l = List.map ~f:(fun (left, _) -> left) (Array.to_list vals) in
    Phrase.Option.in_expr cols_l vals_l
  in
  List.map ~f:is_changed changes |> Phrase.List.fold_or_opt

let delta_merge_affected ~db lens data =
  let sort = Value.sort lens in
  let fun_deps = Sort.fds sort in
  let changelist = Sorted.calculate_fd_changelist data ~fun_deps in
  (* query relevant rows in database *)
  let predicate = matches_change changelist in
  let res = Value.lens_get_select_opt ~db lens ~predicate in
  let res =
    Sorted.construct_cols
      ~columns:(Value.cols_present_aliases lens)
      ~records:res
  in
  (* perform relational update *)
  let rel_merge =
    Sorted_records.relational_merge res ~update_with:data ~fun_deps
  in
  Sorted.merge rel_merge (Sorted.negate res)

let query_join_records ~db lens set on =
  let proj = Sorted.project_onto set ~columns:on in
  let recs = Sorted.all_values proj in
  let predicate = Phrase.Option.in_expr on recs in
  let records = Value.lens_get_select_opt ~db lens ~predicate in
  Sorted.construct_cols ~columns:(Value.cols_present_aliases lens) ~records

let query_project_records ~db lens set key drop =
  let set = Sorted.force_positive set in
  let proj = Sorted.project_onto set ~columns:key in
  let recs = Sorted.all_values proj in
  let predicate = Phrase.Option.in_expr key recs in
  let records = Value.lens_get_select_opt ~db lens ~predicate in
  let records =
    Sorted.construct_cols ~columns:(Value.cols_present_aliases lens) ~records
  in
  Sorted.project_onto records ~columns:(List.append key drop)

let matches_any predicate s =
  let s = Sorted.filter s ~predicate in
  Sorted.plus_rows s |> Array.length > 0

let lens_put_set_step ~db ~env lens delt
    (fn : env:env -> Value.t -> Sorted.t -> env) =
  match lens with
  | Lens _ -> fn ~env lens delt
  | LensDrop { lens = l; drop; key; default; _ } ->
      let relevant = query_project_records ~db l delt [ key ] [ drop ] in
      let delt =
        Sorted.relational_extend_exn delt ~key ~by:drop ~default ~data:relevant
      in
      fn ~env l delt
  | LensJoin { left; right; on; del_left; del_right; _ } ->
      let cols_simp = List.map ~f:(fun (a, _, _) -> a) on in
      let sort1 = Value.sort left in
      let proj1 =
        Sorted.project_onto delt ~columns:(Sort.cols_present_aliases sort1)
      in
      let sort2 = Value.sort right in
      let proj2 =
        Sorted.project_onto delt ~columns:(Sort.cols_present_aliases sort2)
      in
      let delta_m0 = delta_merge_affected ~db left proj1 in
      let delta_n0 = delta_merge_affected ~db right proj2 in
      let on' = List.map ~f:(fun a -> (a, a, a)) cols_simp in
      let delta_l =
        Sorted.merge
          (Sorted.merge
             (Sorted.join_exn delta_m0 delta_n0 ~on:on')
             (Sorted.merge
                (Sorted.join_exn delta_m0
                   (query_join_records ~db right delta_m0 cols_simp)
                   ~on:on')
                (Sorted.join_exn delta_n0
                   (query_join_records ~db left delta_n0 cols_simp)
                   ~on:on')))
          (Sorted.negate delt)
      in
      let j =
        if matches_any del_right delta_l then
          Sorted.project_onto
            (Sorted.merge (query_join_records ~db lens delta_l cols_simp) delt)
            ~columns:cols_simp
        else Sorted.construct_cols ~columns:cols_simp ~records:[]
      in
      let delta_l_l = Sorted.join_exn delta_l j ~on:on' in
      let delta_l_a = Sorted.merge delta_l (Sorted.negate delta_l_l) in
      let delta_m =
        Sorted.merge
          (Sorted.merge delta_m0
             (Sorted.negate
                (Sorted.project_onto_set
                   (Sorted.filter delta_l_a ~predicate:del_left)
                   ~onto:delta_m0)))
          (Sorted.negate (Sorted.project_onto_set delta_l_l ~onto:delta_m0))
      in
      let delta_n =
        Sorted.merge delta_n0
          (Sorted.negate
             (Sorted.project_onto_set
                (Sorted.filter delta_l_a ~predicate:del_right)
                ~onto:delta_n0))
      in
      let env = fn ~env right delta_n in
      fn ~env left delta_m
  | LensSelect { lens; predicate; _ } ->
      let delta_m1 =
        Sorted.merge
          (delta_merge_affected ~db
             (Value.lens_select_internal lens ~predicate:(Phrase.not' predicate))
             delt)
          (Sorted.negative delt)
      in
      let m1_cap_P = Sorted.filter delta_m1 ~predicate in
      let delta_nhash = Sorted.merge m1_cap_P (Sorted.negate delt) in
      let new_delta = Sorted.merge delta_m1 (Sorted.negate delta_nhash) in
      fn ~env lens new_delta
  | _ -> failwith "Unsupport lens."

let lens_get_delta ~db lens data =
  let columns = Value.cols_present_aliases lens in
  let orig =
    Sorted.construct_cols ~columns ~records:(Value.lens_get ~db lens)
  in
  let data =
    Sorted.merge
      (Sorted.construct_cols ~columns ~records:data)
      (Sorted.negate orig)
  in
  data

let lens_put_step ~db lens data (fn : env:env -> Value.t -> Sorted.t -> env) =
  let data = lens_get_delta ~db lens data in
  lens_put_set_step ~db lens data fn

let rec take (l : 'a list) (n : int) =
  match n with
  | 0 -> []
  | _ -> List.hd l :: take (List.tl l) (n - 1)

let rec skip (l : 'a list) (n : int) =
  match n with
  | 0 -> l
  | _ -> skip (List.tl l) (n - 1)

module OrderedBoolList = struct
  type t = bool list [@@deriving show]

  let rec compare b1 b2 =
    match (b1, b2) with
    | x :: xs, y :: ys ->
        if x = y then compare xs ys else if not x then -1 else 1
    | [], [] -> 0
    | _, [] -> 1
    | [], _ -> -1
end

module MapBoolList = Lens_map.Make (OrderedBoolList)

let apply_delta ~table ~db ~sort ~env data =
  let { Database.Table.name = table; keys } = table in
  (* get the first key, otherwise return an empty key *)
  let key =
    match keys with
    | [] -> Sorted.columns data
    | _ -> List.hd keys
  in
  let columns, (insert_vals, update_vals, delete_vals) =
    Sorted.to_diff_exn data ~key
  in
  let map_env vals =
    let open Phrase_value in
    List.map
      ~f:(fun r ->
        List.map
          ~f:(fun v ->
            match v with
            | Serial (`NewKeyMapped key) -> (
                match Int.Map.find ~key env with
                | Some key -> Serial (`Key key)
                | None -> v)
            | _ -> v)
          r)
      vals
  in
  let insert_vals = map_env insert_vals in
  let update_vals = map_env update_vals in
  let delete_vals = map_env delete_vals in

  (* when inserting the data we need to split the to be inserted rows into
     different batches, where each batch has the same serial column insert
     pattern. Example: given a table

      A | B
     ---|---
        |

     If both columns A and B are serial columns, then we split into four batches
     of insert rows:
       - (1) where both A and B contain existing values,
       - (2) A should be a new value but B is an existing value,
       - (3) B should be a new value but A is a known value, and
       - (4) A and B should both be new values. *)
  (* generate a list of serial columns and each column index *)
  let scolumns =
    Sort.cols sort
    |> List.map ~f:(fun v ->
           ( List.findi columns ~f:(( = ) (Column.alias v))
             |> Option.map ~f:fst
             |> Option.value ~default:0,
             v ))
    |> List.filter (snd >> Column.typ >> Phrase_type.equal Phrase_type.Serial)
  in
  (* Pattern for records with no new keys. *)
  let special_false = scolumns |> List.map ~f:(fun _ -> false) in
  let insert_val_batches =
    insert_vals
    |> List.map ~f:(fun row ->
           ( scolumns
             |> List.map ~f:(fun (i, _) ->
                    List.nth row i |> Phrase_value.is_new_key),
             row ))
    |> List.groupBy (module MapBoolList) ~f:(fun (k, _) -> k)
    |> List.map ~f:(fun (k, v) -> (k, v |> List.map ~f:(fun (_, v) -> v)))
  in
  (* use regular insert for all insert values where no new keys are generated. *)
  let insert_vals =
    insert_val_batches
    |> List.find ~f:(fun (k, _) -> k = special_false)
    |> Option.map ~f:(fun (_, v) -> v)
    |> Option.value ~default:[]
  in
  let insert_returning =
    insert_val_batches |> List.filter (fun (k, _) -> k != special_false)
  in
  let prepare_where row =
    List.zip_nofail key row
    |> List.map ~f:(fun (k, v) ->
           Phrase.equal (Phrase.var k) (Phrase.Constant.of_value v))
    |> Phrase.List.fold_and
  in
  (* Format and execute commands that don't require returning output. *)
  let insert_cmd values =
    let values = [ values ] in
    let returning = [] in
    Database.Change.Insert { Database.Insert.table; columns; values; returning }
  in
  let delete_cmd row =
    let predicate = prepare_where row in
    Database.Change.Delete { Database.Delete.table; predicate }
  in
  let update_cmd row =
    let predicate = prepare_where row in
    let set = List.zip_exn columns row |> List.drop ~n:(List.length key) in
    Database.Change.Update { Database.Update.table; predicate; set }
  in
  let cmds =
    List.concat
      [
        List.map ~f:insert_cmd insert_vals;
        List.map ~f:delete_cmd delete_vals;
        List.map ~f:update_cmd update_vals;
      ]
  in
  Database.Change.exec_multi ~db cmds;
  (* generate commands where we need  the returning id *)
  List.fold_right
    (fun (k, values_all) env ->
      let returning_cols =
        List.zip_nofail scolumns k
        |> List.filter (fun (_, u) -> u)
        |> List.map ~f:(fun (c, _) -> c)
      in
      let returning =
        returning_cols |> List.map ~f:(fun c -> snd c |> Column.name)
      in
      let returnings = Lens_string.Set.of_list returning in
      let values =
        values_all |> List.map ~f:(List.filter (Phrase_value.is_new_key >> not))
      in
      let columns =
        columns |> List.filter (fun v -> String.Set.mem v returnings |> not)
      in
      let insert = { Database.Insert.table; columns; values; returning } in
      let field_types =
        returning_cols
        |> List.map ~f:(fun (_, c) -> (Column.name c, Column.typ c))
      in
      let res =
        Database.Insert.exec_insert_returning ~db ~field_types insert
        |> List.map ~f:(fun v ->
               Phrase_value.unbox_record v |> List.map ~f:(fun (_, v) -> v))
      in
      let env =
        List.fold_right2
          (fun r1 r2 acc ->
            List.fold_right2
              (fun (i, _) c2 acc ->
                let c1 = List.nth r1 i in
                let c1 = Phrase_value.unbox_serial_newkeymapped c1 in
                let c2 = Phrase_value.unbox_serial_key c2 in
                Int.Map.add c1 c2 acc)
              returning_cols r2 acc)
          values_all res env
      in
      env)
    insert_returning env

let get_fds (fds : (string list * string list) list) (cols : Column.t list) :
    Fun_dep.Set.t =
  let check_col xs =
    List.iter
      ~f:(fun alias ->
        if not (Column.List.mem_alias cols ~alias) then
          failwith ("The column " ^ alias ^ " does not exist."))
      xs
  in
  List.iter
    ~f:(fun (left, right) ->
      check_col left;
      check_col right)
    fds;
  let fd_of (left, right) =
    Fun_dep.make (Alias.Set.of_list left) (Alias.Set.of_list right)
  in
  Fun_dep.Set.of_list (List.map ~f:fd_of fds)

let map_keys (delta : Sorted.t) =
  let open Phrase_value in
  let mx =
    Array.fold_right
      (fun p mx ->
        List.fold_right
          (fun v mx ->
            match v with
            | Serial (`NewKeyMapped key) -> if key > mx then key else mx
            | _ -> mx)
          p mx)
      (Sorted.plus_rows delta) 0
  in
  let next_key = mx + 1 |> ref in
  Sorted.map_values
    ~f:(fun v ->
      match v with
      | Serial `NewKey ->
          let key = !next_key in
          next_key := key + 1;
          Serial (`NewKeyMapped key)
      | _ -> v)
    delta

let lens_put ~db (lens : Value.t) (data : Phrase_value.t list) =
  let env = Int.Map.empty in
  let rec do_step_rec ~env lens delt =
    match lens with
    | Lens { table; sort } -> apply_delta ~table ~db ~sort ~env delt
    | _ -> lens_put_set_step ~db lens delt ~env do_step_rec
  in
  let delta = lens_get_delta ~db lens data |> map_keys in
  do_step_rec ~env lens delta |> ignore
