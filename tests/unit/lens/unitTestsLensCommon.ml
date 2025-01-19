open OUnit2
open Links_postgresql.Pg_database
module Debug = Links_core.Debug
module Settings = Links_core.Settings
module Basicsettings = Links_core.Basicsettings
module Links_value = Links_core.Value
open Lens
open Lens.Utility
open Lens.Utility.O

include TestUtility.Options

module LensTestHelpers = struct
  let get_db test_ctx =
    (* host port dbname user pw *)
    let conn, cstr = get_pg_database_by_string (database_args_opt test_ctx) in
    Links_core.Lens_database_conv.lens_db_of_db cstr conn

  (** Only print when **)
  let fmt_std_v test_ctx (fn : Format.formatter -> unit) =
    if verbose_opt test_ctx then fn Format.std_formatter else ()

  let fmt_err_v test_ctx (fn : Format.formatter -> unit) =
    if verbose_opt test_ctx then fn Format.err_formatter else ()

  let print_verbose test_ctx message =
    if verbose_opt test_ctx then Printf.printf "%s\n%!" message else ()

  let print_table_query test_ctx message =
    if display_table_query_opt test_ctx then Printf.printf "%s\n%!" message

  let colslist_of_string str =
    let cols = String.split_on_char ' ' str in
    let cols = List.filter (fun s -> String.length s <> 0) cols in
    cols

  let rec_constr_int (cols : string) (vals : int list) =
    let cols = colslist_of_string cols in
    Links_value.box_record
      (List.map2 (fun c v -> (c, Links_value.box_int v)) cols vals)

  let delt_constr_int (cols : string) ((vals, m) : int list * int) =
    (rec_constr_int cols vals, m)

  let colset_of_string str =
    let cols = colslist_of_string str in
    Lens.Alias.Set.of_list cols

  let fundep_of_string str =
    let split = Str.split (Str.regexp "->") str in
    let _ = assert_equal (List.length split) 2 in
    let colsets = List.map ~f:colset_of_string split in
    Lens.Fun_dep.make (List.nth colsets 0) (List.nth colsets 1)

  let fundepset_of_string str =
    let split = Str.split (Str.regexp ";") str in
    let fds = List.map ~f:fundep_of_string split in
    Lens.Fun_dep.Set.of_list fds

  let mem_lens fds name records =
    let cols =
      Lens.Fun_dep.Set.fold
        (fun fd fld ->
          Lens.Alias.Set.union_all
            [ Lens.Fun_dep.left fd; Lens.Fun_dep.right fd; fld ])
        fds Lens.Alias.Set.empty
    in
    let cols = Lens.Alias.Set.elements cols in
    let colFn table name =
      Column.make ~table ~name ~alias:name ~typ:Phrase.Type.Int ~present:true
    in
    let sort = Sort.make ~fds (List.map ~f:(colFn name) cols) in
    Value.LensMem { records; sort }

  let mem_lens_str fds name data = mem_lens (fundepset_of_string fds) name data

  let join_lens_dl left right on =
    let sort, on =
      Lens.Sort.join_lens_sort (Lens.Value.sort left) (Lens.Value.sort right)
        ~on
      |> Result.ok_exn
    in
    let del_left = Lens.Phrase.Constant.bool true in
    let del_right = Lens.Phrase.Constant.bool false in
    Value.LensJoin { left; right; on; del_left; del_right; sort }

  let join_lens_dr left right on =
    let sort, on =
      Lens.Sort.join_lens_sort (Lens.Value.sort left) (Lens.Value.sort right)
        ~on
      |> Result.ok_exn
    in
    let del_left = Lens.Phrase.Constant.bool false in
    let del_right = Lens.Phrase.Constant.bool true in
    Value.LensJoin { left; right; on; del_left; del_right; sort }

  let select_lens lens predicate =
    let sort = Lens.Value.sort lens in
    let sort = Lens.Sort.select_lens_sort sort ~predicate |> Result.ok_exn in
    Lens.Value.LensSelect { lens; predicate; sort }

  let drop_lens lens drop key default =
    let key' = Alias.Set.singleton key in
    let sort = Value.sort lens in
    let sort =
      Sort.drop_lens_sort sort ~drop:[ drop ] ~default:[ default ] ~key:key'
      |> Result.ok_exn
    in
    Value.LensDrop { lens; drop; key; default; sort }

  let select_query l predicate =
    let predicate = Some predicate in
    Lens.Value.lens_get_select_opt l ~predicate

  let create_table test_ctx db table (primary_key : string list)
      (fields : string list) =
    let open (val TestUtility.Table.create_db test_ctx db) in
    create ~table ~primary_key ~fields

  let create_table_easy test_ctx db tablename str =
    let fd = fundep_of_string str in
    let left = Lens.Fun_dep.left fd in
    let right = Lens.Fun_dep.right fd in
    let cols = Lens.Alias.Set.union left right in
    create_table test_ctx db tablename
      (Lens.Alias.Set.elements left)
      (Lens.Alias.Set.elements cols)
    |> ignore

  let value_as_string db = function
    | `String s -> "\'" ^ db#escape_string s ^ "\'"
    | v -> Links_value.string_of_value v

  let row_columns data =
    let r = List.hd data in
    List.map ~f:fst (Phrase.Value.unbox_record r)

  let row_values _db data =
    List.map ~f:(Phrase.Value.unbox_record >> List.map ~f:snd) data

  let box_int_record_list cols data =
    List.map ~f:(List.map ~f:Phrase.Value.box_int) data
    |> List.map ~f:(List.zip_exn cols)
    |> List.map ~f:Phrase.Value.box_record

  let insert_rows db table data =
    (* See lib.ml "InsertRows" *)
    let columns = row_columns data in
    let values = row_values db data in
    let returning = [] in
    let open Database.Insert in
    let insert =
      Format.asprintf "%a" (fmt ~db) { table; columns; values; returning }
    in
    let open Database in
    db.execute insert

  let drop_if_exists test_ctx (db : Database.t) table =
    let open Database in
    let query = "DROP TABLE IF EXISTS " ^ db.quote_field table in
    print_table_query test_ctx query;
    db.execute query

  let drop_if_cleanup test_ctx (db : Database.t) table =
    if leave_tables_opt test_ctx |> not then drop_if_exists test_ctx db table

  type col_gen_type = [ `Seq | `Constant of int | `RandTo of int | `Rand ]

  (* is there a more standardized function for this? *)
  let rec range a b = if a > b then [] else a :: range (a + 1) b

  let gen_data (cols : col_gen_type list) cnt =
    let _ = Random.self_init () in
    let data =
      List.map
        ~f:(fun i ->
          List.map
            ~f:(function
              | `Seq -> i
              | `Constant n -> n
              | `RandTo n -> 1 + Random.int (if n < 5 then 5 else n)
              | `Rand -> Random.bits ())
            cols)
        (range 1 cnt)
    in
    data

  let create_record_type (cols : (string * Links_core.Types.typ) list) =
    let cols = List.map ~f:(fun (a, b) -> (a, `Present b)) cols in
    `Record
      ( Links_core.Utility.StringMap.from_alist cols,
        Links_core.Unionfind.fresh `Closed,
        false )

  let create_lens_db tablename fd (key : string list) (cols : string list) =
    let colFn table name =
      Column.make ~alias:name ~name ~table ~typ:Phrase.Type.Int ~present:true
    in
    (* table is `Table of (database * db : str) * tablename : str * keys : string list list * row type *)
    let table = Database.Table.{ name = tablename; keys = [ key ] } in
    let fds = Lens.Fun_dep.Set.singleton fd in
    let cols = List.map ~f:(colFn tablename) cols in
    let sort = Lens.Sort.make ~fds cols in
    let l1 = Value.Lens { table; sort } in
    l1

  let drop_create_populate_table test_ctx (db : Database.t) table str str2
      colGen cnt =
    let fd = fundep_of_string str in
    let left = Lens.Fun_dep.left fd in
    let cols = colslist_of_string str2 in
    drop_if_exists test_ctx db table |> ignore;
    create_table test_ctx db table (Lens.Alias.Set.elements left) cols |> ignore;
    let data = gen_data colGen cnt in
    if List.length data > 0 then
      let data = box_int_record_list cols data in
      let _ = insert_rows db table data in
      ()
    else ();
    let lens = create_lens_db table fd (Lens.Alias.Set.elements left) cols in
    lens

  let print_query_time () =
    let qtime = Lens.Statistics.get_query_time () in
    let qcount = Lens.Statistics.get_query_count () in
    Format.printf "Query Time: %d / %d queries" qtime qcount

  let time_query _in_mem fn =
    Lens.Statistics.reset ();
    let res =
      Lens.Statistics.debug_time_out fn (fun time ->
          print_endline ("Total Time: " ^ string_of_int (int_of_float time)))
    in
    print_query_time ();
    res

  let time_op fn =
    Lens.Statistics.reset ();
    let ttime = ref 0.0 in
    let _ = Lens.Statistics.debug_time_out fn (fun time -> ttime := time) in
    (Lens.Statistics.get_query_time (), int_of_float !ttime)

  let time_query_both fn =
    let res = time_query false fn in
    let _ = time_query true fn in
    res

  let col_list_to_string (cols : string list) (sep : string) =
    List.fold_left (fun a b -> a ^ sep ^ b) (List.hd cols) (List.tl cols)

  let assert_rec_list_eq (actual : Phrase.Value.t list)
      (expected : Phrase.Value.t list) =
    if actual = [] || expected = [] then
      (* cannot construct sorted records without columns, but if one is empty so should the other *)
      assert (actual = expected)
    else
      let actual = Lens.Sorted_records.construct ~records:actual in
      let expected = Lens.Sorted_records.construct ~records:expected in
      assert (actual = expected)

  let assert_ok v =
    match v with
    | Result.Ok v -> v
    | _ -> assert_failure "Expected an ok result type."

  let assert_error v =
    match v with
    | Result.Error v -> v
    | _ -> assert_failure "Expected an error result type."
end

let test_fundep_of_string _test_ctx =
  let fds = LensTestHelpers.fundepset_of_string "A B -> C; C -> D; D -> E F" in
  assert_equal
    (Format.asprintf "%a" Fun_dep.Set.pp_pretty fds)
    "A B -> C; C -> D; D -> E F"

let suite =
  "lens_common_helpers" >::: [ "fundep_of_string" >:: test_fundep_of_string ]
