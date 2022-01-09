open Lens.Utility
open Lens.Utility.O

type col_gen_type = [ `Seq | `Constant of int | `RandTo of int | `Rand ]

module type DatabaseCommon_S = sig
  val print_table_query : string -> unit

  val print_and_execute : string -> unit

  val pp_print_quoted : string Format.fmt_fn
end

let database_common test_ctx db =
  (module struct
    let print_table_query s =
      if Options.display_table_query_opt test_ctx then Printf.printf "%s\n%!" s
      else ()

    let print_and_execute str =
      print_table_query str;
      let open Lens.Database in
      db.execute str

    let pp_print_quoted f v =
      let open Lens.Database in
      Format.pp_print_string f (db.quote_field v)
  end : DatabaseCommon_S)

let create_table test_ctx db table =
  (module struct
    include (val database_common test_ctx db)

    let lens_ref = ref None

    let set_lens l = lens_ref := Some l

    let lens () = Option.value_exn !lens_ref

    let row_columns data =
      let r = List.hd data in
      List.map ~f:fst (Lens.Phrase.Value.unbox_record r)

    let row_values _db data =
      List.map ~f:(Lens.Phrase.Value.unbox_record >> List.map ~f:snd) data

    let count () =
      let open Lens.Database in
      let query =
        Format.asprintf "SELECT COUNT(*) AS count FROM %a" pp_print_quoted table
      in
      let res =
        db.execute_select query ~field_types:[ ("count", Lens.Phrase.Type.Int) ]
      in
      List.hd res
      |> Lens.Phrase.Value.unbox_record
      |> List.hd
      |> snd
      |> Lens.Phrase.Value.unbox_int

    let insert data =
      let columns = row_columns data in
      let values = row_values db data in
      let returning = [] in
      let open Lens.Database.Insert in
      let insert =
        Format.asprintf "%a" (fmt ~db) { table; columns; values; returning }
      in
      print_and_execute insert

    let insert_ints data =
      match data with
      | [] -> ()
      | _ ->
          let pp_row f v =
            Format.fprintf f "(%a)" (Format.pp_comma_list Format.pp_print_int) v
          in
          let insert =
            Format.asprintf "INSERT INTO %a VALUES %a" pp_print_quoted table
              (Format.pp_comma_list pp_row)
              data
          in
          print_and_execute insert

    let drop () =
      print_and_execute (Format.asprintf "DROP TABLE %a" pp_print_quoted table)

    let drop_if_cleanup () =
      if Options.leave_tables_opt test_ctx then ()
      else
        print_and_execute
          (Format.asprintf "DROP TABLE %a" pp_print_quoted table)

    let drop_if_exists () =
      print_and_execute
        (Format.asprintf "DROP TABLE IF EXISTS %a" pp_print_quoted table)
  end : Table_S.S)

let create_db test_ctx db =
  (module struct
    include (val database_common test_ctx db)

    let db = db

    let table_reference table = create_table test_ctx db table

    let create ~table ~primary_key ~fields =
      let pp_field f v =
        Format.fprintf f "  %a INTEGER NOT NULL," pp_print_quoted v
      in
      let pk = "PK_" ^ table in
      let query =
        Format.asprintf
          {|CREATE TABLE %a (
%a
  CONSTRAINT %a PRIMARY KEY (%a)
)|}
          pp_print_quoted table
          (Format.pp_print_list pp_field)
          fields pp_print_quoted pk
          (Format.pp_comma_list pp_print_quoted)
          primary_key
      in
      print_and_execute query;
      table_reference table

    let drop_create ~table ~primary_key ~fields =
      let module M = (val table_reference table) in
      M.drop_if_exists ();
      create ~table ~primary_key ~fields

    let easy_lens ~table ~fd ~key ~cols =
      let colFn name =
        Lens.Column.make ~alias:name ~name ~table ~typ:Lens.Phrase.Type.Int
          ~present:true
      in
      let table = Lens.Database.Table.{ name = table; keys = [ key ] } in
      let fds = Lens.Fun_dep.Set.singleton fd in
      let cols = List.map ~f:colFn cols in
      let sort = Lens.Sort.make ~fds cols in
      let l1 = Lens.Value.Lens { table; sort } in
      l1

    let create_easy ~table fd =
      let fd = Fun_dep.of_string fd in
      let m =
        create ~table ~primary_key:(Fun_dep.left_list fd)
          ~fields:(Fun_dep.all_columns_list fd)
      in
      let module Table = (val m) in
      Table.set_lens
        (easy_lens ~table ~fd ~key:(Fun_dep.left_list fd)
           ~cols:(Fun_dep.all_columns_list fd));
      m

    let drop_create_easy ~table fd =
      let module M = (val table_reference table) in
      M.drop_if_exists ();
      create_easy ~table fd

    type col_gen_type = [ `Seq | `Constant of int | `RandTo of int | `Rand ]

    let generate_data ~n (cols : col_gen_type list) =
      let _ = Random.self_init () in
      let gen i = function
        | `Seq -> i
        | `Constant n -> n
        | `RandTo n -> 1 + Random.int (if n < 5 then 5 else n)
        | `Rand -> Random.bits ()
      in
      let data =
        List.map ~f:(fun i -> List.map ~f:(gen i) cols) (List.init n (( + ) 1))
      in
      data

    let drop_create_easy_populate ~table ~n ~fd generator =
      let m = drop_create_easy ~table fd in
      let module Table = (val m) in
      Table.insert_ints (generate_data ~n generator);
      m
  end : Database_S.S)

let get_db test_ctx =
  (* host port dbname user pw *)
  let conn, cstr =
    Links_postgresql.Pg_database.get_pg_database_by_string
      (Options.database_args_opt test_ctx)
  in
  Links_core.Lens_database_conv.lens_db_of_db cstr conn

let create test_ctx = create_db test_ctx (get_db test_ctx)
